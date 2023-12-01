
library(data.table)
library(sf)
library(ggplot2)
library(leaflet)
library(tmap)
library(tmaptools)
library(scales)
library(lubridate)
library(glmmTMB)
library(ggeffects)
source("../functions/import_table_in2_posgresql.r")



con <- con_lpi()


query <- "
SELECT id_site, season, protocol_type, o.opcod, date,year,surface,nb_sp,nb_ind,bassin_name, code, common_name,latin_name,length,biomass, nind, trend_dortel_2023, alien_dortel_2023
FROM sample as s, observation AS o, species AS sp
WHERE s.opcod = o.opcod AND o.species = sp.code AND  s.valid = TRUE ;
"
d <- send_query(con,query)

print(head(d))

dim(d)

query <- "
SELECT id_site, bassin_name ,opcod, year, date,season,protocol_type
FROM sample as s
WHERE s.valid = TRUE ;
"
dsample <- send_query(con,query)



vecsp <- unique(d[,code])
for (sp in vecsp) {

  dsp <- d[code == sp]

  dsamplesp <- dsample[id_site %in% unique(dsp[,id_site])]
  dsp_abs <- dsamplesp[!(opcod %in% dsp[,opcod])]
  dsp_abs[,`:=`(code = sp,biomass = 0,nind=0)]

  dsp <- dsp[,colnames(dsp_abs),with = FALSE]
  dsp <- rbind(dsp,dsp_abs)

  if(sp == vecsp[1]) dd <- dsp else dd <- rbind(dd,dsp)

}

dd[,year_txt := as.character(year)]
setnames(dd,"nind","abundance")


query <- "
SELECT code, common_name,latin_name, trend_dortel_2023, alien_dortel_2023
FROM species;
"
dsp <- send_query(con,query)
dsp <- dsp[code %in% vecsp]



print(dsp)

vecbassin <- unique(dd[,bassin_name])
vecseason <- unique(dd[,season])
vecvar <- c("abundance","biomass")
vecfam <- c("nbinom2","gaussian")
out_init <- FALSE

for(isp in 1:length(vecsp)) {
    sp <- vecsp[isp]
    cat("\n\n (",isp,"/",length(vecsp),") ",sp)
    for(b in vecbassin) {
        for(s in vecseason) {
            for(i in 1:2) {
                ##  sp <- vecsp[1]
                ##  b = vecbassin[1]
                ##  s <- vecseason[1]
                ## i <- 2
                v <- vecvar[i]
                f <- vecfam[i]

                form <- as.formula(paste0(v," ~ year_txt + protocol_type + (1|id_site)"))
                md <- try(glmmTMB(form, dd[code == sp & season == s & bassin_name == b],family = f))
                if(class(md)[1] != "try-error") {
                    ggmd <- as.data.frame(ggpredict(md)$year_txt)
                    setDT(ggmd)
                    setnames(ggmd,"x","year")
                    ggmd[,`:=`(code = sp,variable = v,season = s, bassin_name = b)]
                    if(!out_init) {
                        d_out <- ggmd
                        out_init <- TRUE
                    } else {
                        d_out <- rbind(d_out,ggmd,fill=TRUE)
                    }
                }
            }
        }
    }
}



for(isp in 1:length(vecsp)) {
    sp <- vecsp[isp]
    cat("\n\n (",isp,"/",length(vecsp),") ",sp)
        for(s in vecseason) {
            for(i in 1:2) {
                ##  sp <- vecsp[1]
                ##  b = vecbassin[1]
                ##  s <- vecseason[1]
                ## i <- 2
                v <- vecvar[i]
                f <- vecfam[i]

                form <- as.formula(paste0(v," ~ year_txt + protocol_type + (1|id_site)"))
                md <- try(glmmTMB(form, dd[code == sp & season == s],family = f))
                if(class(md)[1] != "try-error") {
                    ggmd <- as.data.frame(ggpredict(md)$year_txt)
                    setDT(ggmd)
                    setnames(ggmd,"x","year")
                    ggmd[,`:=`(code = sp,variable = v,season = s, bassin_name = "France")]
                    if(!out_init) {
                        d_out <- ggmd
                        out_init <- TRUE
                    } else {
                        d_out <- rbind(d_out,ggmd,fill=TRUE)
                    }
                }
            }
    }
}



d_out <- merge(d_out,dsp,by = "code")
print(head(d_out))

d_out[,ref := median(predicted),by = .(code,variable,season,bassin_name)]

d_out[,`:=`(predicted_sc = ifelse(variable == "abundance",predicted / ref,predicted - ref),
            conf.low_sc = ifelse(variable == "abundance",conf.low / ref,conf.low - ref),
            conf.high_sc = ifelse(variable == "abundance",conf.high / ref,conf.high - ref))]

d_out[,diff_conf := ifelse(variable == "abundance", conf.high_sc / conf.low_sc,conf.high_sc - conf.low_sc),]
d_out[,`:=`(q1 = quantile(diff_conf,.25,na.rm = TRUE), q3 = quantile(diff_conf,.75,na.rm = TRUE)),by = variable]

k <- 3

d_out[,tukey_fences_SUP := q3+k*(q3-q1)]

d_out[,valid_trend := ifelse(is.na(conf.low_sc)| is.na(conf.high_sc),FALSE,diff_conf <= tukey_fences_SUP)]

 dbWriteTable(con,"onema_ofb_fish_trends",value = d_out, overwrite = T, append = F, row.names = FALSE)

print(head(d_out))



######################

query <- "
SELECT code, common_name,latin_name, trend_dortel_2023, alien_dortel_2023
FROM species;
"
dsp <- send_query(con,query)
dsp <- dsp[code %in% vecsp]



print(dsp)

d_sample_sp <- dd[,.(nb_sample = .N, sum = sum(abundance)), by = .(code,bassin_name,season,year)]
d_occ_sp <- dd[abundance >0 ,.(occurence = .N), by = .(code,bassin_name,season,year)]
d_sample_sp <- merge(d_sample_sp, d_occ_sp, by =c("code","bassin_name","season","year"))
d_sample_sp[,`:=`(prop = occurence / nb_sample, mean_sum = sum/nb_sample)]
d_sample_sp[,`:=`(prop_mean = mean(prop), nb_sample_tot = sum(nb_sample)) , by=code]
d_sample_sp <- merge(d_sample_sp,dsp,by="code")

setorder(d_sample_sp,-nb_sample_tot)
head(d_sample_sp)


vecsp <- unique(d_sample_sp[,common_name])

gg_d_sp <- melt(data=d_sample_sp[,.(common_name,bassin_name,season,year,nb_sample,mean_sum,prop)],id.vars = c("common_name","bassin_name","season","year"))
setDT(gg_d_sp)

for(isp in 1:length(vecsp)) {
    sp <- vecsp[isp]
   cat("\n",isp,sp)

    code_sp <- dsp[common_name == sp, code]
  sc_sp <- dsp[common_name == sp,latin_name ]
  gg <- ggplot(data = gg_d_sp[common_name == sp,],aes(x=year,y=value,colour=season,group=season)) + geom_line() + geom_point(size=0.7) + facet_grid(variable~bassin_name,scales = "free_y")
  gg <- gg + labs(title = paste0(sp," (",sc_sp,", ",code_sp,")"))


  ggsave(paste0("output/descri_fish/",sprintf("%02d",isp),"_",code_sp,"_descri.png"),gg,width = 10,height = 8)

}

cat("\n")

library(cowplot)


vecsp_trend <- unique(d_out[valid_trend == TRUE,common_name])
for(isp in 1:length(vecsp_trend)) {
    sp <- vecsp[isp]
 cat("\n",isp,sp)
       code_sp <- dsp[common_name == sp, code]
  sc_sp <- dsp[common_name == sp,latin_name ]
    gg1 <- ggplot(data = d_out[common_name == sp & valid_trend == TRUE & variable == "abundance",],aes(x=year,y=predicted_sc,colour=season,group=season,fill=season)) +  geom_ribbon(aes(ymin = conf.low_sc, ymax = conf.high_sc), alpha = 0.2,colour=NA)  + geom_line() + geom_point(size=0.7) + facet_wrap(~bassin_name,scales = "free_y",nrow=1)
  gg1 <- gg1 + labs(title = paste0(sp," (",sc_sp,", ",code_sp,"): abundance"),y = "Abundace variation")
gg1


      gg2 <- ggplot(data = d_out[common_name == sp & valid_trend == TRUE & variable == "biomass",],aes(x=year,y=predicted_sc,colour=season,group=season,fill=season)) +  geom_ribbon(aes(ymin = conf.low_sc, ymax = conf.high_sc), alpha = 0.2,colour=NA)  + geom_line() + geom_point(size=0.7) + facet_wrap(~bassin_name,scales = "free_y",nrow=1)
  gg2 <- gg2 + labs(title = paste0(sp," (",sc_sp,", ",code_sp,"): biomass"),y = "Biomass variation")
gg2

    gg <- plot_grid(gg1,gg2,nrow=2)

    save_plot(paste0("output/trend_fish/",sprintf("%02d",isp),"_",code_sp,"_trenf.png"),gg,base_width = 10, base_height = 8)

}

cat("\n")



q <- "
CREATE INDEX s_id_site ON sample(id_site);
CREATE INDEX s_opcod ON sample(opcod);
CREATE INDEX sp_sp ON species(code);
CREATE INDEX o_opcod ON observation(opcod);
CREATE INDEX o_species ON observation(species);
"
d <- send_query(con,q)




query <- "
WITH
-- table des couples site-espece
	site_sp AS (
	SELECT DISTINCT id_site, species FROM observation o NATURAL JOIN sample s WHERE s.valid = TRUE),
-- table des inventaires
	sample_valid AS (
	SELECT DISTINCT opcod, id_site FROM sample s WHERE s.valid = TRUE),
-- table des espèces
	species_obs AS (
	SELECT DISTINCT species FROM observation o JOIN species sp ON o.species = sp.code),
-- produit cartesien inventaire espèces
	cross_sp_sample AS (
	SELECT * FROM species_obs spo CROSS JOIN sample_valid sv),
-- couple espece sample
	sp_sample AS (
	SELECT * FROM cross_sp_sample NATURAL JOIN site_sp),
-- abondance obs
	abondance AS (
	SELECT opcod, species, length, biomass, nind FROM observation o NATURAL JOIN sample s WHERE s.valid = TRUE),
-- add absence
	sp_abs AS (
	SELECT sps.opcod, sps.species, length, COALESCE(nind,0) AS abondance,  COALESCE(biomass,0) AS biomass FROM sp_sample sps LEFT JOIN abondance ab ON sps.opcod = ab.opcod AND sps.species = ab.species
	)
SELECT s.id_site, season, protocol_type, s.opcod, date,year,surface,nb_sp,nb_ind,bassin_name, code, common_name,latin_name,length,biomass, abondance, trend_dortel_2023, alien_dortel_2023
FROM sp_abs sps
NATURAL JOIN sample s
JOIN species sp ON sps.species = sp.code;
"
d <- send_query(con,query)


## nb data par inventaire et par espèce

dinvsp <- d[,.(nb = .N),by = .(opcod, code)]
summary(dinvsp)


d[,log_surface := log(surface)]
d[,year_txt := as.factor(year)]

md <- glmmTMB(abondance ~ year_txt + protocol_type + season +(1|bassin_name/id_site) ,offset=log_surface,family= nbinom2, data = d[code == "TAC",])

summary(md)

