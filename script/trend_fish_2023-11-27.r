library(sf)
library(ggplot2)
library(leaflet)
library(tmap)
library(tmaptools)
library(scales)
library(lubridate)
library(glmmTMB)
library(ggeffects)
setwd("script/")
source("../functions/import_table_in2_posgresql.r")



con <- con_lpi()




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
SELECT s.id_site, section_class,season, protocol_type, s.opcod, date,year,surface,nb_sp,nb_ind,bassin_name, code, common_name,latin_name,length,biomass, abondance AS abundance, trend_dortel_2023, alien_dortel_2023
FROM sp_abs sps
NATURAL JOIN sample s NATURAL JOIN site_hydro
JOIN species sp ON sps.species = sp.code;
"
d <- send_query(con,query)
d[,log_surface := log(surface)]
d[,year_txt := as.factor(year)]

## nb data par inventaire et par espèce

## dinvsp <- d[,.(nb = .N),by = .(opcod, code)]
## summary(dinvsp)




## value_offset <- as.vector(unlist(d[code == "TAC","log_surface", with = FALSE]))
## md <- glmmTMB(abondance ~ year_txt + protocol_type + season +(1|bassin_name/id_site) ,offset=value_offset,family= "nbinom2", data = d[code == "TAC",])

## summary(md)








dsp <- d[,.(abundance_tot = sum(abundance), occurence_tot = sum(ifelse(abundance>0,1,0)), nb_sample_tot = .N),by = .(code,common_name, alien_dortel_2023,trend_dortel_2023 )]
dsp[,prop_occurence_tot := round(occurence_tot/nb_sample_tot,3)]
setorder(dsp,-occurence_tot,-abundance_tot)
print(dsp)

dspy <- d[,.(abundance_y = sum(abundance), occurence_y = sum(abundance>0,1,0), nb_sample_y = .N),by = .(code,year)]
setorder(dspy,code,year)
dspy[,prop_occurence_y := round(occurence_y/nb_sample_y,3)]
dspy[,occurence_med := median(occurence_y),by = code]

dspy <- merge(dspy,dsp,by = "code")
print(dspy)
setorder(dspy,-occurence_tot,abundance_tot,code,year)
print(dspy)


dsp_med <- dspy[,.(occurence_med = median(occurence_y)), by = code]
setorder(dsp_med,-occurence_med)
print(dsp_med)


dsp_med <- merge(dsp,dsp_med,by="code")
dsp_med <- dsp_med[occurence_med > 14,]


vecsp <- dsp[,code]
vecvar <- c("abundance","biomass")
vecfam <- c("nbinom2","gaussian")
vecoffset <- c("log_surface","surface")
out_init <- FALSE


library(parameters)
library(arm)

for(isp in 1:length(vecsp)) {
    ## isp <- 1
    sp <- vecsp[isp]
    name <- dsp_med[code == sp,common_name]
    med <- dsp_med[code == sp, occurence_med]

    cat("\n\n (",isp,"/",length(vecsp),") ",sp)

    for(i in 1:2) {

        ## sp <- vecsp[1]
        ## b = vecbassin[1]
        ## s <- vecseason[1]
        ## i <- 2

        v <- vecvar[i]
        f <- vecfam[i]
        o <- vecoffset[i]

        value_offset <- as.vector(unlist(d[code == sp,o, with = FALSE]))

        form1 <- as.formula(paste0(v," ~ year_txt + protocol_type + (1 |bassin_name/id_site) + (1|season)"))
        md1 <- try(glmmTMB(form1, d[code == sp],offset=value_offset,family = f))

        form2 <- as.formula(paste0(v," ~ year_txt * season + protocol_type + (1 |bassin_name/id_site)"))
        md2 <- try(glmmTMB(form2, d[code == sp],offset=value_offset,family = f))

        if(class(md1)[1] != "try-error" & class(md2)[1] != "try-error") {


            smd1 <- summary(md1)
            est1 <- as.data.frame(smd1$coefficients$cond)[1:24,]
            colnames(est1) <- c("estimate","sd","z","p_val")
            est1$year <- 1995:2018
            setDT(est1)
            est1[,z:=NULL]
            est1[1,`:=`(estimate = 1, sd = 0, p_val = 1)]
            est1[,`:=`(code = sp,variable = v)]


            pred1 <- as.data.frame(ggpredict(md1,terms = c("year_txt")))
            setDT(pred1)
            pred1[,group := "all"]

            pred2 <- as.data.frame(ggpredict(md2,terms = c("year_txt","season")))
            setDT(pred2)

            pred <- rbind(pred1,pred2,fill=TRUE)

            pred[,x := as.numeric(as.character(x))]

            d_init <- pred[x == min(x),.(predicted,group)]
            setnames(d_init,"predicted","init")
            pred <- merge(pred,d_init,by = "group")
            setnames(pred,"x","year")


            pred[,`:=`(code = sp,variable = v)]
            if(!out_init) {
                pred_out <- pred
                est_out <- est1
                out_init <- TRUE
            } else {
                pred_out <- rbind(pred_out,pred,fill=TRUE)
                est_out <- rbind(est_out,est1,fill=TRUE)
            }
        }
    }

    fwrite(pred_out,"output/fish_river_2023-11-27/_predict.csv")
    fwrite(est_out,"output/fish_river_2023-11-27/_estimate.csv")
}



pred_out[,`:=`(predicted = predicted / init,conf.low = conf.low/init, conf.high = conf.high/init) ]
fwrite(pred_out,"output/fish_river_2023-11-27/_predict.csv")

for(isp in 1:length(vecsp)) {
    ## isp <- 1
    sp <- vecsp[isp]
    name <- dsp_med[code == sp,common_name]
    med <- dsp_med[code == sp, occurence_med]

    cat("\n\n (",isp,"/",length(vecsp),") ",sp)


    ggfile <- paste0("output/fish_river_2023-11-27/",sp,".png")
    ggpred <- pred_out[code == sp,]
    title <- paste0(name," (",sp,")")
    sub <- paste0("médiane occurence:",med)
    gg <- ggplot(ggpred,aes(x = year,y= predicted, colour = group, ymin = conf.low, ymax = conf.high , fill = group , group = group)) + facet_grid(variable~.,scales= "free_y")
    gg <- gg + geom_ribbon(alpha = 0.2, colour = NA) + geom_point(size = 0.8) + geom_line(size = 1.5)
    gg <- gg + labs(x="",y="",title = title, subtitle = sub)
    ggsave(ggfile,gg)

}


est_out2 <- est_out[code %in% dsp_med[,code]& variable == "abundance"]

sp_exclude <- unique(est_out2[is.na(sd),code])
est_out2 <- est_out2[!(code %in% sp_exclude),]


sim <- rnorm(nrow(est_out2) * 100,rep(as.vector(est_out2[,estimate]),each = 100),rep(as.vector(est_out2[,sd]),each = 100))
sim <- data.frame(code = rep(est_out2[,code],each = 100),year = rep(est_out2[,year],each = 100),id_sim = rep(1:100,nrow(est_out2)),sim)
setDT(sim)
sim[,pred := exp(sim)]
sim[year == 1995, pred := 1]
sim

sim <- merge(sim, dsp_med, by = "code")
sim[alien_dortel_2023 == TRUE, group := "alien"]
sim[alien_dortel_2023 == FALSE, group := "native"]


agg_tot <- sim[,.(geom_mean = exp(mean(log(pred))), median = median(pred)),by = .(year,id_sim)]
agg_gr <- sim[!is.na(group),.(geom_mean = exp(mean(log(pred))), median = median(pred)),by = .(year,id_sim,group)]

## library(mgcv)
## d_gam_pred <- data.frame(year = seq(from = 1995, to = 2018, by = 1))

## for(i in 1:500) {
##
##     agg_tot_i <- agg_tot[id_sim == i,]
##     K <- nrow(agg_tot_i)
##     md_gam <- gam(geom_mean ~ s(year,k = K),data = agg_tot_i, method="REML")
##     pred_gam_i <- as.data.frame(predict.gam(md_gam,newdata = d_gam_pred,type = "terms",se=TRUE))
##     setDT(pred_gam_i)
##     pred_gam_i
##     plot(md_gam)
## }




agg_tot[,group := "all"]
agg_tot <- agg_tot[,colnames(agg_gr),with=FALSE]

agg_md <- rbind(agg_tot,agg_gr)

v_var <- c("geom_mean","median")
v_group <- c("all","native","alien")

init <- TRUE

for(g in v_group) {
    for(i in 1:100) {
        for(v in v_var){
            d_agg_i <- agg_md[id_sim == i & group == g,]

            form <- paste0(v," ~ year")
            md_lm <- lm(form,data = d_agg_i)

            est_lm_i <- as.data.frame(summary(md_lm)$coefficients)
            colnames(est_lm_i) <- c("estimate","sd","t","p_value")
            est_lm_i$var <- row.names(est_lm_i)
            setDT(est_lm_i)
            est_lm_i[,`:=`(t=NULL, id_sim = i,indice = v,group = g)]

            if(init){
                est_lm <- est_lm_i
                init <- FALSE
            } else {

                est_lm <- rbind(est_lm,est_lm_i,fill=TRUE)
            }
        }
    }
}



est_tot <- est_lm[var == "year",.(mean = mean(estimate), ICinf = quantile(estimate,0.025), ICsup = quantile(estimate,0.975)),by = .(group, indice)]

est_tot[,`:=`(pourcent = round(mean * 23 * 100,2),pourcent_inf = round(ICinf * 23 * 100,2),pourcent_sup = round(ICsup * 23 * 100,2))]


est_tot
fwrite(est_tot,"output/fish_river_2023-11-27/_estimates_gr.csv")


agg_tot[,id_sim := NULL]
agg_gr[,id_sim := NULL]

agg_tot <- melt(agg_tot,id.vars = "year")

agg_gr <- melt(agg_gr,id.vars = c("year","group"))

agg_tot <- agg_tot[,.(val = mean(value), ICinf = quantile(value,0.025), ICsup = quantile(value,0.975)),by = .(year,variable)]

agg_gr <- agg_gr[,.(val = mean(value), ICinf = quantile(value,0.025), ICsup = quantile(value,0.975)),by = .(year,group,variable)]





gg <- ggplot(data = agg_tot, aes(x = year, y = val, colour = variable, fill= variable, group = variable)) #+ facet_grid(variable~.,scales = "free_y")
gg <- gg + geom_ribbon(aes(ymin = ICinf, ymax=ICsup),alpha = 0.2, colour= NA)
gg <- gg + geom_point(size =1.5) + geom_line(size = 1.2)
gg <- gg + labs(title = "Poissons des rivières", x= "Années",y="")
print(gg)
ggfile <-  paste0("output/fish_river_2023-11-27/_tout_especes.png")
ggsave(ggfile,gg)

gg <- ggplot(data = agg_gr, aes(x = year, y = val, colour = variable, fill= variable, group = variable)) + facet_grid(group~.,scales = "free_y")
gg <- gg + geom_ribbon(aes(ymin = ICinf, ymax=ICsup),alpha = 0.2, colour= NA)
gg <- gg + geom_point(size =1.5) + geom_line(size = 1.2)
gg <- gg + labs(title = "Poissons des rivières", x= "Années",y="")
print(gg)
ggfile <-  paste0("output/fish_river_2023-11-27/_tout_especes_introduite_native.png")
ggsave(ggfile,gg)



for(isp in 1:length(vecsp)) {
    ## isp <- 1
    sp <- vecsp[isp]
    name <- dsp_med[code == sp,common_name]
    med <- dsp_med[code == sp, occurence_med]

    cat("\n\n (",isp,"/",length(vecsp),") ",sp)

    agg_tot_i <- agg_tot[id_sim == i,]
    K <- nrow(agg_tot_i)/3
      form1 <- as.formula(paste0(v," ~ year_txt + protocol_type + (1 |bassin_name/id_site) + (1|season)"))
        md1 <- try(glmmTMB(form1, d[code == sp],offset=value_offset,family = f))



    md_gam <- gam(abundance ~ s(year) + protocol_type + id_site + bassin_name + season + log_surface,data = d[code == sp], method="REML",family = nb())

    pred_gam_i <- as.data.frame(predict.gam(md_gam,newdata =  d[code == sp],type = "terms",se=TRUE))
    setDT(pred_gam_i)
    pred_gam_i


    plot(md_gam)

}

