

library(scales)
library(lubridate)
library(data.table)
source("../functions/import_table_in2_posgresql.r")







sim_b <- fread("output/bird_river_2023-11-30/_sim_bird_river_2001_2023.csv")
sim_b[,class := "bird"]
setnames(sim_b,"specialisation","group")
sim_b <- sim_b[code_sp != "LANCOL",.(class,code_sp,group,year,id_sim,pred)]


sim_f_95 <- fread("output/fish_river_2023-11-27/_sim_poisson_1995_2018.csv")
sim_f_01 <- fread("output/fish_river_2023-11-27/_sim_poisson_2001_2018.csv")

sim_f_95[,class := "fish"]
setnames(sim_f_95,"code","code_sp")
sim_f_95 <- sim_f_95[,.(class,code_sp,group,year,id_sim,pred)]


sim_f_01[,class := "fish"]
setnames(sim_f_01,"code","code_sp")
sim_f_01 <- sim_f_01[,.(class,code_sp,group,year,id_sim,pred)]




con <- con_lpi()



## poissons


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


d <- d[code %in% dsp_med[,code]]



## qq chiffre

d[,id_pop := paste0(id_site,"_",code)]
d[alien_dortel_2023 == TRUE, group := "alien"]
d[alien_dortel_2023 == FALSE, group := "native"]


## sp

dsp_all <- unique(d[code %in% sim_f_95[,code_sp],.(code,group)])[,.(nb_sp = .N),by = group]
dsp_all <- rbind(dsp_all,data.frame(group = "", nb_sp = sum(dsp_all[,nb_sp])))
dsp_all[,`:=`(class = "poisson",data = "all")]


dsp_sub <- unique(d[code %in% sim_f_01[,code_sp] & year > 2000,.(code,group)])[,.(nb_sp = .N),by = group]
dsp_sub <- rbind(dsp_sub,data.frame(group = "", nb_sp = sum(dsp_sub[,nb_sp])))
dsp_sub[,`:=`(class = "poisson",data = "sub")]

dsp <- rbind(dsp_all,dsp_sub)



## pop

dpop_all <- unique(d[code %in% sim_f_95[,code_sp],.(id_pop,group)])[,.(nb_pop = .N),by = group]
dpop_all <- rbind(dpop_all,data.frame(group = "", nb_pop = sum(dpop_all[,nb_pop])))
dpop_all[,`:=`(class = "poisson",data = "all")]


dpop_sub <- unique(d[code %in% sim_f_01[,code_sp] & year > 2000,.(id_pop,group)])[,.(nb_pop = .N),by = group]
dpop_sub <- rbind(dpop_sub,data.frame(group = "", nb_pop = sum(dpop_sub[,nb_pop])))
dpop_sub[,`:=`(class = "poisson",data = "sub")]

dpop <- rbind(dpop_all,dpop_sub)

dpoisson <- merge(dsp,dpop,by=c("group","class","data"))



#### oiseaux

q <- "
SELECT * FROM stoc_eps_fw.obs_point_river_abs
;"


d <- send_query(con, q)

d <- d[code_sp %in% sim_b[,code_sp],]
d[,id_pop := paste0(point,"_",code_sp)]
d[, group := specialisation]
d[specialisation == "", group := NA]

## sp

dsp_all <- unique(d[,.(code_sp,group)])[,.(nb_sp = .N),by = group]
dsp_all <- rbind(dsp_all,data.frame(group = "", nb_sp = sum(dsp_all[,nb_sp])))
dsp_all[,`:=`(class = "oiseau",data = "all")]


dsp_sub <- unique(d[year <2019,.(code_sp,group)])[,.(nb_sp = .N),by = group]
dsp_sub <- rbind(dsp_sub,data.frame(group = "", nb_sp = sum(dsp_sub[,nb_sp])))
dsp_sub[,`:=`(class = "oiseau",data = "sub")]

dsp <- rbind(dsp_all,dsp_sub)



## pop

dpop_all <- unique(d[,.(id_pop,group)])[,.(nb_pop = .N),by = group]
dpop_all <- rbind(dpop_all,data.frame(group = "", nb_pop = sum(dpop_all[,nb_pop])))
dpop_all[,`:=`(class = "oiseau",data = "all")]


dpop_sub <- unique(d[year <2019,.(id_pop,group)])[,.(nb_pop = .N),by = group]
dpop_sub <- rbind(dpop_sub,data.frame(group = "", nb_pop = sum(dpop_sub[,nb_pop])))
dpop_sub[,`:=`(class = "oiseau",data = "sub")]

dpop <- rbind(dpop_all,dpop_sub)

doiseau <- merge(dsp,dpop,by=c("group","class","data"))

d <- rbind(dpoisson,doiseau)

d <- d[!is.na(group),]
d[group == "", group := "all"]


fname <- paste0("output/fig_river_2023-12-05/sum_data.csv")
fwrite(d,fname)
