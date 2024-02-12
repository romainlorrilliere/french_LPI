


#### calcul du LPI oiseaux river

setwd("c:/git/french_LPI/script/")
source("../functions/import_table_in2_posgresql.r")
source("../../abundance_variation_trend/functions/fun_glmmTMB_for_variation.r")


library(data.table)

library(ggplot2)

library(glmmTMB)

library(arm)
library(ggeffects)

con <- con_lpi()




q <- "
SELECT * FROM stoc_eps_fw.obs_point_river_abs
;"


dobs <- send_query(con, q)

dobs

dobs[,log_effort := log(10 * qualite_inventaire_stoc)]

dobs[,sum_point := sum(obs),by = .(code_sp,point)]

head(dobs)

dobs[,year_txt := as.character(year)]

dobs[,departement := as.numeric(as.character(departement))]



fwrite(dobs,"data/obs_bird_river.csv")


dsp_y <- dobs[obs > 0,.(nb_occ_y = .N),by = .(french_name, code_sp,specialisation,year)]
dsp_y[,totsp := sum(nb_occ_y),by = .(french_name,code_sp,specialisation)]
setorder(dsp_y,-totsp,-year)

dsp_nb_site_y <- dobs[,.(nb_site = .N),by = .(code_sp,year)]
dsp_y <- merge(dsp_y, dsp_nb_site_y)
dsp_y[,prop_occ := nb_occ_y / nb_site]

dsp <- dsp_y[,.(nb_occ = sum(nb_occ_y),median_occ = median(nb_occ_y)),by = .(french_name,code_sp,specialisation)]
setorder(dsp,-median_occ)
dsp

for(i in 1:nrow(dsp)) {

    sp <- dsp[i,code_sp]
    cat("\n",i," ",sp)


  #  md_nb <- glmmTMB(obs~year_txt + (1|departement/carre/point),offset = log_effort, data = dobs[code_sp == sp,],family = "nbinom2")
   # summary(md)

    md_pois <- glmmTMB(obs~year_txt + (1|departement/carre/point) + (1|info_passage_an), data = dobs[code_sp == sp,],family = "poisson")

    smd1 <- summary(md_pois)
    est1 <- as.data.frame(smd1$coefficients$cond)
    est1 <- est1[c(1,grep("year",rownames(est1))),]
    colnames(est1) <- c("estimate","sd","z","p_val")
    est1$year <- 2001:2023
    setDT(est1)
    est1[,z:=NULL]
    est1[1,`:=`(estimate = 0, sd = 0, p_val = 1)]
    est1[,`:=`(code = sp)]

    pred <- as.data.frame(ggpredict(md_pois,terms = c("year_txt"),allow.new.levels=TRUE))
    setDT(pred)
    pred[,x := as.numeric(as.character(x))]
    setnames(pred,"x","year")
    pred[,`:=`(code = sp)]
    setorder(pred,year)

    pred[,init := pred[year == 2001,predicted]]
    pred[,`:=`(predicted = predicted / init,conf.low = conf.low/init, conf.high = conf.high/init) ]

    ggfile <- paste0("output/bird_river_2023-11-30/glmmTMB_pois_",sp,".png")

    nom_sp <- dsp[i,french_name]
    tot <- dsp[i,nb_occ]
    med <- dsp[i,median_occ]

    title <- paste0(nom_sp," (",sp,")")
    sub <- paste0("médiane occurence:",med)

    gg <- ggplot(pred,aes(x = year,y= predicted,ymin = conf.low, ymax = conf.high))
    gg <- gg + geom_ribbon(alpha = 0.2, colour = NA) + geom_point(size = 0.8) + geom_line(size = 1.5)
    gg <- gg + labs(x="",y="",title = title,subtitle=sub)
gg
    ggsave(ggfile,gg)

    if(i == 1) {
        pred_out <- pred
        est_out <- est1
    } else {
        pred_out <- rbind(pred_out,pred,fill=TRUE)
        est_out <- rbind(est_out,est1,fill=TRUE)

    }

fwrite(pred_out, "output/bird_river_2023-11-30/_pred_year_bird_river.csv")
fwrite(est_out, "output/bird_river_2023-11-30/_coef_bird_river.csv")

}


fwrite(pred_out, "output/bird_river_2023-11-30/_pred_year_bird_river.csv")
fwrite(est_out, "output/bird_river_2023-11-30/_coef_bird_river.csv")


est_out <- fread("output/bird_river_2023-11-30/_coef_bird_river.csv")
est_out2 <- est_out[! code %in% c("PHYTRO","MOTFLA"),]

nb_rep <- 500

sim <- rnorm(nrow(est_out2) * nb_rep,rep(as.vector(est_out2[,estimate]),each = nb_rep),rep(as.vector(est_out2[,sd]),each = nb_rep))
sim <- data.frame(code = rep(est_out2[,code],each = nb_rep),year = rep(est_out2[,year],each = nb_rep),id_sim = rep(1:nb_rep,nrow(est_out2)),sim)
setDT(sim)
sim[,pred := exp(sim)]

setnames(sim,"code","code_sp")

sim <- merge(sim, dsp, by = "code_sp")



fwrite(sim, "output/bird_river_2023-11-30/_sim_bird_river_2001_2023.csv")
























coef_year[,tval:=NULL]

coef_year[,var_year := exp(coef)]
coef_year[,erreur_standard := sd * var_year]

col_year <- c("code_sp","year","var_year","erreur_standard","p_val","coef","sd")
coef_year <- coef_year[,col_year, with=FALSE]


coef_2002 <- data.frame( code_sp = dsp[,code_sp],year=2002,var_year = 1,erreur_standard=0, p_val = 1,coef = 0, sd = 0)

coef_year <- rbind(coef_year,coef_2002)
setorder(coef_year,code_sp,year)




coef_sim[,var_year := exp(coef)]
col_sim <- c("code_sp","id_sim","year","var_year","coef")
coef_sim <- coef_sim[,col_sim, with=FALSE]

sim_2002 <- expand.grid(code_sp = dsp[,code_sp],id_sim = 1:100)
setDT(sim_2002)
sim_2002[,`:=`(year = 2002,var_year = 1,coef = 0)]
sim_2002 <- sim_2002[,col_sim,with=FALSE]

coef_sim <- rbind(coef_sim,sim_2002)
setorder(coef_sim,code_sp,id_sim,year)


IC <- coef_sim[,.(ICinf = quantile(var_year,0.025),ICsup = quantile(var_year,0.975)),by = .(code_sp,year)]

coef_year <- merge(coef_year,IC,by = c("code_sp","year"))

fwrite(coef_year, "output/coef_year_bird_river.csv")
fwrite(coef_sim, "output/coef_sim_bird_river.csv")





## qq chiffre

dobs[,id_pop := paste0(point,"_",code_sp)]

dpop <- dobs[,(nb=.N),by = .(id_pop,specialisation)]

dpop <- dpop[,(nb_pop = .N),by = specialisation]
dpop
