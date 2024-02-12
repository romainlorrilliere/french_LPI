library(data.table)
library(ggplot2)
library(tmap)

## setwd("script/")

setwd("c:/git/french_LPI/script/")
source("../functions/import_table_in2_posgresql.r")


sim_b <- fread("output/bird_river_2023-11-30/_sim_bird_river_2001_2023.csv")
sim_b[,class := "bird"]
setnames(sim_b,"specialisation","group")
sim_b <- sim_b[code_sp != "LANCOL",.(class,code_sp,french_name,group,year,id_sim,pred)]


sim_f_95 <- fread("output/fish_river_2023-11-27/_sim_poisson_1995_2018.csv")
sim_f_01 <- fread("output/fish_river_2023-11-27/_sim_poisson_2001_2018.csv")

sim_f_95[,class := "fish"]
setnames(sim_f_95,c("code","common_name"),c("code_sp","french_name"))
sim_f_95 <- sim_f_95[,.(class,code_sp,french_name,group,year,id_sim,pred)]


sim_f_01[,class := "fish"]
setnames(sim_f_01,c("code","common_name"),c("code_sp","french_name"))
sim_f_01 <- sim_f_01[,.(class,code_sp,french_name,group,year,id_sim,pred)]


hh <- rbind(unique(sim_b[,.(class,french_name,group)]),(unique(sim_f_95[,.(class,french_name,group)])))
fname <- paste0("output/fig_river_2023-12-05/les_especes.csv")
fwrite(hh,fname)



#########################################

dinfo <- fread("output/fig_river_2023-12-05/sum_data.csv")


col_river <- "#2b8cbe"
col_trend1 <- "#e6550d"
col_trend2 <- "#fdae6b"


nb_rep <- 500


#########################################

## fig 1 : indices multi-espèces multi-taxons "biodiv de rivières", avec poissons (natifs + exotiques) + oiseaux d'eau : narratifs sur qualité des rivières depuis année 2000'
## 2001 -> 2018

## importer simulation poisson 2001 -> 2018
df <- sim_f_01
## importer simulation oiseaux 2001 -> 2018
db <- sim_b[year < 2019 & group == "Aquatique",]

d <- rbind(df,db)
d[,pred := as.numeric(as.character(pred))]

## aggrégation par moyenne géomtrique

agg_tot <- d[id_sim <= nb_rep,.(geom_mean = exp(mean(log(pred))), median = median(pred)),by = .(year,id_sim)]



## calcul des pentes

init <- TRUE



for(i in 1:nb_rep) {
    d_agg_i <- agg_tot[id_sim == i,]

    form <- paste0("geom_mean ~ year")
    md_lm <- lm(form,data = d_agg_i)

    est_lm_i <- as.data.frame(summary(md_lm)$coefficients)
    colnames(est_lm_i) <- c("estimate","sd","t","p_value")
    est_lm_i$var <- row.names(est_lm_i)
    setDT(est_lm_i)
    est_lm_i[,`:=`(t=NULL, id_sim = i)]

    if(init){
        est_lm <- est_lm_i
        init <- FALSE
    } else {

        est_lm <- rbind(est_lm,est_lm_i,fill=TRUE)
    }
}

## calul des pente moyenne et ICs

est_lm_wd <- dcast(est_lm[,.(id_sim, var,estimate)],id_sim~var)
colnames(est_lm_wd)[2] <- "inter"

year_central <- mean(unique(d[,year]))
setDT(est_lm_wd)
est_lm_wd[,center_val := year_central * year + inter]

est_lm_md <- est_lm_wd[,.(inter = median(inter), year = median(year), center_val = median(center_val))]
est_lm_md[,inter_center := center_val - year_central * year]


est_tot <- est_lm[,.(mean = median(estimate), ICinf = quantile(estimate,0.025), ICsup = quantile(estimate,0.975)),by=var]

nb_y <- max(d[,year])-min(d[,year])
est_tot[,`:=`(nb_year = nb_y,pourcent = round(mean * nb_y * 100,2),pourcent_inf = round(ICinf * nb_y * 100,2),pourcent_sup = round(ICsup * nb_y * 100,2))]


## calcul variation annuelle
agg_tot[,id_sim := NULL]
agg_tot <- melt(agg_tot,id.vars = "year")
setDT(agg_tot)
agg_tot <- agg_tot[,.(val = mean(value), ICinf = quantile(value,0.025), ICsup = quantile(value,0.975)),by = .(year,variable)]

est_gg <- merge(est_lm[var == "(Intercept)",.(inter = estimate, id_sim)],est_lm[var == "year",.(year = estimate, id_sim)],by="id_sim")


## figure en français

dinfo_gg <- dinfo[data=="sub" &  ((group == "all" & class == "poisson")| group == "Aquatique") ,]
dinfo_gg[,txt := paste0(class,": ",nb_pop," populations de ",nb_sp," espèces")]

ggfile <-  paste0("output/fig_river_2023-12-05/riviere_vivante.svg")
sub <- paste0(ifelse(est_tot[var=="year",pourcent] > 0,"+ ","- "),abs(est_tot[var=="year",pourcent]),"%  [",est_tot[var=="year",pourcent_inf],", ",est_tot[var=="year",pourcent_sup],"] sur ",est_tot[var=="year",nb_year]," ans")
nb_sp <- length(unique(d[,code_sp]))




gg <- ggplot(data = agg_tot[variable == "geom_mean",], aes(x = year, y = val)) #+ facet_grid(variable~.,scales = "free_y")
gg <- gg + geom_abline(data=est_gg,aes(intercept = inter,slope = year),alpha=0.08,colour = col_trend2,size=0.8)
gg <- gg + geom_abline(data = est_lm_md,aes(intercept = inter_center,slope = year),alpha=0.8,colour = col_trend1,size=1.2, linetype="dashed")
gg <- gg + geom_ribbon(aes(ymin = ICinf, ymax=ICsup),alpha = 0.2, colour= NA, fill= col_river)
gg <- gg + geom_point(size =2,colour = col_river) + geom_line(size = 1.2,colour = col_river)

##gg <- gg + geom_text(data = dinfo_gg[class == "poisson"],aes(label = txt), x = 2004, y = 1.55,size=4.5)
##gg <- gg + geom_text(data =  dinfo_gg[class == "oiseau"],aes(label = txt), x = 2004, y = 1.52,size=4.5)

gg <- gg + labs(title = "Indice rivière vivante", x= "Années",y=paste0("Variation d'abondance moyenne des espèces"), subtitle = sub)
gg <- gg + theme_bw() +
    theme(plot.caption = element_text(color="purple", face="bold", size = 14),
          axis.title = element_text(size = 14,face="bold"),
          axis.text = element_text(size = 14,face="bold"),legend.position="bottom")
print(gg)
ggsave(ggfile,gg)



## save data

fname <- paste0("output/fig_river_2023-12-05/riviere_vivante_trend.csv")
fwrite(est_tot,fname)

fname <- paste0("output/fig_river_2023-12-05/riviere_vivante_var_abond.csv")
fwrite(agg_tot,fname)


########################################

## fig 2 : indices multi-espèces multi-taxons "biodiv de rivères", avec poissons (natifs) + oiseaux d'eau : narratifs sur qualité des rivières & biodiversité "de chez nous", montrant un état écologique satisfaisant
## 2001 -> 2018



## importer simulation poisson 2001 -> 2018
df <- sim_f_01[group == "native",]
## importer simulation oiseaux 2001 -> 2018
db <- sim_b[year < 2019 & group == "Aquatique",]

d <- rbind(df,db)
d[,pred := as.numeric(as.character(pred))]

## aggrégation par moyenne géomtrique

agg_tot <- d[,.(geom_mean = exp(mean(log(pred))), median = median(pred)),by = .(year,id_sim)]



## calcul des pentes

init <- TRUE


for(i in 1:nb_rep) {
    d_agg_i <- agg_tot[id_sim == i,]

    form <- paste0("geom_mean ~ year")
    md_lm <- lm(form,data = d_agg_i)

    est_lm_i <- as.data.frame(summary(md_lm)$coefficients)
    colnames(est_lm_i) <- c("estimate","sd","t","p_value")
    est_lm_i$var <- row.names(est_lm_i)
    setDT(est_lm_i)
    est_lm_i[,`:=`(t=NULL, id_sim = i)]

    if(init){
        est_lm <- est_lm_i
        init <- FALSE
    } else {

        est_lm <- rbind(est_lm,est_lm_i,fill=TRUE)
    }
}

est_lm_wd <- dcast(est_lm[,.(id_sim, var,estimate)],id_sim~var)
colnames(est_lm_wd)[2] <- "inter"

year_central <- mean(unique(d[,year]))
setDT(est_lm_wd)
est_lm_wd[,center_val := year_central * year + inter]

est_lm_md <- est_lm_wd[,.(inter = median(inter), year = median(year), center_val = median(center_val))]
est_lm_md[,inter_center := center_val - year_central * year]


## calul des pente moyenne et ICs
est_tot <- est_lm[,.(mean = median(estimate), ICinf = quantile(estimate,0.025), ICsup = quantile(estimate,0.975)),by=var]

nb_y <- max(d[,year])-min(d[,year])

est_tot[,`:=`(nb_year = nb_y,pourcent = round(mean * nb_y * 100,2),pourcent_inf = round(ICinf * nb_y * 100,2),pourcent_sup = round(ICsup * nb_y * 100,2))]

est_gg <- merge(est_lm[var == "(Intercept)",.(inter = estimate, id_sim)],est_lm[var == "year",.(year = estimate, id_sim)],by="id_sim")


## calcul variation annuelle
agg_tot[,id_sim := NULL]
agg_tot <- melt(agg_tot,id.vars = "year")
setDT(agg_tot)
agg_tot <- agg_tot[,.(val = mean(value), ICinf = quantile(value,0.025), ICsup = quantile(value,0.975)),by = .(year,variable)]



## figure en français

ggfile <-  paste0("output/fig_river_2023-12-05/riviere_native.svg")

dinfo_gg <- dinfo[data=="sub" &  ((group == "native" & class == "poisson")| group == "Aquatique") ,]
dinfo_gg[,txt := paste0(class,": ",nb_pop," populations de ",nb_sp," espèces")]


sub <- paste0(ifelse(est_tot[var=="year",pourcent] > 0,"+ ","- "),abs(est_tot[var=="year",pourcent]),"%  [",est_tot[var=="year",pourcent_inf],", ",est_tot[var=="year",pourcent_sup],"] sur ",est_tot[var=="year",nb_year]," ans")
nb_sp <- length(unique(d[,code_sp]))



gg <- ggplot(data = agg_tot[variable == "geom_mean",], aes(x = year, y = val)) #+ facet_grid(variable~.,scales = "free_y")
gg <- gg + geom_abline(data=est_gg,aes(intercept = inter,slope = year),alpha=0.08,colour = col_trend2,size=0.8)
gg <- gg + geom_abline(data = est_lm_md,aes(intercept = inter_center,slope = year),alpha=0.8,colour = col_trend1,size=1.2, linetype="dashed")
gg <- gg + geom_ribbon(aes(ymin = ICinf, ymax=ICsup),alpha = 0.2, colour= NA, fill= col_river)
gg <- gg + geom_point(size =2,colour = col_river) + geom_line(size = 1.2,colour = col_river)

gg <- gg + geom_text(data = dinfo_gg[class == "poisson"],aes(label = txt), x = 2004, y = 1.4,size=4.5)
gg <- gg + geom_text(data =  dinfo_gg[class == "oiseau"],aes(label = txt), x = 2004, y = 1.38,size=4.5)


gg <- gg + labs(title = "Indice rivière native", x= "Années",y=paste0("Variation d'abondance moyenne des ",nb_sp," espèces"), subtitle = sub)
gg <- gg + theme_bw() +
    theme(plot.caption = element_text(color="purple", face="bold", size = 14),
          axis.title = element_text(size = 14,face="bold"),
          axis.text = element_text(size = 14,face="bold"),legend.position="bottom")

print(gg)
ggsave(ggfile,gg)



## save data

fname <- paste0("output/fig_river_2023-12-05/riviere_native_trend.csv")
fwrite(est_tot,fname)

fname <- paste0("output/fig_river_2023-12-05/riviere_native_var_abond.csv")
fwrite(agg_tot,fname)



#########################################

## fig 3 : indices multi-espèces "poissons", distinguant poissons natifs vs. poissons exotiques: narratifs sur les 2 catégories de poissons, qui se portent mieux même si plus marqué pour les exotiques (exclusion compétitive, expansion / colonisation, etc)
## 1998 -> 2018



## importer simulation poissons 1995 -> 2018
d <- sim_f_95[group != "",]

d[,pred := as.numeric(as.character(pred))]

## aggrégation par moyenne géomtrique

agg_tot <- d[,.(geom_mean = exp(mean(log(pred))), median = median(pred)),by = .(year,id_sim,group)]



## calcul des pentes

init <- TRUE
v_group <- unique(d[,group])

for(g in v_group) {
    for(i in 1:nb_rep) {
        d_agg_i <- agg_tot[id_sim == i & group == g,]

        form <- paste0("geom_mean ~ year")
        md_lm <- lm(form,data = d_agg_i)

        est_lm_i <- as.data.frame(summary(md_lm)$coefficients)
        colnames(est_lm_i) <- c("estimate","sd","t","p_value")
        est_lm_i$var <- row.names(est_lm_i)
        setDT(est_lm_i)
        est_lm_i[,`:=`(t=NULL, id_sim = i, group = g)]

        if(init){
            est_lm <- est_lm_i
            init <- FALSE
        } else {

            est_lm <- rbind(est_lm,est_lm_i,fill=TRUE)
        }
    }
}

est_lm_wd <- dcast(est_lm[,.(id_sim, var,group,estimate)],id_sim + group~var)
colnames(est_lm_wd)[3] <- "inter"

year_central <- mean(unique(d[,year]))
setDT(est_lm_wd)
est_lm_wd[,center_val := year_central * year + inter]

est_lm_md <- est_lm_wd[,.(inter = median(inter), year = median(year), center_val = median(center_val)),by=group]
est_lm_md[,inter_center := center_val - year_central * year]


## calul des pente moyenne et ICs

est_tot <- est_lm[var == "year",.(mean = mean(estimate), ICinf = quantile(estimate,0.025), ICsup = quantile(estimate,0.975)),by = .(group)]

nb_y <- max(d[,year])-min(d[,year])

est_tot[,`:=`(nb_year = nb_y,pourcent = round(mean * nb_y * 100,2),pourcent_inf = round(ICinf * nb_y * 100,2),pourcent_sup = round(ICsup * nb_y * 100,2))]





est_tot <- est_lm[,.(mean = median(estimate), ICinf = quantile(estimate,0.025), ICsup = quantile(estimate,0.975)),by=.(var,group)]

nb_y <- max(d[,year])-min(d[,year])


est_tot[,`:=`(nb_year = nb_y,pourcent = round(mean * nb_y * 100,2),pourcent_inf = round(ICinf * nb_y * 100,2),pourcent_sup = round(ICsup * nb_y * 100,2))]

est_gg <- merge(est_lm[var == "(Intercept)",.(inter = estimate, id_sim,group)],est_lm[var == "year",.(year = estimate, id_sim,group)],by=c("id_sim","group"))

est_tot_gg <- dcast(data=est_tot[,.(group,var,mean)],formula = group~var)
colnames(est_tot_gg) <- c("group","inter","year")


txt_slope <-est_tot[var == "year",]
txt_slope[,txt := paste0(ifelse(pourcent > 0,"+ ","- "),abs(pourcent),"% [",pourcent_inf,", ",pourcent_sup,"]\nsur ",nb_year," ans ")]

dinfo_gg <- dinfo[data=="all" &  class == "poisson" & group != "all" ,]
dinfo_gg[,txt := paste0(nb_pop," populations \n",nb_sp," espèces")]


## calcul variation annuelle
agg_tot[,id_sim := NULL]
agg_tot <- melt(agg_tot,id.vars = c("year","group"))
setDT(agg_tot)
agg_tot <- agg_tot[,.(val = mean(value), ICinf = quantile(value,0.025), ICsup = quantile(value,0.975)),by = .(year,variable,group)]


## figure en français

ggfile <-  paste0("output/fig_river_2023-12-05/riviere_poissons_vivante.svg")
# sub <- paste0(ifelse(est_tot[,pourcent] > 0,"+ ","- "),abs(est_tot[,pourcent]),"% sur ",est_tot[,nb_year]," ans [",est_tot[,pourcent_inf],", ",est_tot[,pourcent_sup],"]")
# nb_sp <- length(unique(d[,code_sp]))

gg <- ggplot(data = agg_tot[variable == "geom_mean",], aes(x = year, y = val)) + facet_grid(group~.)
gg <- gg + geom_abline(data=est_gg,aes(intercept = inter,slope = year),alpha=0.08,colour = col_trend2,size=0.8)
gg <- gg + geom_abline(data = est_lm_md,aes(intercept = inter_center,slope = year),alpha=0.8,colour = col_trend1,size=1.2, linetype="dashed")
gg <- gg + geom_ribbon(aes(ymin = ICinf, ymax=ICsup),alpha = 0.2, colour= NA, fill= col_river)
gg <- gg + geom_point(size =2,colour = col_river) + geom_line(size = 1.2,colour = col_river)
gg <- gg + geom_text(data = txt_slope,aes(label = txt), x = 2015, y = 2.25,size=4.5)
gg <- gg + geom_text(data = dinfo_gg,aes(label = txt), x = 1997, y = 2.25,size=4.5)
gg <- gg + labs(title = "Indice des poissons des rivières", x= "Années",y=paste0("Variation d'abondance moyenne des espèces"))
gg <- gg + theme_bw() +
    theme(plot.caption = element_text(color="purple", face="bold", size = 14),
          axis.title = element_text(size = 14,face="bold"),
          axis.text = element_text(size = 14,face="bold"),legend.position="bottom")

print(gg)
ggsave(ggfile,gg)



## save data

fname <- paste0("output/fig_river_2023-12-05/riviere_poissons_vivante_trend.csv")
fwrite(est_tot,fname)

fname <- paste0("output/fig_river_2023-12-05/riviere_poissons_vivante_var_abond.csv")
fwrite(agg_tot,fname)



#########################################

## fig 3bis : indices multi-espèces "poissons", distinguant poissons natifs vs. poissons exotiques: narratifs sur les 2 catégories de poissons, qui se portent mieux même si plus marqué pour les exotiques (exclusion compétitive, expansion / colonisation, etc)
## 1998 -> 2018



## importer simulation poissons 2001 -> 2018
d <- sim_f_01[group != "",]

d[,pred := as.numeric(as.character(pred))]

## aggrégation par moyenne géomtrique

agg_tot <- d[,.(geom_mean = exp(mean(log(pred))), median = median(pred)),by = .(year,id_sim,group)]



## calcul des pentes

init <- TRUE
v_group <- unique(d[,group])

for(g in v_group) {
    for(i in 1:nb_rep) {
        d_agg_i <- agg_tot[id_sim == i & group == g,]

        form <- paste0("geom_mean ~ year")
        md_lm <- lm(form,data = d_agg_i)

        est_lm_i <- as.data.frame(summary(md_lm)$coefficients)
        colnames(est_lm_i) <- c("estimate","sd","t","p_value")
        est_lm_i$var <- row.names(est_lm_i)
        setDT(est_lm_i)
        est_lm_i[,`:=`(t=NULL, id_sim = i, group = g)]

        if(init){
            est_lm <- est_lm_i
            init <- FALSE
        } else {

            est_lm <- rbind(est_lm,est_lm_i,fill=TRUE)
        }
    }
}

est_lm_wd <- dcast(est_lm[,.(id_sim, var,group,estimate)],id_sim + group~var)
colnames(est_lm_wd)[3] <- "inter"

year_central <- mean(unique(d[,year]))
setDT(est_lm_wd)
est_lm_wd[,center_val := year_central * year + inter]

est_lm_md <- est_lm_wd[,.(inter = median(inter), year = median(year), center_val = median(center_val)),by=group]
est_lm_md[,inter_center := center_val - year_central * year]

## calul des pente moyenne et ICs

est_tot <- est_lm[var == "year",.(mean = mean(estimate), ICinf = quantile(estimate,0.025), ICsup = quantile(estimate,0.975)),by = .(group)]

nb_y <- max(d[,year])-min(d[,year])

est_tot[,`:=`(nb_year = nb_y,pourcent = round(mean * nb_y * 100,2),pourcent_inf = round(ICinf * nb_y * 100,2),pourcent_sup = round(ICsup * nb_y * 100,2))]





est_tot <- est_lm[,.(mean = median(estimate), ICinf = quantile(estimate,0.025), ICsup = quantile(estimate,0.975)),by=.(var,group)]

nb_y <- max(d[,year])-min(d[,year])


est_tot[,`:=`(nb_year = nb_y,pourcent = round(mean * nb_y * 100,2),pourcent_inf = round(ICinf * nb_y * 100,2),pourcent_sup = round(ICsup * nb_y * 100,2))]

est_gg <- merge(est_lm[var == "(Intercept)",.(inter = estimate, id_sim,group)],est_lm[var == "year",.(year = estimate, id_sim,group)],by=c("id_sim","group"))

est_tot_gg <- dcast(data=est_tot[,.(group,var,mean)],formula = group~var)
colnames(est_tot_gg) <- c("group","inter","year")


txt_slope <-est_tot[var == "year",]
txt_slope[,txt := paste0(ifelse(pourcent > 0,"+ ","- "),abs(pourcent),"% [",pourcent_inf,", ",pourcent_sup,"]\nsur ",nb_year," ans ")]

dinfo_gg <- dinfo[data=="sub" &  class == "poisson" & group != "all" ,]
dinfo_gg[,txt := paste0(nb_pop," populations \n",nb_sp," espèces")]


## calcul variation annuelle
agg_tot[,id_sim := NULL]
agg_tot <- melt(agg_tot,id.vars = c("year","group"))
setDT(agg_tot)
agg_tot <- agg_tot[,.(val = mean(value), ICinf = quantile(value,0.025), ICsup = quantile(value,0.975)),by = .(year,variable,group)]


## figure en français

ggfile <-  paste0("output/fig_river_2023-12-05/riviere_poissons_sub_vivante.svg")
# sub <- paste0(ifelse(est_tot[,pourcent] > 0,"+ ","- "),abs(est_tot[,pourcent]),"% sur ",est_tot[,nb_year]," ans [",est_tot[,pourcent_inf],", ",est_tot[,pourcent_sup],"]")
# nb_sp <- length(unique(d[,code_sp]))

gg <- ggplot(data = agg_tot[variable == "geom_mean",], aes(x = year, y = val)) + facet_grid(group~.)
gg <- gg + geom_abline(data=est_gg,aes(intercept = inter,slope = year),alpha=0.08,colour = col_trend2,size=0.8)
gg <- gg + geom_abline(data = est_lm_md,aes(intercept = inter_center,slope = year),alpha=0.8,colour = col_trend1,size=1.2, linetype="dashed")
gg <- gg + geom_ribbon(aes(ymin = ICinf, ymax=ICsup),alpha = 0.2, colour= NA, fill= col_river)
gg <- gg + geom_point(size =2,colour = col_river) + geom_line(size = 1.2,colour = col_river)
gg <- gg + geom_text(data = txt_slope,aes(label = txt), x = 2015, y = 6,size=4.5)
gg <- gg + geom_text(data = dinfo_gg,aes(label = txt), x = 2003.5, y = 6,size=4.5)
gg <- gg + labs(title = "Indice des poissons des rivières", x= "Années",y=paste0("Variation d'abondance moyenne des espèces"))
gg <- gg + theme_bw() +
    theme(plot.caption = element_text(color="purple", face="bold", size = 14),
          axis.title = element_text(size = 14,face="bold"),
          axis.text = element_text(size = 14,face="bold"),legend.position="bottom")

print(gg)
ggsave(ggfile,gg)



## save data

fname <- paste0("output/fig_river_2023-12-05/riviere_poissons_sub_vivante_trend.csv")
fwrite(est_tot,fname)

fname <- paste0("output/fig_river_2023-12-05/riviere_poissons_sub_vivante_var_abond.csv")
fwrite(agg_tot,fname)




###########################################

## fig 4 : indices multi-espèces "oiseaux", distinguant oiseaux d'eau vs. agricole vs. forestier vs urbain vs...: narratifs sur oiseaux d'eau qui reste stable (donc qualité rivière plutot ok depuis années 2000), par rapport aux autres catégories, qui sont par ailleurs conformes aux tendances nationales (non montrées mais références aux plublications STOC).
## 1998 -> 2023



## importer simulation oiseaux 2001 -> 2023
d <- sim_b[group != "",]

d[,pred := as.numeric(as.character(pred))]

## aggrégation par moyenne géomtrique

agg_tot <- d[,.(geom_mean = exp(mean(log(pred))), median = median(pred)),by = .(year,id_sim,group)]



## calcul des pentes

init <- TRUE
v_group <- unique(d[,group])

for(g in v_group) {
    for(i in 1:nb_rep) {
        d_agg_i <- agg_tot[id_sim == i & group == g,]

        form <- paste0("geom_mean ~ year")
        md_lm <- lm(form,data = d_agg_i)

        est_lm_i <- as.data.frame(summary(md_lm)$coefficients)
        colnames(est_lm_i) <- c("estimate","sd","t","p_value")
        est_lm_i$var <- row.names(est_lm_i)
        setDT(est_lm_i)
        est_lm_i[,`:=`(t=NULL, id_sim = i, group = g)]

        if(init){
            est_lm <- est_lm_i
            init <- FALSE
        } else {

            est_lm <- rbind(est_lm,est_lm_i,fill=TRUE)
        }
    }
}

est_lm_wd <- dcast(est_lm[,.(id_sim, var,group,estimate)],id_sim + group~var)
colnames(est_lm_wd)[3] <- "inter"

year_central <- mean(unique(d[,year]))
setDT(est_lm_wd)
est_lm_wd[,center_val := year_central * year + inter]

est_lm_md <- est_lm_wd[,.(inter = median(inter), year = median(year), center_val = median(center_val)),by=group]
est_lm_md[,inter_center := center_val - year_central * year]

## calul des pente moyenne et ICs
est_tot <- est_lm[var == "year",.(mean = mean(estimate), ICinf = quantile(estimate,0.025), ICsup = quantile(estimate,0.975)),by = .(group)]

nb_y <- max(d[,year])-min(d[,year])

est_tot[,`:=`(nb_year = nb_y,pourcent = round(mean * nb_y * 100,2),pourcent_inf = round(ICinf * nb_y * 100,2),pourcent_sup = round(ICsup * nb_y * 100,2))]





est_tot <- est_lm[,.(mean = median(estimate), ICinf = quantile(estimate,0.025), ICsup = quantile(estimate,0.975)),by=.(var,group)]

nb_y <- max(d[,year])-min(d[,year])


est_tot[,`:=`(nb_year = nb_y,pourcent = round(mean * nb_y * 100,2),pourcent_inf = round(ICinf * nb_y * 100,2),pourcent_sup = round(ICsup * nb_y * 100,2))]

est_gg <- merge(est_lm[var == "(Intercept)",.(inter = estimate, id_sim,group)],est_lm[var == "year",.(year = estimate, id_sim,group)],by=c("id_sim","group"))

est_tot_gg <- dcast(data=est_tot[,.(group,var,mean)],formula = group~var)
colnames(est_tot_gg) <- c("group","inter","year")


txt_slope <-est_tot[var == "year",]
txt_slope[,txt := paste0(ifelse(pourcent > 0,"+ ","- "),abs(pourcent),"% [",pourcent_inf,", ",pourcent_sup,"]\nsur ",nb_year," ans ")]

dinfo_gg <- dinfo[data=="all" &  class == "oiseau" & group != "all" ,]
dinfo_gg[,txt := paste0(nb_pop," populations \n",nb_sp," espèces")]



## calcul variation annuelle
agg_tot[,id_sim := NULL]
agg_tot <- melt(agg_tot,id.vars = c("year","group"))
setDT(agg_tot)
agg_tot <- agg_tot[,.(val = mean(value), ICinf = quantile(value,0.025), ICsup = quantile(value,0.975)),by = .(year,variable,group)]


## figure en français

ggfile <-  paste0("output/fig_river_2023-12-05/riviere_oiseaux_vivante.svg")


gg <- ggplot(data = agg_tot[variable == "geom_mean",], aes(x = year, y = val)) + facet_wrap(group~.)

gg <- gg + geom_abline(data=est_gg,aes(intercept = inter,slope = year),alpha=0.08,colour = col_trend2,size=0.8)
gg <- gg + geom_abline(data = est_lm_md,aes(intercept = inter_center,slope = year),alpha=0.8,colour = col_trend1,size=1.2, linetype="dashed")
gg <- gg + geom_ribbon(aes(ymin = ICinf, ymax=ICsup),alpha = 0.2, colour= NA, fill= col_river)
gg <- gg + geom_point(size =2,colour = col_river) + geom_line(size = 1.2,colour = col_river)
gg <- gg + geom_text(data = txt_slope,aes(label = txt), x = 2019.5, y = 1.75,size=4.5)
gg <- gg + geom_text(data = dinfo_gg,aes(label = txt), x = 2003.5, y = 1.75,size=4.5)

gg <- gg + labs(title = "Indice des oiseaux des rivières", x= "Années",y=paste0("Variation d'abondance moyenne des espèces"))
gg <- gg + theme_bw() +
    theme(plot.caption = element_text(color="purple", face="bold", size = 14),
          axis.title = element_text(size = 14,face="bold"),
          axis.text = element_text(size = 14,face="bold"),legend.position="bottom")

print(gg)
ggsave(ggfile,gg,width = 16, height = 11)



## save data

fname <- paste0("output/fig_river_2023-12-05/riviere_oiseaux_vivante_trend.csv")
fwrite(est_tot,fname)

fname <- paste0("output/fig_river_2023-12-05/riviere_oiseaux_vivante_var_abond.csv")
fwrite(agg_tot,fname)








###########################################

## fig 4bis : indices multi-espèces "oiseaux", distinguant oiseaux d'eau vs. agricole vs. forestier vs urbain vs...: narratifs sur oiseaux d'eau qui reste stable (donc qualité rivière plutot ok depuis années 2000), par rapport aux autres catégories, qui sont par ailleurs conformes aux tendances nationales (non montrées mais références aux plublications STOC).
## 1998 -> 2023



## importer simulation oiseaux 2001 -> 2018
d <- sim_b[group != "" & year < 2019 ,]

d[,pred := as.numeric(as.character(pred))]

## aggrégation par moyenne géomtrique

agg_tot <- d[,.(geom_mean = exp(mean(log(pred))), median = median(pred)),by = .(year,id_sim,group)]



## calcul des pentes

init <- TRUE
v_group <- unique(d[,group])

for(g in v_group) {
    for(i in 1:nb_rep) {
        d_agg_i <- agg_tot[id_sim == i & group == g,]

        form <- paste0("geom_mean ~ year")
        md_lm <- lm(form,data = d_agg_i)

        est_lm_i <- as.data.frame(summary(md_lm)$coefficients)
        colnames(est_lm_i) <- c("estimate","sd","t","p_value")
        est_lm_i$var <- row.names(est_lm_i)
        setDT(est_lm_i)
        est_lm_i[,`:=`(t=NULL, id_sim = i, group = g)]

        if(init){
            est_lm <- est_lm_i
            init <- FALSE
        } else {

            est_lm <- rbind(est_lm,est_lm_i,fill=TRUE)
        }
    }
}

est_lm_wd <- dcast(est_lm[,.(id_sim, var,group,estimate)],id_sim + group~var)
colnames(est_lm_wd)[3] <- "inter"

year_central <- mean(unique(d[,year]))
setDT(est_lm_wd)
est_lm_wd[,center_val := year_central * year + inter]

est_lm_md <- est_lm_wd[,.(inter = median(inter), year = median(year), center_val = median(center_val)),by=group]
est_lm_md[,inter_center := center_val - year_central * year]

## calul des pente moyenne et ICs
est_tot <- est_lm[var == "year",.(mean = mean(estimate), ICinf = quantile(estimate,0.025), ICsup = quantile(estimate,0.975)),by = .(group)]

nb_y <- max(d[,year])-min(d[,year])

est_tot[,`:=`(nb_year = nb_y,pourcent = round(mean * nb_y * 100,2),pourcent_inf = round(ICinf * nb_y * 100,2),pourcent_sup = round(ICsup * nb_y * 100,2))]





est_tot <- est_lm[,.(mean = median(estimate), ICinf = quantile(estimate,0.025), ICsup = quantile(estimate,0.975)),by=.(var,group)]

nb_y <- max(d[,year])-min(d[,year])


est_tot[,`:=`(nb_year = nb_y,pourcent = round(mean * nb_y * 100,2),pourcent_inf = round(ICinf * nb_y * 100,2),pourcent_sup = round(ICsup * nb_y * 100,2))]

est_gg <- merge(est_lm[var == "(Intercept)",.(inter = estimate, id_sim,group)],est_lm[var == "year",.(year = estimate, id_sim,group)],by=c("id_sim","group"))

est_tot_gg <- dcast(data=est_tot[,.(group,var,mean)],formula = group~var)
colnames(est_tot_gg) <- c("group","inter","year")


txt_slope <-est_tot[var == "year",]
txt_slope[,txt := paste0(ifelse(pourcent > 0,"+ ","- "),abs(pourcent),"% [",pourcent_inf,", ",pourcent_sup,"]\nsur ",nb_year," ans ")]

dinfo_gg <- dinfo[data=="sub" &  class == "oiseau" & group != "all" ,]
dinfo_gg[,txt := paste0(nb_pop," populations \n",nb_sp," espèces")]



## calcul variation annuelle
agg_tot[,id_sim := NULL]
agg_tot <- melt(agg_tot,id.vars = c("year","group"))
setDT(agg_tot)
agg_tot <- agg_tot[,.(val = mean(value), ICinf = quantile(value,0.025), ICsup = quantile(value,0.975)),by = .(year,variable,group)]


## figure en français

ggfile <-  paste0("output/fig_river_2023-12-05/riviere_oiseaux_sub_vivante.svg")


gg <- ggplot(data = agg_tot[variable == "geom_mean",], aes(x = year, y = val)) + facet_wrap(group~.)

gg <- gg + geom_abline(data=est_gg,aes(intercept = inter,slope = year),alpha=0.08,colour = col_trend2,size=0.8)
gg <- gg + geom_abline(data = est_lm_md,aes(intercept = inter_center,slope = year),alpha=0.8,colour = col_trend1,size=1.2, linetype="dashed")
gg <- gg + geom_ribbon(aes(ymin = ICinf, ymax=ICsup),alpha = 0.2, colour= NA, fill= col_river)
gg <- gg + geom_point(size =2,colour = col_river) + geom_line(size = 1.2,colour = col_river)
gg <- gg + geom_text(data = txt_slope,aes(label = txt), x = 2015.5, y = 1.75,size=4.5)
gg <- gg + geom_text(data = dinfo_gg,aes(label = txt), x = 2003.5, y = 1.75,size=4.5)

gg <- gg + labs(title = "Indice des oiseaux des rivières", x= "Années",y=paste0("Variation d'abondance moyenne des espèces"))
gg <- gg + theme_bw() +
    theme(plot.caption = element_text(color="purple", face="bold", size = 14),
          axis.title = element_text(size = 14,face="bold"),
          axis.text = element_text(size = 14,face="bold"),legend.position="bottom")

print(gg)
ggsave(ggfile,gg,width = 16, height = 11)



## save data

fname <- paste0("output/fig_river_2023-12-05/riviere_oiseaux_sub_vivante_trend.csv")
fwrite(est_tot,fname)

fname <- paste0("output/fig_river_2023-12-05/riviere_oiseaux_sub_vivante_var_abond.csv")
fwrite(agg_tot,fname)



###############################




###############################
## carto



## data oiseaux




q <- "
SELECT * FROM stoc_eps_fw.obs_point_river_abs
;"


dobs <- send_query(con, q)

dobs

dpop <- dobs[,.(n=.N), by = .(point,code_sp, french_name,specialisation)]
nrow(unique(dpop[,.(point,code_sp)]))
dpop[,.N,by = specialisation]

dpop[specialisation == "Aquatique",.(nb_pop = .N),by = french_name]


dsite <- dobs[,.(n=.N), by = .(point,specialisation)]
length(unique(dsite[,point]))
dsite[,.N,by = specialisation]


dsp <- dobs[,.(n=.N), by = .(code_sp,french_name,specialisation)]
dsp[,.N]
dsp[,.N,by = specialisation]


## data poisson


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

d <- d[code %in% dsp_med[,code],]



## site oiseaux

con <- con_lpi()


q <- "
SELECT * FROM stoc_eps_fw.point_aqua;
"
stoc_sf <- st_read( con, query = q)

stoc_sf <- subset(stoc_sf, pk_point %in% dsite[,point])


stoc_sf$group <- "oiseaux"
stoc_sf <- stoc_sf[,c("group","pk_point")]
colnames(stoc_sf)[2] <- c("id_site")




## site_poisson

station_sf <- st_read(con,"site")
station_sf <- subset(station_sf,id %in% d[,id_site])

station_sf$group <- "poissons"
station_sf <- station_sf[,c("group","id")]
colnames(station_sf)[2] <- "id_site"

## sites

site_sf <- rbind(station_sf,stoc_sf)



## hydro

hydro_sf <- read_sf(con,"hydro")
bassin_sf <- read_sf(con,"main_bassin_hydro")

library(scales)
hydro_sf$lwd <- rescale((-1 * hydro_sf$classe), to = c(0.5,5))
hydro_sf_1 <- subset(hydro_sf,classe < 4)



## carto

myPalette <- get_brewer_pal("Blues", n = 3, contrast = c(0.92,0.57))

tmap_mode("view")
tm <-   tm_shape(bassin_sf) + tm_fill("bassin_name",alpha = 0.2)  +
    tm_shape(hydro_sf_1) + tm_lines(alpha = 0.5,col = "black",lwd = "lwd",scale = 0.8) +
   # tm_shape(hydro_sf_2) + tm_lines(col = "classe",palette = myPalette,lwd = "lwd",scale = 3) +
    tm_shape(stoc_sf) + tm_dots("red") + tm_shape(station_sf) + tm_dots("blue")

tm


tmap_save(tm, filename = "output/fig_river_2023-12-05/map.svg")



###############################################

### Toutes espèces

## fig 1 : indices multi-espèces multi-taxons "biodiv de rivières", avec poissons (natifs + exotiques) + oiseaux  : narratifs sur qualité des rivières depuis année 2000'
## 2001 -> 2018

## importer simulation poisson 2001 -> 2018
df <- sim_f_01
## importer simulation oiseaux 2001 -> 2018
db <- sim_b[year < 2019 & group != "",]

d <- rbind(df,db)
d[,pred := as.numeric(as.character(pred))]

## aggrégation par moyenne géomtrique

agg_tot <- d[id_sim <= nb_rep,.(geom_mean = exp(mean(log(pred))), median = median(pred)),by = .(year,id_sim)]



## calcul des pentes

init <- TRUE



for(i in 1:nb_rep) {
    d_agg_i <- agg_tot[id_sim == i,]

    form <- paste0("geom_mean ~ year")
    md_lm <- lm(form,data = d_agg_i)

    est_lm_i <- as.data.frame(summary(md_lm)$coefficients)
    colnames(est_lm_i) <- c("estimate","sd","t","p_value")
    est_lm_i$var <- row.names(est_lm_i)
    setDT(est_lm_i)
    est_lm_i[,`:=`(t=NULL, id_sim = i)]

    if(init){
        est_lm <- est_lm_i
        init <- FALSE
    } else {

        est_lm <- rbind(est_lm,est_lm_i,fill=TRUE)
    }
}

## calul des pente moyenne et ICs

est_lm_wd <- dcast(est_lm[,.(id_sim, var,estimate)],id_sim~var)
colnames(est_lm_wd)[2] <- "inter"

year_central <- mean(unique(d[,year]))
setDT(est_lm_wd)
est_lm_wd[,center_val := year_central * year + inter]

est_lm_md <- est_lm_wd[,.(inter = median(inter), year = median(year), center_val = median(center_val))]
est_lm_md[,inter_center := center_val - year_central * year]


est_tot <- est_lm[,.(mean = median(estimate), ICinf = quantile(estimate,0.025), ICsup = quantile(estimate,0.975)),by=var]

nb_y <- max(d[,year])-min(d[,year])
est_tot[,`:=`(nb_year = nb_y,pourcent = round(mean * nb_y * 100,2),pourcent_inf = round(ICinf * nb_y * 100,2),pourcent_sup = round(ICsup * nb_y * 100,2))]


## calcul variation annuelle
agg_tot[,id_sim := NULL]
agg_tot <- melt(agg_tot,id.vars = "year")
setDT(agg_tot)
agg_tot <- agg_tot[,.(val = mean(value), ICinf = quantile(value,0.025), ICsup = quantile(value,0.975)),by = .(year,variable)]

est_gg <- merge(est_lm[var == "(Intercept)",.(inter = estimate, id_sim)],est_lm[var == "year",.(year = estimate, id_sim)],by="id_sim")


## figure en français

dinfo_gg <- dinfo[data=="sub" &  ((group == "all" & class == "poisson")| group == "Aquatique") ,]
dinfo_gg[,txt := paste0(class,": ",nb_pop," populations de ",nb_sp," espèces")]

ggfile <-  paste0("output/fig_river_2023-12-05/riviere_vivante_all_birds.svg")
ggfile_png <-  paste0("output/fig_river_2023-12-05/riviere_vivante_all_birds.png")
sub <- paste0(ifelse(est_tot[var=="year",pourcent] > 0,"+ ","- "),abs(est_tot[var=="year",pourcent]),"%  [",est_tot[var=="year",pourcent_inf],", ",est_tot[var=="year",pourcent_sup],"] sur ",est_tot[var=="year",nb_year]," ans")
nb_sp <- length(unique(d[,code_sp]))



gg <- ggplot(data = agg_tot[variable == "geom_mean",], aes(x = year, y = val)) #+ facet_grid(variable~.,scales = "free_y")
gg <- gg + geom_abline(data=est_gg,aes(intercept = inter,slope = year),alpha=0.08,colour = col_trend2,size=0.8)
gg <- gg + geom_abline(data = est_lm_md,aes(intercept = inter_center,slope = year),alpha=0.8,colour = col_trend1,size=1.2, linetype="dashed")
gg <- gg + geom_ribbon(aes(ymin = ICinf, ymax=ICsup),alpha = 0.2, colour= NA, fill= col_river)
gg <- gg + geom_point(size =2,colour = col_river) + geom_line(size = 1.2,colour = col_river)

##gg <- gg + geom_text(data = dinfo_gg[class == "poisson"],aes(label = txt), x = 2004, y = 1.55,size=4.5)
##gg <- gg + geom_text(data =  dinfo_gg[class == "oiseau"],aes(label = txt), x = 2004, y = 1.52,size=4.5)

gg <- gg + labs(title = "Indice rivière vivante", x= "Années",y=paste0("Variation d'abondance moyenne des espèces"), subtitle = sub)
gg <- gg + theme_bw() +
    theme(plot.caption = element_text(color="purple", face="bold", size = 14),
          axis.title = element_text(size = 14,face="bold"),
          axis.text = element_text(size = 14,face="bold"),legend.position="bottom")
print(gg)
ggsave(ggfile,gg)
ggsave(ggfile_png,gg)



## save data

fname <- paste0("output/fig_river_2023-12-05/riviere_vivante_trend_all_birds.csv")
fwrite(est_tot,fname)

fname <- paste0("output/fig_river_2023-12-05/riviere_vivante_var_abond_all_birds.csv")
fwrite(agg_tot,fname)






###############################################

### grèbe

dobs <- fread("data/obs_bird_river.csv")

d <-  dobs[code_sp == "PODCRI",]


d[,year_txt := as.character(year_txt)]


md1 <-glmmTMB(obs~year_txt + (1|departement/carre/point) + (1|info_passage_an), data = d,family = "nbinom2")


md2 <- glmmTMB(obs~year + (1|departement/carre/point) + (1|info_passage_an), data = d,family = "nbinom2")






smd1 <- summary(md1)
est1 <- as.data.frame(smd1$coefficients$cond)
colnames(est1) <- c("estimate","sd","z","p_val")
est1$year <- sort(unique(d[,year]))
setDT(est1)
est1[,z:=NULL]
est1[1,`:=`(estimate = 1, sd = 0, p_val = 1)]
est1[,`:=`(code = sp,variable = v)]

pred1 <- as.data.frame(ggpredict(md1,terms = c("year_txt")))
setDT(pred1)
pred1[,group := "var"]


smd2 <- summary(md2)
est2 <- as.data.frame(smd2$coefficients$cond)[2,]

est_tot <- as.data.frame(confint(md2))[2,]
colnames(est_tot) <- c("ICinf","ICsup","mean")
setDT(est_tot)

nb_y <- max(d[,year])-min(d[,year])

est_tot[,`:=`(var = "year",nb_year = nb_y,pourcent = round(mean * nb_y * 100,2),pourcent_inf = round(ICinf * nb_y * 100,2),pourcent_sup = round(ICsup * nb_y * 100,2))]



pred2 <- as.data.frame(ggpredict(md2,terms = list(year = seq(2001,2023,0.1))))
setDT(pred2)
pred2[,group := "trend"]

pred <- rbind(pred1,pred2,fill=TRUE)

pred[,x := as.numeric(as.character(x))]

init <- pred[x == min(x) & group == "var",predicted]

setnames(pred,"x","year")
pred[,`:=`(predicted = predicted/init,conf.low = conf.low/init, conf.high = conf.high/init)]


ggfile_png <- paste0("output/bird_river_2023-11-30/glmmTMB_nbinom2_PODCRI_trend.png")
ggfile_svg <- paste0("output/bird_river_2023-11-30/glmmTMB_nbinom2_PODCRI_trend.svg")

ggpred <- pred_out[code == sp,]

title <- "Grèbe huppé"
sub <- paste0(ifelse(est_tot[var=="year",pourcent] > 0,"+ ","- "),abs(est_tot[var=="year",pourcent]),"%  [",est_tot[var=="year",pourcent_inf],", ",est_tot[var=="year",pourcent_sup],"] sur ",est_tot[var=="year",nb_year]," ans")




col_river <- "#2b8cbe"
col_trend1 <- "#e6550d"
col_trend2 <- "#fdae6b"
vec_col = c(col_river,col_trend1)
vec_fill = c(col_river,col_trend2)

gg <- ggplot(pred,aes(x = year,y= predicted, colour = group, ymin = conf.low, ymax = conf.high , fill = group , group = group))
gg <- gg + geom_ribbon(alpha = 0.2, colour = NA) + geom_point(size = 0.8) + geom_line(size = 1.5)
gg <- gg + labs(x="",y="",title = title, subtitle = sub)
gg <- gg + scale_colour_manual(values=vec_col) + scale_fill_manual(values = vec_fill)
 gg <- gg + theme_bw() +
    theme(plot.caption = element_text(color="purple", face="bold", size = 14),
          axis.title = element_text(size = 14,face="bold"),
          axis.text = element_text(size = 14,face="bold"),legend.position="none") + guides(fill="none")
print(gg)
ggsave(ggfile_png,gg)
ggsave(ggfile_svg,gg)

###############################################

### truite fario



library(sf)
library(ggplot2)
library(leaflet)
library(tmap)
library(tmaptools)
library(scales)
library(lubridate)
library(glmmTMB)
library(ggeffects)
library(data.table)
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


isp <- 1


sp <- vecsp[isp]
name <- dsp_med[code == sp,common_name]
med <- dsp_med[code == sp, occurence_med]

cat("\n\n (",isp,"/",1,") ",sp)

i <- 1
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

form2 <- as.formula(paste0(v," ~ year + protocol_type + (1 |bassin_name/id_site) + (1|season)"))
md2 <- try(glmmTMB(form2, d[code == sp],offset=value_offset,family = f))






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
pred1[,group := "var"]


smd2 <- summary(md2)
est2 <- as.data.frame(smd2$coefficients$cond)[2,]

est_tot <- as.data.frame(confint(md2))[2,]
colnames(est_tot) <- c("ICinf","ICsup","mean")
setDT(est_tot)

nb_y <- max(d[,year])-min(d[,year])

est_tot[,`:=`(var = "year",nb_year = nb_y,pourcent = round(mean * nb_y * 100,2),pourcent_inf = round(ICinf * nb_y * 100,2),pourcent_sup = round(ICsup * nb_y * 100,2))]



pred2 <- as.data.frame(ggpredict(md2,terms = list(year = seq(1995,2018,0.1))))
setDT(pred2)
pred2[,group := "trend"]

pred <- rbind(pred1,pred2,fill=TRUE)

pred[,x := as.numeric(as.character(x))]

init <- pred[x == min(x) & group == "var",predicted]

setnames(pred,"x","year")
pred[,`:=`(predicted = predicted/init,conf.low = conf.low/init, conf.high = conf.high/init)]


ggfile_png <- paste0("output/fish_river_2023-11-27/",sp,"_trend.png")
ggfile_svg <- paste0("output/fish_river_2023-11-27/",sp,"_trend.svg")
ggpred <- pred_out[code == sp,]
title <- paste0(name)
sub <- paste0(ifelse(est_tot[var=="year",pourcent] > 0,"+ ","- "),abs(est_tot[var=="year",pourcent]),"%  [",est_tot[var=="year",pourcent_inf],", ",est_tot[var=="year",pourcent_sup],"] sur ",est_tot[var=="year",nb_year]," ans")



col_river <- "#2b8cbe"
col_trend1 <- "#e6550d"
col_trend2 <- "#fdae6b"
vec_col = c(col_river,col_trend1)
vec_fill = c(col_river,col_trend2)

gg <- ggplot(pred,aes(x = year,y= predicted, colour = group, ymin = conf.low, ymax = conf.high , fill = group , group = group))
gg <- gg + geom_ribbon(alpha = 0.2, colour = NA) + geom_point(size = 0.8) + geom_line(size = 1.5)
gg <- gg + labs(x="",y="",title = title, subtitle = sub)
gg <- gg + scale_colour_manual(values=vec_col) + scale_fill_manual(values = vec_fill)
 gg <- gg + theme_bw() +
    theme(plot.caption = element_text(color="purple", face="bold", size = 14),
          axis.title = element_text(size = 14,face="bold"),
          axis.text = element_text(size = 14,face="bold"),legend.position="none") + guides(fill="none")
print(gg)
ggsave(ggfile_png,gg)
ggsave(ggfile_svg,gg)

