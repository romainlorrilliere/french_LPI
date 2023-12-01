
library(bit64)
library(data.table)
library(sf)
library(tmap)
library(lubridate)

setwd("c:/git/french_LPI/script/")
source("../functions/import_table_in2_posgresql.r")


con <- con_lpi()


d_faune <- fread("../data/Naiades_Export_France_Entiere_HB/fauneflore.csv")
colnames(d_faune)
setnames(d_faune, colnames(d_faune),f_change_colnames(colnames(d_faune)))
colnames(d_faune)
dbWriteTable(con,"miriades_faune_flore",value = d_faune, overwrite = T, append = F, row.names = FALSE)

d_res  <- fread("../data/Naiades_Export_France_Entiere_HB/resultat.csv")
d_cep <- fread("../data/Naiades_Export_France_Entiere_HB/cep.csv")

colnames(d_res)
setnames(d_res, colnames(d_res),f_change_colnames(colnames(d_res)))
colnames(d_res)

colnames(d_cep)
setnames(d_cep, colnames(d_cep),f_change_colnames(colnames(d_cep)))
colnames(d_cep)


dbWriteTable(con,"miriades_resultat",value = d_res, overwrite = T, append = F, row.names = FALSE)
dbWriteTable(con,"miriades_cep",value = d_cep, overwrite = T, append = F, row.names = FALSE)


d_op  <- fread("../data/Naiades_Export_France_Entiere_HB/operation.csv")
colnames(d_op)
setnames(d_op, colnames(d_op),f_change_colnames(colnames(d_op)))
colnames(d_op)
d_op[,date := as.Date(date_debut_operation_prel_bio)]
d_op[,`:=`(year = year(date),month = month(date))]

dbWriteTable(con,"miriades_sample",value = d_op, overwrite = T, append = F, row.names = FALSE)


# needed to clean up the station data file to remove the few ";" present in the text of certain columns.
d_station <- fread("../data/Naiades_Export_France_Entiere_HB/stations.csv")
colnames(d_station)
setnames(d_station, colnames(d_station),f_change_colnames(colnames(d_station)))
colnames(d_station)


d_op_station_y <- d_op[,.(nb_sample_y = .N),by = .(cd_station_mesure_eaux_surface, year)]
d_op_station_y <- d_op_station_y[,.(mean_sample_year = round(mean(nb_sample_y),2),
                                    sd_sample_year = round(sd(nb_sample_y),2),
                                    max_sample_year = max(nb_sample_y),
                                    first_year = min(year),
                                    last_year = max(year),
                                    nb_sample_y = .N,
                                    duration = max(year) - min(year) + 1),
                                 by = .(cd_station_mesure_eaux_surface)]

d_station <- merge(d_station,d_op_station_y,by="cd_station_mesure_eaux_surface")


d_op_station_support <- d_op[,.(n = TRUE),by = .(cd_station_mesure_eaux_surface, lb_support)]
d_op_station_support[,lb_support := f_change_colnames(lb_support)]
d_op_station_support_w <- dcast(d_op_station_support,cd_station_mesure_eaux_surface ~lb_support)
d_op_station_support_w[is.na(d_op_station_support_w)] <- FALSE

d_op_station_support <- d_op_station_support[,.(nb_support = .N),by = .(cd_station_mesure_eaux_surface)]
d_op_station_support <- merge(d_op_station_support, d_op_station_support_w,by="cd_station_mesure_eaux_surface")

d_station <- merge(d_station,d_op_station_support,by="cd_station_mesure_eaux_surface")

d_station[,.(nb = .N),by= .(libelle_projection,lb_region)]
d_station[,metropole := libelle_projection == "RGF93   Lambert 93"]

d_station[,.(nb = .N),by= .(libelle_projection,lb_region,metropole)]

vecprojection <- unique(d_station[,libelle_projection])
vecCRS <- c(2154,4559,2975,2972)
for(i in 1:length(vecprojection)) {
   # i <- 1
    p <- vecprojection[i]
    crs <- vecCRS[i]
   cat("\n",p,"| CRS:",crs,"\n")
    dp <- d_station[libelle_projection == p,]
    sf_dp <- st_as_sf(dp,coords = c("coord_xstation_mesure_eaux_surface","coord_ystation_mesure_eaux_surface"),crs = crs)
    sf_dp <- st_transform(sf_dp,crs = 4326)
    if(i == 1) sf_d <- sf_dp else sf_d <- rbind(sf_d,sf_dp)
}

write_sf(sf_d,dsn = con,layer="miriades_station")



## add bassin name
q <- "
ALTER TABLE miriades_station ADD COLUMN bassin_name VARCHAR;

WITH station_bassin AS (
	SELECT cd_station_mesure_eaux_surface, b.bassin_name
	FROM miriades_station s JOIN main_bassin_hydro b ON ST_INTERSECTS(s.geometry,b.geometry))
UPDATE miriades_station ms SET bassin_name = sb.bassin_name
FROM station_bassin sb WHERE ms.cd_station_mesure_eaux_surface = sb.cd_station_mesure_eaux_surface;
"

send_query(con,q)




tmap_mode("view")
tm <-   tm_shape(subset(sf_d,finalite_station_mesure_eaux_surface == "RCS")) + tm_dots("red")
tm

##tmap_save(tm, filename = "img/hydro_reg.html")



q <- "
SELECT *
FROM miriades_cep as cep, miriades_station as s
WHERE cep.cd_station_mesure_eaux_surface = s.cd_station_mesure_eaux_surface ;
"
q
sf_st_macroinv0 <- st_read( con, query = q)

tmap_mode("view")
tm <-   tm_shape(sf_st_macroinv0) + tm_dots("finalite_station_mesure_eaux_surface")
tm


q <- "
SELECT *
FROM miriades_cep as cep, miriades_station as s
WHERE cep.cd_station_mesure_eaux_surface = s.cd_station_mesure_eaux_surface AND cep.cd_support = 13;
"
q
sf_st_macroinv<- st_read( con, query = q)

tmap_mode("view")
tm <-   tm_shape(sf_st_macroinv) + tm_dots("finalite_station_mesure_eaux_surface")
tm


q <- "
SELECT * FROM (
	SELECT cd_station_mesure_eaux_surface,COUNT(*) AS nb
	FROM miriades_sample
	WHERE cd_support = 13
	GROUP BY cd_station_mesure_eaux_surface
	) as inv, miriades_station as s
WHERE
inv.cd_station_mesure_eaux_surface = s.cd_station_mesure_eaux_surface AND metropole = TRUE;
"
cat(q)
sf_st_macroinv<- st_read( con, query = q)

tmap_mode("view")
tm <-   tm_shape(sf_st_macroinv) + tm_dots("finalite_station_mesure_eaux_surface")
tm


st_macroinv <- as.data.frame(sf_st_macroinv)
setDT(st_macroinv)
d_finalite <- st_macroinv[,.(nb = .N),by = finalite_station_mesure_eaux_surface]
setorder(d_finalite,-nb)
print(d_finalite)

vec_finalite <- c("RCS","Référence","Contrôle de surveillance DCE")





q <- "
SELECT cd_station_mesure_eaux_surface, finalite_station_mesure_eaux_surface,
mean_sample_year, sd_sample_year, max_sample_year, first_year,last_year, nb_sample_y, duration,
nb_support, crustace,diatomees_benthiques,macroinvertebres_aquatiques, macrophytes, oligochetes, phytoplancton, poissons,
geometry
FROM miriades_station as s
WHERE metropole = TRUE AND macroinvertebres_aquatiques = TRUE AND duration > 40;
"
cat(q)
sf_station_old <- st_read( con, query = q)

tmap_mode("view")
tm <-   tm_shape(sf_station_old) + tm_dots("duration")
tm


tmap_mode("view")
tm <-   tm_shape(sf_station_old) + tm_dots("first_year")
tm




q <- "
SELECT cd_station_mesure_eaux_surface, finalite_station_mesure_eaux_surface,
mean_sample_year, sd_sample_year, max_sample_year, first_year,last_year, nb_sample_y, duration,
nb_support, crustace,diatomees_benthiques,macroinvertebres_aquatiques, macrophytes, oligochetes, phytoplancton, poissons,
geometry
FROM miriades_station as s
WHERE metropole = TRUE AND macroinvertebres_aquatiques = TRUE;
"
cat(q)
sf_station <- st_read( con, query = q)

tmap_mode("view")
tm <-   tm_shape(sf_station) + tm_dots("duration")
tm


tmap_mode("view")
tm <-   tm_shape(sf_station) + tm_dots("first_year")
tm


## recherche les sites

## Controle surveillance DCE # surveillance plan d'échnatillonnage structuré
## RCS # ==
## référence DCE # bonne qualité, reprénsentative du type de cours "

q <- "
SELECT cd_station_mesure_eaux_surface, finalite_station_mesure_eaux_surface,
mean_sample_year, sd_sample_year, max_sample_year, first_year,last_year, nb_sample_y, duration,
nb_support, crustace,diatomees_benthiques,macroinvertebres_aquatiques, macrophytes, oligochetes, phytoplancton, poissons,
geometry
FROM miriades_station as s
WHERE metropole = TRUE AND macroinvertebres_aquatiques = TRUE;
"
cat(q)
sf_station <- st_read( con, query = q)
