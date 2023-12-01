
###  French fish data management

setwd("c:/git/french_LPI/script/")
source("../functions/import_table_in2_posgresql.r")


do_init_db <- FALSE
do_length_hydro <- FALSE
do_import_data_Alain <- FALSE
do_site_river <- FALSE
do_site_reg <- FALSE

do_site_bassin <- FALSE
do_site_hydro <- FALSE
do_valide <- FALSE

con <- con_lpi()




if(do_init_db) {
    psql_posgis_init(con=con)

    psql_import_shp("../GIS/CoursEau_FXX-shp/CoursEau_FXX.shp","lpi_metro","postgres","postgres","hydro",crs_overwrite="WGS84")

    psql_import_shp("../GIS/RegionHydro_FXX-shp/RegionHydro_FXX.shp","lpi_metro","postgres","postgres","region_hydro",crs_overwrite="WGS84")



    query <- "
ALTER TABLE region_hydro
ADD COLUMN id_bassin SMALLINT,
ADD COLUMN bassin_name VARCHAR;
"
        send_query(con,query)




    query <- "
DROP TABLE IF EXISTS main_bassin_hydro;
CREATE TABLE main_bassin_hydro AS
SELECT MIN(r.ogc_fid) AS ogc_fid, min(gid) AS gid, id_bassin, bassin_name,
    ST_Multi(ST_Union(r.wkb_geometry)) AS geometry
FROM region_hydro AS r
GROUP BY id_bassin, bassin_name;
"
    send_query(con,query)



    psql_import_shp("../GIS/BassinVersantTopographique.shp/BassinVersantTopographique.shp","lpi_metro","postgres","postgres","bassin_hydro",crs_transform="WGS84")
    psql_import_shp("../../data_GIS/tnc_terr_ecoregions.shp","lpi_metro","postgres","postgres","ecoreg")



    query <- "
CREATE INDEX h_classe on hydro(classe);
CREATE INDEX e_eco_code on ecoreg(eco_code);
CREATE INDEX e_eco_num on ecoreg(eco_num);
CREATE INDEX r_cdregionhy on region_hydro(cdregionhy);
CREATE INDEX r_lbregionhy on region_hydro(lbregionhy);
"

    send_query(con,query)

}

if(do_length_hydro) {

    query <- "
DROP TABLE IF EXISTS length_hydro_region;
CREATE TABLE length_hydro_region AS
SELECT r.ogc_fid as ogc_fid, lbregionhy,classe,SUM(ST_LENGTH(ST_TRANSFORM(ST_INTERSECTION(h.wkb_geometry,r.wkb_geometry),2154)))/ 1000 as length_km
FROM hydro as h, region_hydro as r
GROUP BY lbregionhy, r.ogc_fid, classe;
"

send_query(con,query)


    query <- "
DROP TABLE IF EXISTS length_hydro_bassin;
CREATE TABLE length_hydro_bassin AS
SELECT b.id_bassin as id_bassin, bassin_name,classe,SUM(ST_LENGTH(ST_TRANSFORM(ST_INTERSECTION(h.wkb_geometry,b.geometry),2154)))/ 1000 as length_km
FROM hydro as h, main_bassin_hydro as b
GROUP BY bassin_name, id_bassin, classe;
"

send_query(con,query)




query <- "SELECT SUM(ST_LENGTH(ST_TRANSFORM(h.wkb_geometry,2154)))/1000 as length_km FROM hydro as h;"
dhydro <- send_query(con, query)


query <- "SELECT classe, SUM(ST_LENGTH(ST_TRANSFORM(h.wkb_geometry,2154)))/1000 as length_kmFROM hydro as h GROUP BY classe;"
dhydro_class <- send_query(con, query)

setDT(dhydro_class)
dhydro_class

}

if(do_import_data_Alain ) {
    load("../data/fish_metro/op_analysis.rda")

    setDT(op_analysis)
    dbWriteTable(con,"sample",value = op_analysis, overwrite = T, append = F, row.names = FALSE)


    load("../data/fish_metro/station_analysis.rda")
    setDT(station_analysis)

    coords <- st_coordinates(station_sp)
    station <- cbind(station_analysis,coords)

    write.table(station,"../data/fish_metro/station.csv",sep="\t",row.names = FALSE)
    station <- read.table("../data/fish_metro/station.csv",sep="\t",header=TRUE)

    setDT(station)
    station[,geometry := NULL]
    setDF(station)

    station_sf <- st_as_sf(x = station,
                           coords = c("X", "Y"),
                           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

    st_crs(station_sf) <- 4326


    dbWriteTable(con,"site",value = station_sf, overwrite = T, append = F, row.names = FALSE)


    load("../data/fish_metro/community_analysis.rda")
    setDT(community_analysis)
    dbWriteTable(con,"observation",value = community_analysis, overwrite = T, append = F, row.names = FALSE)

    load("../data/fish_metro/species.rda")
    setDT(species)


    species_nat <- fread("../data/fish_metro/Dortel_2023_tendances_poissons.csv")
    species_nat[Code == "GOL", Code := "GOI"]

    newsp <- species_nat[!(Code %in% species[,code]),]
    newsp[,`:=`(Tendance = NULL, Native = NULL)]
    setnames(newsp,c("Code","nom_scientifique","nom_commun"),c("code","latin_name","common_name"))

    newsp <- newsp[,colnames(species)[1:3],with = FALSE]

    species <- rbind(species,newsp,fill = TRUE)
    setorder(species,code)


    setnames(species_nat,"Tendance","trend_Dortel_2023")
    species_nat[,alien_Dortel_2023 := Native == "NON"]
    species_nat[,`:=`( nom_scientifique = NULL ,  nom_commun = NULL, Native = NULL)]

    dim(species)
    species <- merge(species,species_nat,by.x = "code",by.y = "Code",all.x = TRUE)
    dim(species)

    dbWriteTable(con,"species",value = species, overwrite = T, append = F, row.names = FALSE)
}


if(do_site_river) {

    query <- "
DROP TABLE IF EXISTS site_river100m;
CREATE TABLE site_river100m AS
SELECT id AS id_site,gid AS gid_hydro ,classe as class, ST_Distance(s.geometry,h.wkb_geometry) as distance, precise_location, com_code_insee, geometry
FROM site as s, hydro as h
WHERE ST_DWITHIN(ST_TRANSFORM(s.geometry,2154),ST_TRANSFORM(h.wkb_geometry,2154),100);
"
    send_query(con, query)

    query <- "
SELECT id_site, gid_hydro, distance FROM site_river100m;"
    dd <- send_query(con,query)
    setDT(dd)
    setorder(dd,id_site,distance)
    print(dd)
    dd[,inc := 1:.N,by=id_site]
    ddd <- dd[inc == 1]
    length(unique(dd[,id_site]))
    dim(dd)
    dim(ddd)

    dbWriteTable(con,"site_river",value = ddd, overwrite = T, append = F, row.names = FALSE)

}


if(do_site_reg) {

    query <- "
DROP TABLE IF EXISTS site_region_hydro;
CREATE TABLE site_region_hydro AS
SELECT id AS id_site,gid AS gid_region_hydro
FROM site as s, region_hydro as r
WHERE ST_INTERSECTS(s.geometry,r.wkb_geometry);
"
    send_query(con, query)


}



if(do_site_bassin) {

    query <- "
DROP TABLE IF EXISTS site_bassin_hydro;
CREATE TABLE site_bassin_hydro AS
SELECT id AS id_site,gid AS gid_bassin_hydro
FROM site as s, main_bassin_hydro as b
WHERE ST_INTERSECTS(s.geometry,b.geometry);
"
    send_query(con, query)


}


if(do_site_hydro) {

    query <- "
DROP TABLE IF EXISTS site_hydro;
CREATE TABLE site_hydro AS
SELECT s.id AS id_site,
	b.id_bassin, b.bassin_name,
	r.gid as id_region_hydro, lbregionhy as region_hydro_name,
	h.gid AS id_section, h.nomentiteh AS section_name, classe AS section_class, ROUND(distance::numeric,3) AS dist2river_m
FROM site AS s, hydro AS h, site_river AS sr, region_hydro AS r, site_region_hydro AS srh, main_bassin_hydro AS b, site_bassin_hydro as sb
WHERE s.id = sr.id_site AND sr.gid_hydro = h.gid AND
	s.id = srh.id_site AND srh.gid_region_hydro = r.gid AND
	s.id = sb.id_site AND sb.gid_bassin_hydro = b.gid
ORDER BY id_site;
"

    ds <- send_query(con,query)

}


# , region_hydro AS r, site_region_hydro AS srh, main_bassin_hydro AS b, site_bassin_hydro as sb


if(do_valide) {
    query <- "
SELECT *
FROM sample as s, site_hydro as h
WHERE s.station = h.id_site;"
    sample <- send_query(con,query)

    sample[,y_day := as.numeric(yday(date))]
    sample[,year := year(date)]
    sample[,date_fig := as.Date(paste0("2020-",substr(date,6,10)))]
    sample[,month := as.numeric(month(date))]
    sample[,season := ifelse(month >= 4 & month <= 7,"spring",ifelse(month >= 8 & month <= 11, "fall",""))]
    sample[,valid_season := season %in% c("spring","fall")]

    sample[,med_date := median(y_day),by = .(station, bassin_name,season)]
    sample[,diff_med_date := y_day - med_date]
    sample[,`:=`(diff_q1 = quantile(diff_med_date,0.25),diff_q3 = quantile(diff_med_date,0.75))]

    k = 1.5

    sample[,`:=`(tukey_fences_date_INF = diff_q1-k*(diff_q3-diff_q1),
                 tukey_fences_date_SUP = diff_q3+k*(diff_q3-diff_q1))]

    sample[, valid_date := diff_med_date  >= tukey_fences_date_INF & diff_med_date  <= tukey_fences_date_SUP]


 site_date <- sample[(valid_season & valid_date),.(mean_y_day = mean(y_day), sd_y_day = sd(y_day), first_year = min(year), last_year = max(year),nb_sample = .N, duration = 999, completude = 999, trend_y_day = 999, pval = 999), by=.(station, bassin_name,season)]

  nb_sample_sup3 <- sum(site_date[nb_sample > 3,nb_sample])
  site_date[,valid_series := nb_sample > 3]
  site_date[,duration := last_year - first_year +1]
  site_date[,completude := round(nb_sample / duration,3)]
  site_date[,cat_nb_sample := cut(nb_sample,breaks=c(0,2,3,4,10,20,Inf))]
  t_nb_sample <- site_date[,.(nb_site = .N), by = .(season,cat_nb_sample)]
setorder(t_nb_sample,season,cat_nb_sample)
    print(t_nb_sample)

     vec_period <- c("spring","fall")


    for(s in unique(sample[,station]))
  {
    for(p in vec_period) {
      ds <- sample[station == s & season == p,]
      if(nrow(ds) > 1) {
      md <- lm(y_day~year,data=ds)
      smd <- as.data.frame(summary(md)$coef)
      site_date[station == s & season == p, `:=`(trend_y_day = round(smd[2,1],3), pval =  round(smd[2,4],3))]
    }
    }
  }


  site_date[,cat_trend_y_day := cut(abs(trend_y_day),breaks=c(0,1,2,5,10,Inf))]



  site_trend <- site_date[,.(station,season,first_year,last_year,nb_sample,duration,completude,trend_y_day,pval,cat_trend_y_day,valid_series)]
  setnames(site_trend,"station","id_site")
  sample <- merge(sample,site_trend,by=c("id_site","season"))

    sample[,valid := valid_season & valid_date & valid_series]

    sample[,valid_trend_day := abs(trend_y_day) < 5]
    sample[,valid := valid & valid_trend_day]

    dbWriteTable(con,"sample",value = sample, overwrite = T, append = F, row.names = FALSE)

}
