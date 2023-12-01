
setwd("c:/git/french_LPI/script/")
source("../functions/import_table_in2_posgresql.r")


library(bit64)
library(data.table)
library(sf)
library(tmap)
library(lubridate)
library(scales)
library(ggplot2)
library(tmaptools)


con <- con_lpi()

if(do_data_stoc_river) {

"CREATE TABLE point_aqua AS(
WITH aqua AS (
	SELECT id_point,  p_milieu_sug ,s_milieu_sug
	FROM habitat
	GROUP BY id_point, p_milieu_sug, s_milieu_sug
	HAVING p_milieu_sug = 'F' OR s_milieu_sug = 'F'
),
pt_aqua AS(
	SELECT DISTINCT id_point as pk_point FROM aqua
)
SELECT pk_point,etude,nb_year,first_year,last_year,pt.geom
FROM point pt NATURAL JOIN pt_aqua
INNER JOIN carre ca ON pt.id_carre = ca.pk_carre
WHERE etude LIKE('STOC_EPS') AND nb_year > 3 );"


"CREATE TABLE stoc_eps_fw.stoc_point AS SELECT * FROM stoc_eps_fw.point;"

"CREATE TABLE stoc_eps_fw.stoc_point_aqua AS SELECT * FROM stoc_eps_fw.point_aqua;"



"DROP TABLE IF EXISTS stoc_eps_fw.stoc_aqua_river100m;
CREATE TABLE stoc_eps_fw.stoc_aqua_river100m AS
SELECT pk_point,gid AS gid_hydro ,classe as class, ST_Distance(pt.geom,h.wkb_geometry) as distance, etude, nb_year,first_year,last_year,geom
FROM stoc_eps_fw.stoc_point_aqua as pt, hydro as h
WHERE ST_DWITHIN(ST_TRANSFORM(pt.geom,2154),ST_TRANSFORM(h.wkb_geometry,2154),100);"


query <- "
DROP TABLE IF EXISTS site_river100m;
CREATE TABLE site_river100m AS
SELECT id AS id_site,gid AS gid_hydro ,classe as class, ST_Distance(s.geometry,h.wkb_geometry) as distance, precise_location, com_code_insee, geometry
FROM site as s, hydro as h
WHERE ST_DWITHIN(ST_TRANSFORM(s.geometry,2154),ST_TRANSFORM(h.wkb_geometry,2154),100);
"


send_query(con, query)



    "
CREATE TABLE stoc_eps_fw.stoc_observation AS SELECT * FROM stoc_eps_fw.observation;
CREATE TABLE stoc_eps_fw.stoc_inventaire AS SELECT * FROM stoc_eps_fw.inventaire;
CREATE TABLE stoc_eps_fw.stoc_species AS SELECT * FROM stoc_eps_fw.species;

CREATE INDEX i_id_point ON stoc_eps_fw.stoc_inventaire(id_point);
CREATE INDEX o_id_inventaire ON stoc_eps_fw.stoc_observation(id_inventaire);
CREATE INDEX o_code_sp ON stoc_eps_fw.stoc_observation(code_sp);
CREATE INDEX ptaqua_pk_point ON stoc_eps_fw.stoc_aqua_river100m(pk_point);
CREATE INDEX i_pk_inventaire ON stoc_eps_fw.stoc_inventaire(id_inventaire);
CREATE INDEX sp_pk_species ON stoc_eps_fw.species(pk_species);



"

 source("../../STOC_eps_database/functions/fun_generic.r")
 source("../../STOC_eps_database/functions/fun_export.r")

con_stoc <-  openDB.PSQL(user = "postgres",pw = "postgres")
### DB stoc_eps
    q <- "
CREATE SCHEMA temp;
"


send_query(con_stoc, q)

    q <-    "
DROP TABLE IF EXISTS  temp.point_observation_100;
CREATE TABLE  temp.point_observation_100 AS (
 select om.id_point as point, p.id_carre as carre, om.annee as annee,
(om.annee::varchar(4)||om.id_point::varchar(100))::varchar(100) as id_point_annee,
c.etude as etude, p.commune,p.insee,p.departement as departement,
om.code_sp , e.scientific_name as nom_scientifique, e.french_name as nom_francais, e.english_name as nom_anglais, e.euring as code_espece_euring,e.taxref as code_espece_taxref, 'all'::varchar(10) as distance_contact_max,
abond_brut as abondance_brut, abond_tuckey_outlier  as abondance_filtre_tuckey,abond_tuckey_farout  as abondance_filtre_tuckey_farout, seuil_all_tukey_outlier as seuil_tuckey_outlier, seuil_all_tukey_farout as seuil_tuckey_farout,
 pa.qualite_inventaire_stoc as qualite_inventaire_stoc, pa.nombre_passage_stoc_annee, pa.info_passage_an,
p.altitude, longitude_wgs84,  latitude_wgs84,  longitude_grid_wgs84,latitude_grid_wgs84,
 pa.p_habitat_sug_pass1 as code_habitat_principal_pass1,hcp1.habitat_nom as habitat_principal_pass1,
 pa.p_habitat_sug_pass2 as code_habitat_principal_pass2, hcp2.habitat_nom as habitat_principal_pass2,
 pa.s_habitat_sug_pass1 as code_habitat_secondaire_pass1, hcs1.habitat_nom as habitat_secondaire_pass1,
 pa.s_habitat_sug_pass2 as code_habitat_secondaire_pass2,hcs2.habitat_nom as habitat_secondaire_pass2,
pa.time_declaration_sug_j as temps_depuis_derniere_description_habitat,
   pa.foret_p as foret_p, pa.agri_p as agricole_p, pa.urbain_p as urbain_p, pa.ouvert_p as ouvert_p, pa.foret_ps as foret_ps, pa.agri_ps as agricole_ps, pa.urbain_ps as urbain_ps, pa.ouvert_ps as ouvert_ps,
  nbp_foret_p  as carre_nb_pts_foret_p, nbp_ouvert_p as carre_nb_pts_ouvert_p, nbp_agri_p as carre_nb_pts_agricole_p, nbp_urbain_p as carre_nb_pts_urbain_p,
nbp_foret_ps as carre_nb_pts_foret_ps, nbp_ouvert_ps as carre_nb_pts_ouvert_ps, nbp_agri_ps as carre_nb_pts_agricole_ps, nbp_urbain_ps  as carre_nb_pts_urbain_ps,  p.db as data_base_name, '2023-10-12'::varchar(10) as date_export,
'Lorrilliere Romain'::varchar(50) as operateur,
'lorrilliere@mnhn.fr'::varchar(50) as email_operateur

from(
	select id_point, annee,code_sp,max(abond_brut) as abond_brut,max(abond_tuckey_outlier) as abond_tuckey_outlier,max(abond_tuckey_farout) as abond_tuckey_farout
		from -- ## begin ## correction abondance obs par seuil
			(select id_inventaire,id_point, passage_stoc, annee, opb.code_sp,abond as abond_brut, LEAST(abond, seuil_all_tukey_outlier) as abond_tuckey_outlier, seuil_all_tukey_outlier as seuil_tuckey_outlier, LEAST(abond, seuil_all_tukey_farout) as abond_tuckey_farout, seuil_all_tukey_farout as seuil_tuckey_farout
			from -- ## begin ## selection classe de distance et different filtre
				(  SELECT id_inventaire, code_sp, sum(abondance) as abond
				FROM
				observation as o, inventaire as i,point as p, point_annee as pa, carre as c
				WHERE
				o.id_inventaire = i.pk_inventaire and
				o.id_carre = c.pk_carre and
				o.id_point = p.pk_point and
				o.id_point = pa.id_point and o.annee=pa.annee and
				pa.qualite_inventaire_stoc > 0 and
                                  code_sp in  ('APUAPU' , 'DELURB' , 'HIRRUS' , 'RIPRIP' , 'ACCNIS' , 'FALTIN' , 'BUTBUT' , 'CIRAER' , 'CIRCYA' , 'CIRPYG' , 'MILMIG')  and

                                passage_stoc in (1,2) and  i.annee >= 2001  and i.annee <= 2023 and c.etude in ('STOC_EPS', 'STOC_ONF')  and  p.altitude <= 8000 and p.altitude >= 0





			GROUP BY
			id_inventaire, code_sp
	                        ) --  ## end ## selection classe de distance et different filtre
			as opb, seuil_obs as s, inventaire as i
			WHERE
			opb.code_sp = s.code_sp and opb.id_inventaire = i.pk_inventaire

			) -- ## end ## correction abondance obs par seuil
		as op
		group by id_point,annee,code_sp
		 )-- ## end ## obs max par point sur 2 passages
		 as om
 INNER JOIN point as p ON om.id_point = p.pk_point
INNER JOIN carre as c ON p.id_carre = c.pk_carre
LEFT JOIN species as e ON om.code_sp = e.pk_species
INNER JOIN seuil_obs as s  ON om.code_sp = s.code_sp
INNER JOIN point_annee as pa ON (om.id_point = pa.id_point and om.annee = pa.annee)
INNER JOIN carre_annee as ca ON (p.id_carre = ca.id_carre and om.annee = ca.annee)
  LEFT JOIN habitat_cat as hcp1  ON pa.p_habitat_sug_pass1 = hcp1.habitat_code
LEFT JOIN habitat_cat as hcp2 ON pa.p_habitat_sug_pass2 = hcp2.habitat_code
LEFT JOIN habitat_cat as hcs1 ON pa.s_habitat_sug_pass1 = hcs1.habitat_code
LEFT JOIN habitat_cat as hcs2  ON pa.s_habitat_sug_pass2 = hcs2.habitat_code
order by
om.id_point, annee,code_sp);

INSERT INTO temp.point_observation_100
 select om.id_point as point, p.id_carre as carre, om.annee as annee,
(om.annee::varchar(4)||om.id_point::varchar(100))::varchar(100) as id_point_annee,
c.etude as etude, p.commune,p.insee,p.departement as departement,
om.code_sp , e.scientific_name as nom_scientifique, e.french_name as nom_francais, e.english_name as nom_anglais, e.euring as code_espece_euring,e.taxref as code_espece_taxref, 'inf'::varchar(10) as distance_contact_max,
abond_brut as abondance_brut, abond_tuckey_outlier  as abondance_filtre_tuckey,abond_tuckey_farout  as abondance_filtre_tuckey_farout, seuil_inf_tukey_outlier as seuil_tuckey_outlier, seuil_inf_tukey_farout as seuil_tuckey_farout,
 pa.qualite_inventaire_stoc as qualite_inventaire_stoc,pa.nombre_passage_stoc_annee, pa.info_passage_an,
p.altitude, longitude_wgs84,  latitude_wgs84,  longitude_grid_wgs84,latitude_grid_wgs84,
 pa.p_habitat_sug_pass1 as code_habitat_principal_pass1,hcp1.habitat_nom as habitat_principal_pass1,
 pa.p_habitat_sug_pass2 as code_habitat_principal_pass2, hcp2.habitat_nom as habitat_principal_pass2,
 pa.s_habitat_sug_pass1 as code_habitat_secondaire_pass1, hcs1.habitat_nom as habitat_secondaire_pass1,
 pa.s_habitat_sug_pass2 as code_habitat_secondaire_pass2,hcs2.habitat_nom as habitat_secondaire_pass2,
pa.time_declaration_sug_j as temps_depuis_derniere_description_habitat,
   pa.foret_p as foret_p, pa.agri_p as agricole_p, pa.urbain_p as urbain_p, pa.ouvert_p as ouvert_p, pa.foret_ps as foret_ps, pa.agri_ps as agricole_ps, pa.urbain_ps as urbain_ps, pa.ouvert_ps as ouvert_ps,
  nbp_foret_p  as carre_nb_pts_foret_p, nbp_ouvert_p as carre_nb_pts_ouvert_p, nbp_agri_p as carre_nb_pts_agricole_p, nbp_urbain_p as carre_nb_pts_urbain_p,
nbp_foret_ps as carre_nb_pts_foret_ps, nbp_ouvert_ps as carre_nb_pts_ouvert_ps, nbp_agri_ps as carre_nb_pts_agricole_ps, nbp_urbain_ps  as carre_nb_pts_urbain_ps,  p.db as data_base_name, '2023-10-12'::varchar(10) as date_export,
'Lorrilliere Romain'::varchar(50) as operateur,
'lorrilliere@mnhn.fr'::varchar(50) as email_operateur

from(
	select id_point, annee,code_sp,max(abond_brut) as abond_brut,max(abond_tuckey_outlier) as abond_tuckey_outlier,max(abond_tuckey_farout) as abond_tuckey_farout
		from -- ## begin ## correction abondance obs par seuil
			(select id_inventaire,id_point, passage_stoc, annee, opb.code_sp,abond as abond_brut, LEAST(abond, seuil_inf_tukey_outlier) as abond_tuckey_outlier, seuil_inf_tukey_outlier as seuil_tuckey_outlier, LEAST(abond, seuil_inf_tukey_farout) as abond_tuckey_farout, seuil_inf_tukey_farout as seuil_tuckey_farout
			from -- ## begin ## selection classe de distance et different filtre
				(  SELECT id_inventaire, code_sp, sum(abondance) as abond
				FROM
				observation as o, inventaire as i,point as p, point_annee as pa, carre as c
				WHERE
				o.id_inventaire = i.pk_inventaire and
				o.id_carre = c.pk_carre and
				o.id_point = p.pk_point and
				o.id_point = pa.id_point and o.annee=pa.annee and
				pa.qualite_inventaire_stoc > 0 and
                                 code_sp not  in  ('MOTFLA' , 'SAXRUB' , 'ANTPRA' , 'OENOEN' , 'PHYTRO')  and

                                passage_stoc in (1,2) and  distance_contact in ('inf_25m','25-50m','50-100m','25-100m') and  i.annee >= 2001  and i.annee <= 2023 and c.etude in ('STOC_EPS', 'STOC_ONF')  and  p.altitude <= 8000 and p.altitude >= 0





			GROUP BY
			id_inventaire, code_sp
                                union
                                  --  ajout des especes tardives dont on ne garde que le second passage
             SELECT id_inventaire, code_sp, sum(abondance) as abond
				FROM
				observation as o, inventaire as i,point as p, point_annee as pa, carre as c
				WHERE
				o.id_inventaire = i.pk_inventaire and
				o.id_carre = c.pk_carre and
				o.id_point = p.pk_point and
				o.id_point = pa.id_point and o.annee=pa.annee and
				pa.qualite_inventaire_stoc > 0 and
                                 code_sp in  ('MOTFLA' , 'SAXRUB' , 'ANTPRA' , 'OENOEN' , 'PHYTRO')  and    passage_stoc = 2 and  distance_contact in ('inf_25m','25-50m','50-100m','25-100m') and  i.annee >= 2001  and i.annee <= 2023 and c.etude in ('STOC_EPS', 'STOC_ONF')  and  p.altitude <= 8000 and p.altitude >= 0





			GROUP BY
			id_inventaire, code_sp
	                        ) --  ## end ## selection classe de distance et different filtre
			as opb, seuil_obs as s, inventaire as i
			WHERE
			opb.code_sp = s.code_sp and opb.id_inventaire = i.pk_inventaire

			) -- ## end ## correction abondance obs par seuil
		as op
		group by id_point,annee,code_sp
		 )-- ## end ## obs max par point sur 2 passages
		 as om
 INNER JOIN point as p ON om.id_point = p.pk_point
INNER JOIN carre as c ON p.id_carre = c.pk_carre
LEFT JOIN species as e ON om.code_sp = e.pk_species
INNER JOIN seuil_obs as s  ON om.code_sp = s.code_sp
INNER JOIN point_annee as pa ON (om.id_point = pa.id_point and om.annee = pa.annee)
INNER JOIN carre_annee as ca ON (p.id_carre = ca.id_carre and om.annee = ca.annee)
  LEFT JOIN habitat_cat as hcp1  ON pa.p_habitat_sug_pass1 = hcp1.habitat_code
LEFT JOIN habitat_cat as hcp2 ON pa.p_habitat_sug_pass2 = hcp2.habitat_code
LEFT JOIN habitat_cat as hcs1 ON pa.s_habitat_sug_pass1 = hcs1.habitat_code
LEFT JOIN habitat_cat as hcs2  ON pa.s_habitat_sug_pass2 = hcs2.habitat_code
order by
om.id_point, annee,code_sp;
"

send_query(con_stoc, q)


    q <- "
IMPORT FOREIGN SCHEMA temp LIMIT TO (point_observation_100) FROM SERVER stoc_eps_fw INTO stoc_eps_fw;
DROP TABLE IF EXISTS  stoc_eps_fw.stoc_observation_point_100 ;
CREATE TABLE stoc_eps_fw.stoc_observation_point_100 AS SELECT * FROM stoc_eps_fw.point_observation_100;"
send_query(con, q)




    q <- "
DROP TABLE IF EXiSTS stoc_eps_fw.stoc_species_list_indicateur;
CREATE TABLE stoc_eps_fw.stoc_species_list_indicateur AS SELECT * FROM stoc_eps_fw.species_list_indicateur;

UPDATE stoc_eps_fw.stoc_species_list_indicateur SET habitat_specialisation_f = 'Aquatique' WHERE pk_species IN ('ANAPLA','GALCHL','ARDCIN','MOTCIN','FULATR','CETCET','ACRSCI','ALCATT','CYGOLO','PODCRI','PHACAR');
"
send_query(con, q)




















}






q <- "
SELECT pk_point, gid_hydro,class,distance,nb_year,first_year,last_year FROM stoc_eps_fw.stoc_aqua_river100m;
"
d <- send_query(con, q)

dim(d)

summary(d[,distance])

max(d[,distance])

gg <- ggplot(data= d[first_year > 2000,], aes(x="",y=nb_year)) + geom_violin()
gg

gg <- ggplot(data= d[first_year > 2000,], aes(x="",y=first_year)) + geom_violin()
gg


gg <- ggplot(data= d[first_year > 2000,], aes(x="",y=last_year)) + geom_violin()
gg

q <- "
SELECT point,annee,nom_francais AS french_name, code_sp, habitat_specialisation_f AS specialisation, abondance_brut, abondance_filtre_tuckey as abondance
FROM stoc_eps_fw.stoc_aqua_river100m pt
JOIN stoc_eps_fw.stoc_observation_point o ON pt.pk_point = o.point
JOIN stoc_eps_fw.stoc_species_list_indicateur i ON o.code_sp = i.pk_species ;
"
dobs <- send_query(con, q)

dobs[,sum_point := sum(abondance),by = .(code_sp,point)]

head(dobs)



dsp_y <- dobs[annee > 2000,.(nb_occ_y = .N),by = .(french_name, code_sp,specialisation,annee)]
dsp_y[,totsp := sum(nb_occ_y),by = .(french_name,code_sp,specialisation)]
setorder(dsp_y,-totsp,-annee)

dsp <- dsp_y[annee > 2000,.(nb_occ = sum(nb_occ_y)),by = .(french_name,code_sp,specialisation)]
setorder(dsp,-nb_occ)
dsp
hist(dsp$nb_occ)



dsp200 <- dsp[nb_occ > 200,]
dsp200



q <- "
SELECT * FROM stoc_eps_fw.stoc_aqua_river100m;
"
stoc_sf <- st_read( con, query = q)

hydro_sf <- read_sf(con,"hydro")
bassin_sf <- read_sf(con,"main_bassin_hydro")

hydro_sf$lwd <- rescale((-1 * hydro_sf$classe), to = c(0.5,5))
hydro_sf_1 <- subset(hydro_sf,classe < 4)




``
hydro_sf_2 <- subset(hydro_sf, gid %in% d[,gid_hydro])
## hydro_sf_2$lwd <-

myPalette <- get_brewer_pal("Blues", n = 3, contrast = c(0.92,0.57))

tmap_mode("view")
tm <-   tm_shape(bassin_sf) + tm_fill("bassin_name",alpha = 0.5)  +
    tm_shape(hydro_sf_1) + tm_lines(alpha = 0.5,col = "black",lwd = "lwd",scale = 0.8) +
    tm_shape(hydro_sf_2) + tm_lines(col = "classe",palette = myPalette,lwd = "lwd",scale = 3) +
    tm_shape(stoc_sf) + tm_dots("red")

tm


tmap_save(tm, filename = "output/stoc_river.html")






tmap_mode("view")
tm <-   tm_shape(sf_stoc) + tm_dots("nb_year")
tm




gg <- ggplot(data = dsp_y[french_name == "Martin-pêcheur d'Europe"],aes(x=annee,y=nb_occ_y)) + geom_line()
gg




q <- "

WITH
	sp_aqua AS (SELECT pk_species AS code_sp FROM stoc_eps_fw.stoc_species_list_indicateur WHERE  habitat_specialisation_f = 'Aquatique'),
	point_aqua AS (SELECT DISTINCT point FROM stoc_eps_fw.stoc_observation_point op),
	sp_point AS (
		SELECT point, code_sp, SUM(abondance_brut)>0 AS point_sp_present
		FROM stoc_eps_fw.stoc_observation_point op
		NATURAL JOIN sp_aqua spi
		GROUP BY point, code_sp
		ORDER BY point,code_sp),
	point_annee AS (
		SELECT DISTINCT point, annee FROM stoc_eps_fw.stoc_observation_point
	),
	cross_abs AS (
		SELECT point, annee, code_sp
		FROM point_annee inv
		NATURAL JOIN point_aqua pa
		CROSS JOIN sp_aqua
		WHERE annee > 2000
	),
	cross_abs_sp AS (SELECT ROW_NUMBER() OVER (ORDER BY point) AS id_abs, * FROM cross_abs NATURAL JOIN sp_point),
	obs_aqua AS(
	SELECT ROW_NUMBER() OVER (ORDER BY point) AS id_obs, point, annee, nom_francais, code_sp, abondance_brut, abondance_filtre_tuckey
		FROM stoc_eps_fw.stoc_observation_point o INNER JOIN stoc_eps_fw.stoc_species_list_indicateur li ON o.code_sp = li.pk_species
		WHERE habitat_specialisation_f = 'Aquatique'
	)
SELECT cas.point,cas.annee, cas.code_sp, french_name, COALESCE(abondance_brut,0), COALESCE(abondance_filtre_tuckey,0) as abondance
FROM cross_abs_sp cas
LEFT JOIN obs_aqua oa
ON cas.point = oa.point AND cas.annee = oa.annee AND cas.code_sp = oa.code_sp
JOIN stoc_eps_fw.stoc_species sp ON sp.pk_species = cas.code_sp
;
"
dobs <- send_query(con, q)

dobs






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

DROP TABLE IF EXISTS stoc_eps_fw.obs_point_river_abs ;
CREATE TABLE stoc_eps_fw.obs_point_river_abs AS
WITH
	-- occurence by year and species
	occ_y AS (
		SELECT code_sp, annee, COUNT(*) AS occ_y
		FROM stoc_eps_fw.stoc_observation_point_100 AS po
		INNER JOIN stoc_eps_fw.stoc_point_aqua AS pa ON po.point = pa.pk_point
		GROUP BY code_sp, annee),
	-- median of yearly occurence for each species
	occ_med AS (
		SELECT code_sp, PERCENTILE_DISC(0.5) WITHIN GROUP (ORDER BY occ_y) AS occ_med
		FROM occ_y
		GROUP BY code_sp
		ORDER BY occ_med DESC),
	-- Selection of species that have a median of yearly occurrence above 15
	sp_aqua AS (SELECT code_sp
				FROM occ_med om
				WHERE occ_med > 15),
	-- aquatic point define by another table
	point_aqua AS (SELECT DISTINCT pk_point AS point FROM stoc_eps_fw.stoc_point_aqua op),
	-- aquatic observation
	obs_point_aqua AS (
		SELECT point, annee, code_sp,  abondance_brut, abondance_filtre_tuckey
		FROM stoc_eps_fw.stoc_observation_point_100 op
		NATURAL JOIN sp_aqua NATURAL JOIN point_aqua
		WHERE annee > 2001
	),
	-- all available point for each species
	sp_point AS (
		SELECT point, code_sp, SUM(abondance_brut)>0 AS point_sp_present
		FROM obs_point_aqua op
		GROUP BY point, code_sp
		ORDER BY point,code_sp),
	-- yearly sample
	point_annee AS (
		SELECT DISTINCT point, annee FROM obs_point_aqua
	),
	-- yearly aquatic sample
	inv_aqua AS (
		SELECT point, annee
		FROM point_annee inv
		NATURAL JOIN point_aqua pa
	),
	-- all sample-species pairs
	cross_abs AS (
		SELECT point, annee, code_sp
		FROM inv_aqua inv
		CROSS JOIN sp_aqua
	),
	-- all available species-sample pairs
	cross_abs_sp AS (SELECT ROW_NUMBER() OVER (ORDER BY point) AS id_abs, * FROM cross_abs NATURAL JOIN sp_point),
	obs_aqua_abs AS(
		SELECT cas.point, cas.annee, cas.code_sp,  COALESCE(abondance_brut,0) AS obs_brut,  COALESCE(abondance_filtre_tuckey,0) as obs
		FROM cross_abs_sp cas LEFT JOIN obs_point_aqua oa ON cas.point = oa.point AND cas.annee = oa.annee AND cas.code_sp = oa.code_sp
	)
SELECT pk_point_annee as sample,point,pa.id_carre as carre,po.departement, oaa.annee AS year,
qualite_inventaire_stoc, nombre_passage_stoc_annee, info_passage_an,
code_sp, french_name,  habitat_specialisation_f AS specialisation, obs_brut, obs
FROM obs_aqua_abs oaa
INNER JOIN stoc_eps_fw.stoc_species_list_indicateur li ON oaa.code_sp = li.pk_species
INNER JOIN stoc_eps_fw.stoc_species sp ON sp.pk_species = oaa.code_sp
INNER JOIN stoc_eps_fw.point_annee pa ON pa.id_point = oaa.point AND pa.annee = oaa.annee
INNER JOIN stoc_eps_fw.stoc_point po ON po.pk_point = oaa.point
ORDER BY point, oaa.annee, code_sp ;

"

send_query(con, q)


q <- "
SELECT * FROM stoc_eps_fw.obs_point_river_abs
;"


dobs <- send_query(con, q)

dobs



dobs[,sum_point := sum(obs),by = .(code_sp,point)]

head(dobs)

dobs[,year_txt := as.character(year)]

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


md <- glm(obs~year_txt + point,data = dobs[code_sp == sp,],family = quasipoisson)
smd <- summary(md)
coef_year_sp <- as.data.frame(smd$coefficients)
colnames(coef_year_sp) <- c("coef","sd","tval","p_val")
coef_year_sp <- coef_year_sp[grep("year",rownames(coef_year_sp)),]
coef_year_sp$year <- as.numeric(substr(rownames(coef_year_sp),9,13))
setDT(coef_year_sp)
coef_year_sp[,code_sp := sp]


md.sim <- sim(md)
coef_sim_sp <- coef(md.sim)
coef_sim_sp <- coef_sim_sp[,grep("year",colnames(coef_sim_sp))]
coef_sim_sp <- reshape2::melt(coef_sim_sp)
colnames(coef_sim_sp) <- c("id_sim","year","coef")
setDT(coef_sim_sp)
coef_sim_sp[,code_sp := sp]
coef_sim_sp[,year :=  as.numeric(substr(year,9,13))]


    if(i == 1) {
        coef_year <- coef_year_sp
        coef_sim <- coef_sim_sp
    } else {
        coef_year <- rbind(coef_year,coef_year_sp)
        coef_sim <- rbind(coef_sim,coef_sim_sp)

    }

}

fwrite(coef_year, "output/coef_year_bird_river.csv")
fwrite(coef_sim, "output/coef_sim_bird_river.csv")



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



### Figure sp

gg_coef <- coef_year[,.(code_sp,year,var_year,ICinf,ICsup)]
gg_coef[,panel := "Abondance relative"]
col_gg <- colnames(gg_coef)

occ_sp <- dsp_y[,.(code_sp,year,prop_occ,nb_occ_y)]
setnames(occ_sp,c("prop_occ","nb_occ_y"),c("proportion","occurence"))
occ_sp <- melt(occ_sp,id.vars = c("code_sp","year"))
setnames(occ_sp,c("variable","value"),c("panel","var_year"))
occ_sp[,`:=`(ICinf = NA, ICsup = NA)]
occ_sp <- occ_sp[,col_gg, with=FALSE]

gg_coef <- rbind(gg_coef,occ_sp)


table_col <- data.frame(specialisation = c("generaliste","milieux forestiers","milieux bâtis","","Aquatique","milieux agricoles" ), col = c("#762a83","#2ca25f","#de2d26","#878787","#2c7fb8","#fee08b" ))
setDT(table_col)

nb_sp <- nrow(dsp)

for(i in 1:nb_sp) {
   sp <- dsp[i,code_sp]
  nom_sp <- dsp[i,french_name]
 tot <- dsp[i,nb_occ]
 med <- dsp[i,median_occ]
  spe <- dsp[i,specialisation]
  col <- table_col[specialisation == spe, col]
  titre <- paste0(nom_sp," (",sp,")")
  sub <-  paste0("mediane des occurence: ",med,", nombre d'occurence total: ",tot,", ",spe)


  gg <- ggplot(data = gg_coef[code_sp == sp & year < 2020,], aes(x = year, y = var_year)) + facet_grid(panel~.,scales = "free_y")
 gg <- gg + geom_point(colour = col,size =2) + geom_line(colour = col,size = 1.2)
 gg <- gg + geom_ribbon(aes(ymin = ICinf, ymax=ICsup), fill = col,alpha = 0.5)
  gg <- gg + labs(title = titre, subtitle = sub, x= "Années",y="")
 print(gg)
 ggfile <- paste0("output/bird_river_2023-11-24/",sp,"_variation_river.png")
 ggsave(ggfile,gg)

}




### LPI

coef_sim <- merge(coef_sim,dsp,by="code_sp")

for (i in 1:100) {
    simi <- coef_sim[id_sim == i,]

    agg_toti <- simi[,.(geo_mean = exp(mean(log(var_year))),median = median(var_year) ),by = year]
    agg_gri <- simi[,.(geo_mean = exp(mean(log(var_year))),median = median(var_year)  ),by = .(year,specialisation)]

    if(i == 1) {
        agg_tot <- agg_toti
        agg_gr <- agg_gri
    } else {
        agg_tot <- rbind(agg_tot,agg_toti)
        agg_gr <- rbind(agg_gr,agg_gri)

    }

}


agg_tot <- melt(agg_tot,id.vars = "year")

agg_gr <- melt(agg_gr,id.vars = c("year","specialisation"))

agg_tot <- agg_tot[,.(val = mean(value), ICinf = quantile(value,0.025), ICsup = quantile(value,0.975)),by = .(year,variable)]

agg_gr <- agg_gr[,.(val = mean(value), ICinf = quantile(value,0.025), ICsup = quantile(value,0.975)),by = .(year,specialisation,variable)]



  gg <- ggplot(data = agg_tot[year < 2020,], aes(x = year, y = val, colour = variable, fill = variable, group = variable))# + facet_grid(variable~.,scales = "free_y")
 gg <- gg + geom_point(size = 1.5) + geom_line(size = 1.2)
 gg <- gg + geom_ribbon(aes(ymin = ICinf, ymax=ICsup),alpha = 0.2,colour= NA)
  gg <- gg + labs(title = "Oiseaux des rivières", x= "Années",y="")
 print(gg)
 ggfile <- paste0("output/bird_river_2023-11-24/_LPI_tout_sp_variation_river.png")
 ggsave(ggfile,gg)



  gg <- ggplot(data = agg_gr[year < 2020,], aes(x = year, y = val, colour = variable, fill = variable, group = variable)) + facet_wrap(specialisation~.,scales = "free_y")
 gg <- gg + geom_point(size =1.5) + geom_line(size = 1.2)
 gg <- gg + geom_ribbon(aes(ymin = ICinf, ymax=ICsup),alpha = 0.2, colour = NA)
  gg <- gg + labs(title = "Oiseaux des rivières", x= "Années",y="")
 print(gg)
 ggfile <- paste0("output/bird_river_2023-11-24/_LPI_group_variation_river.png")
 ggsave(ggfile,gg)


  gg <- ggplot(data = agg_gr[year < 2020 & specialisation == "Aquatique",], aes(x = year, y = val, colour = variable, fill = variable, group = variable)) + facet_wrap(specialisation~.,scales = "free_y")
 gg <- gg + geom_point(size =1.5) + geom_line(size = 1.2)
 gg <- gg + geom_ribbon(aes(ymin = ICinf, ymax=ICsup),alpha = 0.2, colour = NA)
  gg <- gg + labs(title = "Oiseaux aquatiques des rivières", x= "Années",y="")
 print(gg)
 ggfile <- paste0("output/bird_river_2023-11-24/_LPI_group_variation_river_aquatique.png")
 ggsave(ggfile,gg)
