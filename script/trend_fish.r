
R version 4.3.1 (2023-06-16 ucrt) -- "Beagle Scouts"
Copyright (C) 2023 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R est un logiciel libre livré sans AUCUNE GARANTIE.
Vous pouvez le redistribuer sous certaines conditions.
Tapez 'license()' ou 'licence()' pour plus de détails.

R est un projet collaboratif avec de nombreux contributeurs.
Tapez 'contributors()' pour plus d'information et
'citation()' pour la façon de le citer dans les publications.

Tapez 'demo()' pour des démonstrations, 'help()' pour l'aide
en ligne ou 'help.start()' pour obtenir l'aide au format HTML.
Tapez 'q()' pour quitter R.

[Sauvegarde de la session précédente restaurée]

> setwd('c:/git/french_LPI')
> data.table 1.14.8 using 8 threads (see ?getDTthreads).  Latest news: r-datatable.com
> Linking to GEOS 3.11.2, GDAL 3.6.2, PROJ 9.2.0; sf_use_s2() is TRUE
> > > The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
which was just loaded, will retire in October 2023.
Please refer to R-spatial evolution reports for details, especially
https://r-spatial.org/r/2023/05/15/evolution4.html.
It may be desirable to make the sf package available;
package maintainers should consider adding sf to Suggests:.
The sp package is now running under evolution status 2
     (status 2 uses the sf package in place of rgdal)
> > > 
Attachement du package : 'lubridate'

Les objets suivants sont masqués depuis 'package:data.table':

    hour, isoweek, mday, minute, month, quarter, second, wday, week,
    yday, year

Les objets suivants sont masqués depuis 'package:base':

    date, intersect, setdiff, union

> > > Erreur dans file(filename, "r", encoding = encoding) : 
  impossible d'ouvrir la connexion
De plus : Message d'avis :
Dans file(filename, "r", encoding = encoding) :
  impossible d'ouvrir le fichier '../functions/import_table_in2_posgresql.r' : No such file or directory
> > > > Erreur dans con_lpi() : impossible de trouver la fonction "con_lpi"
> > Erreur dans file(filename, "r", encoding = encoding) : 
  impossible d'ouvrir la connexion
De plus : Message d'avis :
Dans file(filename, "r", encoding = encoding) :
  impossible d'ouvrir le fichier '../functions/import_table_in2_posgresql.r' : No such file or directory
> getwd()
[1] "c:/git/french_LPI"
> setdw("script/")
Erreur dans setdw("script/") : impossible de trouver la fonction "setdw"
> setdw("script")
Erreur dans setdw("script") : impossible de trouver la fonction "setdw"
> setwd("script")
> 
Attachement du package : 'reshape2'

Les objets suivants sont masqués depuis 'package:data.table':

    dcast, melt

Le chargement a nécessité le package : usethis

Attachement du package : 'dplyr'

L'objet suivant est masqué depuis 'package:doBy':

    order_by

Les objets suivants sont masqués depuis 'package:data.table':

    between, first, last

Les objets suivants sont masqués depuis 'package:stats':

    filter, lag

Les objets suivants sont masqués depuis 'package:base':

    intersect, setdiff, setequal, union

Le chargement a nécessité le package : DBI
> lpi_metro postgres **** 
> + + > 
Requete distance:

 
SELECT * FROM onema_ofb_fish_trends
 

>    code year predicted std.error   conf.low conf.high    group  variable season
1:  ABH 1998 0.5000002 1.2701469 0.04147813  6.027277 year_txt abundance   fall
2:  ABH 2000 0.8695754 0.8228158 0.17335053  4.362037 year_txt abundance   fall
3:  ABH 2002 1.4045111 0.7724399 0.30904599  6.383035 year_txt abundance   fall
4:  ABH 2003 0.2843846 1.1600127 0.02927531  2.762553 year_txt abundance   fall
5:  ABH 2010 1.6666674 0.7802877 0.36113270  7.691854 year_txt abundance   fall
6:  ABH 2006 0.4033726 1.2130245 0.03742640  4.347451 year_txt abundance   fall
   bassin_name    common_name            latin_name trend_dortel_2023
1:  Rhin-Meuse Able de Heckel Leucaspius delineatus              <NA>
2:  Rhin-Meuse Able de Heckel Leucaspius delineatus              <NA>
3:  Rhin-Meuse Able de Heckel Leucaspius delineatus              <NA>
4:  Rhin-Meuse Able de Heckel Leucaspius delineatus              <NA>
5:  Rhin-Meuse Able de Heckel Leucaspius delineatus              <NA>
6:  Rhin-Meuse Able de Heckel Leucaspius delineatus              <NA>
   alien_dortel_2023          ref predicted_sc  conf.low_sc conf.high_sc
1:                NA 1.502236e-13 3.328373e+12 2.761093e+11 4.012203e+13
2:                NA 1.502236e-13 5.788540e+12 1.153950e+12 2.903696e+13
3:                NA 1.502236e-13 9.349470e+12 2.057240e+12 4.249023e+13
4:                NA 1.502236e-13 1.893075e+12 1.948782e+11 1.838961e+13
5:                NA 1.502236e-13 1.109458e+13 2.403968e+12 5.120270e+13
6:                NA 1.502236e-13 2.685148e+12 2.491380e+11 2.893987e+13
   diff_conf       q1       q3 tukey_fences_SUP valid_trend
1: 145.31216 6.360214 64.61051         239.3614        TRUE
2:  25.16310 6.360214 64.61051         239.3614        TRUE
3:  20.65400 6.360214 64.61051         239.3614        TRUE
4:  94.36462 6.360214 64.61051         239.3614        TRUE
5:  21.29925 6.360214 64.61051         239.3614        TRUE
6: 116.16001 6.360214 64.61051         239.3614        TRUE
> . + > 
Requete distance:

 
SELECT id_site, season, protocol_type, o.opcod, date,year,surface,nb_sp,nb_ind,bassin_name, code, common_name,latin_name,length,biomass, nind, trend_dortel_2023, alien_dortel_2023
FROM sample as s, observation AS o, species AS sp
WHERE s.opcod = o.opcod AND o.species = sp.code AND  s.valid = TRUE ;
 

> >    id_site season protocol_type opcod       date year surface nb_sp nb_ind
1:    6268   fall      complete    83 2003-09-03 2003     420     6    218
2:    6268   fall      complete    83 2003-09-03 2003     420     6    218
3:    6268   fall      complete    83 2003-09-03 2003     420     6    218
4:    6268   fall      complete    83 2003-09-03 2003     420     6    218
5:    6268   fall      complete    83 2003-09-03 2003     420     6    218
6:    6268   fall      complete    83 2003-09-03 2003     420     6    218
       bassin_name code       common_name          latin_name    length
1: Seine-Normandie  EPT       Epinochette Pungitius pungitius  31.08696
2: Seine-Normandie  GAR            Gardon     Rutilus rutilus  91.50000
3: Seine-Normandie  GOU            Goujon         Gobio gobio  88.37500
4: Seine-Normandie  LOF     Loche franche Barbatula barbatula  67.93827
5: Seine-Normandie  TRF Truite de riviere  Salmo trutta fario 312.66667
6: Seine-Normandie  VAI            Vairon   Phoxinus phoxinus  27.00000
        biomass nind trend_dortel_2023 alien_dortel_2023
1:   10.0167634   23            Hausse             FALSE
2:   22.0710647    2            Baisse             FALSE
3:  232.5114261   24            Baisse             FALSE
4:  640.5753661  162            Baisse             FALSE
5: 2354.4223764    6              <NA>                NA
6:    0.2172862    1            Stable             FALSE
> > [1] 37369    18
> > . + > 
Requete distance:

 
SELECT id_site, bassin_name ,opcod, year, date,season,protocol_type
FROM sample as s
WHERE s.valid = TRUE ;
 

> > > > > . + > > > > > > . + > 
Requete distance:

 
SELECT code, common_name,latin_name, trend_dortel_2023, alien_dortel_2023
FROM species;
 

> > > > >     code                common_name                       latin_name
 1:  ABH             Able de Heckel            Leucaspius delineatus
 2:  ABL                    Ablette                Alburnus alburnus
 3:  ANG          Anguille d'Europe                Anguilla anguilla
 4:  ASP                       Aspe                    Aspius aspius
 5:  BAF         Barbeau fluviatile                    Barbus barbus
 6:  BAM         Barbeau méridional              Barbus meridionalis
 7:  BBG Black bass à grande bouche            Micropterus salmoides
 8:  BLE                    Blennie              Salaria fluviatilis
 9:  BLN                    Blageon                 Telestes souffia
10:  BOU                   Bouvière                   Rhodeus amarus
11:  BRB           Brème bordelière                  Blicca bjoerkna
12:  BRE              Brème commune                    Abramis brama
13:  BRO                    Brochet                      Esox lucius
14:  BRX         Brème indéterminée                          Abramis
15:  CAS                   Carassin              Carassius carassius
16:  CCO              Carpe commune                  Cyprinus carpio
17:  CHA                     Chabot                     Cottus gobio
18:  CHE                   Chevaine                Squalius cephalus
19:  EPI                   Epinoche Gasterosteus aculeatus aculeatus
20:  EPT                Epinochette              Pungitius pungitius
21:  GAM                   Gambusie                 Gambusia affinis
22:  GAR                     Gardon                  Rutilus rutilus
23:  GDL            Gobie demi-lune        Proterorhinus semilunaris
24:  GKS           Gobie de kessler               Ponticola kessleri
25:  GOL          Goujon de l’Adour                    Gobio lozanoi
26:  GOO        Goujon du Languedoc                 Gobio occitaniae
27:  GOU                     Goujon                      Gobio gobio
28:  GOX                   Gobio sp                            Gobio
29:  GRE                   Gremille            Gymnocephalus cernuus
30:  GTN        Gobie à tache noire           Neogobius melanostomus
31:  HOT                       Hotu               Chondrostoma nasus
32:  IDE               Ide melanote                   Leuciscus idus
33:  LOF              Loche franche              Barbatula barbatula
34:  LOR           Loche de rivière                   Cobitis taenia
35:  LOT            Lote de rivière                        Lota lota
36:  LPP         Lamproie de planer                 Lampetra planeri
37:  OBR               Ombre commun              Thymallus thymallus
38:  PAP             Epirine lippue               Pachychilon pictum
39:  PCH               Poisson chat                   Ameiurus melas
40:  PER                     Perche                Perca fluviatilis
41:  PES              Perche soleil                 Lepomis gibbosus
42:  PHX                phoxinus sp                         Phoxinus
43:  PSR              Pseudorasbora              Pseudorasbora parva
44:  ROT                   Rotengle      Scardinius erythrophthalmus
45:  SAN                     Sandre                Sander lucioperca
46:  SIL               Silure glane                   Silurus glanis
47:  SPI                    Spirlin          Alburnoides bipunctatus
48:  TAC         Truite arc-en-ciel              Oncorhynchus mykiss
49:  TAN                     Tanche                      Tinca tinca
50:  TOX                  Toxostome       Parachondrostoma toxostoma
51:  TRF          Truite de riviere               Salmo trutta fario
52:  VAB            Vairon béarnais                 Phoxinus bigerri
53:  VAI                     Vairon                Phoxinus phoxinus
54:  VAN                   Vandoise              Leuciscus leuciscus
55:  VAR           Vandoise rostrée          Leuciscus burdigalensis
    code                common_name                       latin_name
    trend_dortel_2023 alien_dortel_2023
 1:              <NA>                NA
 2:            Stable             FALSE
 3:            Baisse             FALSE
 4:            Hausse              TRUE
 5:            Stable             FALSE
 6:            Baisse             FALSE
 7:            Stable              TRUE
 8:            Hausse             FALSE
 9:            Stable             FALSE
10:            Hausse             FALSE
11:            Hausse             FALSE
12:            Baisse             FALSE
13:            Baisse             FALSE
14:              <NA>                NA
15:            Stable              TRUE
16:            Stable              TRUE
17:            Stable             FALSE
18:            Baisse             FALSE
19:            Stable             FALSE
20:            Hausse             FALSE
21:            Hausse              TRUE
22:            Baisse             FALSE
23:            Hausse              TRUE
24:              <NA>                NA
25:              <NA>                NA
26:            Baisse             FALSE
27:            Baisse             FALSE
28:              <NA>                NA
29:            Hausse             FALSE
30:            Hausse              TRUE
31:            Stable              TRUE
32:            Hausse             FALSE
33:            Baisse             FALSE
34:            Hausse             FALSE
35:            Baisse             FALSE
36:            Hausse             FALSE
37:            Stable             FALSE
38:            Hausse              TRUE
39:            Baisse              TRUE
40:            Baisse             FALSE
41:            Stable              TRUE
42:              <NA>                NA
43:            Hausse              TRUE
44:            Stable             FALSE
45:            Baisse              TRUE
46:            Hausse              TRUE
47:            Hausse             FALSE
48:            Hausse              TRUE
49:            Baisse             FALSE
50:            Baisse             FALSE
51:              <NA>                NA
52:            Stable             FALSE
53:            Stable             FALSE
54:            Baisse             FALSE
55:            Baisse             FALSE
    trend_dortel_2023 alien_dortel_2023
> > > > > > > head(dd)
   id_site     bassin_name opcod year       date season protocol_type code
1:    6268 Seine-Normandie    83 2003 2003-09-03   fall      complete  EPT
2:    3842 Seine-Normandie   588 2002 2002-09-29   fall      complete  EPT
3:    4830 Seine-Normandie  1298 2001 2001-05-29 spring      complete  EPT
4:    4830 Seine-Normandie  1316 2003 2003-06-11 spring      complete  EPT
5:    3842 Seine-Normandie  1421 2004 2004-09-09   fall      complete  EPT
6:    6268 Seine-Normandie  1442 2004 2004-08-31   fall      complete  EPT
      biomass abundance year_txt
1: 10.0167634        23     2003
2:  0.4770086         1     2002
3:  2.3826198         2     2001
4: 39.2125652        44     2003
5:  4.5333736         9     2004
6: 14.5472679        39     2004
> . + > 
Requete distance:

 
SELECT id_site, season, protocol_type, o.opcod, date,year,surface,nb_sp,nb_ind,bassin_name, code, common_name,latin_name,length,biomass, nind, trend_dortel_2023, alien_dortel_2023
FROM sample as s, observation AS o, species AS sp
WHERE s.opcod = o.opcod AND o.species = sp.code AND  s.valid = TRUE ;
 

> d
       id_site season protocol_type opcod       date year surface nb_sp nb_ind
    1:    6268   fall      complete    83 2003-09-03 2003     420     6    218
    2:    6268   fall      complete    83 2003-09-03 2003     420     6    218
    3:    6268   fall      complete    83 2003-09-03 2003     420     6    218
    4:    6268   fall      complete    83 2003-09-03 2003     420     6    218
    5:    6268   fall      complete    83 2003-09-03 2003     420     6    218
   ---                                                                        
37365:    2423   fall      complete 78426 2003-08-26 2003    1392    14   4101
37366:    2423   fall      complete 78426 2003-08-26 2003    1392    14   4101
37367:    2423   fall      complete 78426 2003-08-26 2003    1392    14   4101
37368:    2423   fall      complete 78426 2003-08-26 2003    1392    14   4101
37369:    2423   fall      complete 78426 2003-08-26 2003    1392    14   4101
           bassin_name code       common_name          latin_name    length
    1: Seine-Normandie  EPT       Epinochette Pungitius pungitius  31.08696
    2: Seine-Normandie  GAR            Gardon     Rutilus rutilus  91.50000
    3: Seine-Normandie  GOU            Goujon         Gobio gobio  88.37500
    4: Seine-Normandie  LOF     Loche franche Barbatula barbatula  67.93827
    5: Seine-Normandie  TRF Truite de riviere  Salmo trutta fario 312.66667
   ---                                                                     
37365: Seine-Normandie  OBR      Ombre commun Thymallus thymallus  88.33333
37366: Seine-Normandie  PER            Perche   Perca fluviatilis 168.50000
37367: Seine-Normandie  TRF Truite de riviere  Salmo trutta fario 158.88889
37368: Seine-Normandie  VAI            Vairon   Phoxinus phoxinus  36.45054
37369: Seine-Normandie  VAN          Vandoise Leuciscus leuciscus  59.18803
          biomass nind trend_dortel_2023 alien_dortel_2023
    1:   10.01676   23            Hausse             FALSE
    2:   22.07106    2            Baisse             FALSE
    3:  232.51143   24            Baisse             FALSE
    4:  640.57537  162            Baisse             FALSE
    5: 2354.42238    6              <NA>                NA
   ---                                                    
37365:   24.30291    3            Stable             FALSE
37366:  125.15721    2            Baisse             FALSE
37367: 1058.06876    9              <NA>                NA
37368: 1543.44059 2652            Stable             FALSE
37369: 1168.52823  117            Baisse             FALSE
> . + > 
Requete distance:

 
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
 

> d
       id_site season protocol_type opcod       date year surface nb_sp nb_ind
    1:     122   fall      complete  4833 2013-10-09 2013   500.5     4    519
    2:     122   fall      complete  4833 2013-10-09 2013   500.5     4    519
    3:     122   fall      complete  4833 2013-10-09 2013   500.5     4    519
    4:     122   fall      complete  4833 2013-10-09 2013   500.5     4    519
    5:     122   fall      complete  4833 2013-10-09 2013   500.5     4    519
   ---                                                                        
64295:   27190 spring      complete 38067 2001-07-18 2001   234.0     2    100
64296:   27190 spring      complete 38059 2016-06-30 2016   144.5     2    109
64297:   27190 spring      complete 38059 2016-06-30 2016   144.5     2    109
64298:   27190 spring      complete 38065 2002-06-14 2002   269.5     2    221
64299:   27190 spring      complete 38065 2002-06-14 2002   269.5     2    221
                bassin_name code        common_name          latin_name
    1:      Artois-Picardie  GOU             Goujon         Gobio gobio
    2:      Artois-Picardie  TAN             Tanche         Tinca tinca
    3:      Artois-Picardie  LOF      Loche franche Barbatula barbatula
    4:      Artois-Picardie  ANG  Anguille d'Europe   Anguilla anguilla
    5:      Artois-Picardie  TAC Truite arc-en-ciel Oncorhynchus mykiss
   ---                                                                 
64295: Rhône - Méditerranée  TRF  Truite de riviere  Salmo trutta fario
64296: Rhône - Méditerranée  CHA             Chabot        Cottus gobio
64297: Rhône - Méditerranée  TRF  Truite de riviere  Salmo trutta fario
64298: Rhône - Méditerranée  CHA             Chabot        Cottus gobio
64299: Rhône - Méditerranée  TRF  Truite de riviere  Salmo trutta fario
          length   biomass abondance trend_dortel_2023 alien_dortel_2023
    1:        NA    0.0000         0            Baisse             FALSE
    2:        NA    0.0000         0            Baisse             FALSE
    3: 106.50000  373.8854        26            Baisse             FALSE
    4:        NA    0.0000         0            Baisse             FALSE
    5: 300.00000  964.2248         3            Hausse              TRUE
   ---                                                                  
64295: 117.42857 2996.8545        98              <NA>                NA
64296:  76.94872  215.4071        39            Stable             FALSE
64297: 115.98571 4149.3901        70              <NA>                NA
64298:  71.42254  322.1847        71            Stable             FALSE
64299:  63.66000 1241.5622       150              <NA>                NA
> >      opcod           code                 nb   
 Min.   :   83   Length:64299       Min.   :1  
 1st Qu.: 8970   Class :character   1st Qu.:1  
 Median :16493   Mode  :character   Median :1  
 Mean   :19170                      Mean   :1  
 3rd Qu.:26494                      3rd Qu.:1  
 Max.   :78426                      Max.   :1  
> Erreur dans `[.data.table`(d, , log_surface = log(surface)) : 
  argument inutilisé (log_surface = log(surface))
> > Erreur : objet 'code' introuvable
> Erreur dans eval(predvars, data, env) : objet 'nind' introuvable
> Erreur : couldn't evaluate grouping factor bassin_name > id_site within model frame:error =Error in eval(substitute(makeFac(fac), list(fac = ff0)), frloc) : 
  objet 'bassin_name' introuvable
 Try adding grouping factor to data frame explicitly if possible
> > md
Formula:          abondance ~ year + protocol_type + season + (1 | id_site)
Data: d[code == "TAC", ]
 Offset: log_surface
      AIC       BIC    logLik  df.resid 
1759.2919 1789.8123 -873.6459      1190 
Random-effects (co)variances:

Conditional model:
 Groups  Name        Std.Dev.
 id_site (Intercept) 1.069   

Number of obs: 1196 / Conditional model: id_site, 89

Dispersion parameter for nbinom2 family (): 0.252 

Fixed Effects:

Conditional model:
         (Intercept)                  year  protocol_typepartial  
           -69.97375               0.03099              -0.12799  
        seasonspring  
            -0.28416  
> summary(md)
 Family: nbinom2  ( log )
Formula:          abondance ~ year + protocol_type + season + (1 | id_site)
Data: d[code == "TAC", ]
 Offset: log_surface

     AIC      BIC   logLik deviance df.resid 
  1759.3   1789.8   -873.6   1747.3     1190 

Random effects:

Conditional model:
 Groups  Name        Variance Std.Dev.
 id_site (Intercept) 1.143    1.069   
Number of obs: 1196, groups:  id_site, 89

Dispersion parameter for nbinom2 family (): 0.252 

Conditional model:
                      Estimate Std. Error z value Pr(>|z|)  
(Intercept)          -69.97375   32.20917  -2.172   0.0298 *
year                   0.03099    0.01607   1.928   0.0538 .
protocol_typepartial  -0.12799    0.74325  -0.172   0.8633  
seasonspring          -0.28416    0.28180  -1.008   0.3133  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
> > >  Family: nbinom2  ( log )
Formula:          abondance ~ year_txt + protocol_type + season + (1 | id_site)
Data: d[code == "TAC", ]
 Offset: log_surface

     AIC      BIC   logLik deviance df.resid 
  1786.7   1929.2   -865.4   1730.7     1168 

Random effects:

Conditional model:
 Groups  Name        Variance Std.Dev.
 id_site (Intercept) 1.201    1.096   
Number of obs: 1196, groups:  id_site, 89

Dispersion parameter for nbinom2 family (): 0.269 

Conditional model:
                      Estimate Std. Error z value Pr(>|z|)    
(Intercept)          -8.157030   0.473133 -17.240   <2e-16 ***
year_txt1996          0.000297   0.611636   0.000   0.9996    
year_txt1997          0.097351   0.589811   0.165   0.8689    
year_txt1998          0.332268   0.593021   0.560   0.5753    
year_txt1999          0.226570   0.574861   0.394   0.6935    
year_txt2000         -0.138143   0.590262  -0.234   0.8150    
year_txt2001          0.330530   0.574227   0.576   0.5649    
year_txt2002          0.177651   0.589143   0.302   0.7630    
year_txt2003         -0.083054   0.589021  -0.141   0.8879    
year_txt2004          0.525361   0.562630   0.934   0.3504    
year_txt2005          0.153714   0.580223   0.265   0.7911    
year_txt2006          0.430230   0.584495   0.736   0.4617    
year_txt2007         -0.357739   0.686219  -0.521   0.6021    
year_txt2008          0.212509   0.654569   0.325   0.7454    
year_txt2009         -0.284057   0.708158  -0.401   0.6883    
year_txt2010          0.429544   0.710426   0.605   0.5454    
year_txt2011          1.085159   0.629667   1.723   0.0848 .  
year_txt2012          0.961819   0.662126   1.453   0.1463    
year_txt2013          0.661602   0.633656   1.044   0.2964    
year_txt2014          1.153709   0.669936   1.722   0.0850 .  
year_txt2015          0.592468   0.726348   0.816   0.4147    
year_txt2016          1.048960   0.676187   1.551   0.1208    
year_txt2017          0.257679   0.700560   0.368   0.7130    
year_txt2018         -0.872969   0.864583  -1.010   0.3126    
protocol_typepartial -0.138190   0.753281  -0.183   0.8544    
seasonspring         -0.321335   0.289485  -1.110   0.2670    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
> >  Family: nbinom2  ( log )
Formula:          
abondance ~ year_txt + protocol_type + season + (1 | bassin_name)
Data: d[code == "TAC", ]
 Offset: log_surface

     AIC      BIC   logLik deviance df.resid 
  1840.0   1982.5   -892.0   1784.0     1168 

Random effects:

Conditional model:
 Groups      Name        Variance Std.Dev.
 bassin_name (Intercept) 0.4015   0.6336  
Number of obs: 1196, groups:  bassin_name, 6

Dispersion parameter for nbinom2 family (): 0.149 

Conditional model:
                     Estimate Std. Error z value Pr(>|z|)    
(Intercept)           -7.7197     0.5420 -14.244  < 2e-16 ***
year_txt1996           0.1474     0.6340   0.233  0.81610    
year_txt1997           0.1758     0.6243   0.282  0.77824    
year_txt1998           0.5097     0.6282   0.811  0.41721    
year_txt1999           0.6334     0.5957   1.063  0.28769    
year_txt2000           0.3547     0.6145   0.577  0.56386    
year_txt2001           0.5091     0.6021   0.846  0.39779    
year_txt2002           0.5940     0.6003   0.989  0.32243    
year_txt2003           0.3486     0.6074   0.574  0.56605    
year_txt2004           0.6446     0.5857   1.101  0.27104    
year_txt2005           0.3887     0.6007   0.647  0.51752    
year_txt2006           0.6272     0.6110   1.026  0.30469    
year_txt2007          -0.2152     0.6959  -0.309  0.75713    
year_txt2008           0.2230     0.6946   0.321  0.74818    
year_txt2009          -0.5807     0.7293  -0.796  0.42589    
year_txt2010           0.1177     0.7445   0.158  0.87433    
year_txt2011           1.0166     0.6659   1.527  0.12685    
year_txt2012           1.2487     0.6881   1.815  0.06957 .  
year_txt2013           0.6214     0.6743   0.922  0.35677    
year_txt2014           0.7935     0.6991   1.135  0.25636    
year_txt2015           2.1986     0.6688   3.288  0.00101 ** 
year_txt2016           1.5225     0.6825   2.231  0.02571 *  
year_txt2017           0.8620     0.7112   1.212  0.22551    
year_txt2018          -0.6979     0.8684  -0.804  0.42160    
protocol_typepartial  -0.4832     0.5701  -0.848  0.39668    
seasonspring          -0.5535     0.2266  -2.442  0.01459 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
> >  Family: nbinom2  ( log )
Formula:          
abondance ~ year_txt + protocol_type + season + (1 | bassin_name) +  
    (1 | id_site)
Data: d[code == "TAC", ]
 Offset: log_surface

     AIC      BIC   logLik deviance df.resid 
  1786.2   1933.7   -864.1   1728.2     1167 

Random effects:

Conditional model:
 Groups      Name        Variance Std.Dev.
 bassin_name (Intercept) 0.3356   0.5793  
 id_site     (Intercept) 1.0119   1.0059  
Number of obs: 1196, groups:  bassin_name, 6; id_site, 89

Dispersion parameter for nbinom2 family (): 0.269 

Conditional model:
                      Estimate Std. Error z value Pr(>|z|)    
(Intercept)          -7.960652   0.536528 -14.837   <2e-16 ***
year_txt1996          0.001223   0.608135   0.002   0.9984    
year_txt1997          0.071663   0.587529   0.122   0.9029    
year_txt1998          0.347446   0.590847   0.588   0.5565    
year_txt1999          0.254043   0.571176   0.445   0.6565    
year_txt2000         -0.100993   0.587621  -0.172   0.8635    
year_txt2001          0.325499   0.570903   0.570   0.5686    
year_txt2002          0.181781   0.585175   0.311   0.7561    
year_txt2003         -0.080163   0.585462  -0.137   0.8911    
year_txt2004          0.499789   0.559322   0.894   0.3716    
year_txt2005          0.132931   0.578772   0.230   0.8183    
year_txt2006          0.380927   0.584071   0.652   0.5143    
year_txt2007         -0.445458   0.688794  -0.647   0.5178    
year_txt2008          0.077857   0.664284   0.117   0.9067    
year_txt2009         -0.377967   0.710988  -0.532   0.5950    
year_txt2010          0.323030   0.716076   0.451   0.6519    
year_txt2011          1.075916   0.630127   1.707   0.0877 .  
year_txt2012          0.973892   0.659377   1.477   0.1397    
year_txt2013          0.544320   0.637980   0.853   0.3936    
year_txt2014          1.130522   0.668872   1.690   0.0910 .  
year_txt2015          0.632625   0.718791   0.880   0.3788    
year_txt2016          1.067856   0.675079   1.582   0.1137    
year_txt2017          0.282743   0.695841   0.406   0.6845    
year_txt2018         -0.877543   0.865409  -1.014   0.3106    
protocol_typepartial -0.264557   0.737301  -0.359   0.7197    
seasonspring         -0.294079   0.297682  -0.988   0.3232    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
> >  Family: nbinom2  ( log )
Formula:          
abondance ~ year_txt + protocol_type + season + (1 | bassin_name/id_site)
Data: d[code == "TAC", ]
 Offset: log_surface

     AIC      BIC   logLik deviance df.resid 
  1786.2   1933.7   -864.1   1728.2     1167 

Random effects:

Conditional model:
 Groups              Name        Variance Std.Dev.
 id_site:bassin_name (Intercept) 1.0119   1.0059  
 bassin_name         (Intercept) 0.3356   0.5793  
Number of obs: 1196, groups:  id_site:bassin_name, 89; bassin_name, 6

Dispersion parameter for nbinom2 family (): 0.269 

Conditional model:
                      Estimate Std. Error z value Pr(>|z|)    
(Intercept)          -7.960652   0.536528 -14.837   <2e-16 ***
year_txt1996          0.001223   0.608135   0.002   0.9984    
year_txt1997          0.071663   0.587529   0.122   0.9029    
year_txt1998          0.347446   0.590847   0.588   0.5565    
year_txt1999          0.254043   0.571176   0.445   0.6565    
year_txt2000         -0.100993   0.587621  -0.172   0.8635    
year_txt2001          0.325499   0.570903   0.570   0.5686    
year_txt2002          0.181781   0.585175   0.311   0.7561    
year_txt2003         -0.080163   0.585462  -0.137   0.8911    
year_txt2004          0.499789   0.559322   0.894   0.3716    
year_txt2005          0.132931   0.578772   0.230   0.8183    
year_txt2006          0.380927   0.584071   0.652   0.5143    
year_txt2007         -0.445458   0.688794  -0.647   0.5178    
year_txt2008          0.077857   0.664284   0.117   0.9067    
year_txt2009         -0.377967   0.710988  -0.532   0.5950    
year_txt2010          0.323030   0.716076   0.451   0.6519    
year_txt2011          1.075916   0.630127   1.707   0.0877 .  
year_txt2012          0.973892   0.659377   1.477   0.1397    
year_txt2013          0.544320   0.637980   0.853   0.3936    
year_txt2014          1.130522   0.668872   1.690   0.0910 .  
year_txt2015          0.632625   0.718791   0.880   0.3788    
year_txt2016          1.067856   0.675079   1.582   0.1137    
year_txt2017          0.282743   0.695841   0.406   0.6845    
year_txt2018         -0.877543   0.865409  -1.014   0.3106    
protocol_typepartial -0.264557   0.737301  -0.359   0.7197    
seasonspring         -0.294079   0.297682  -0.988   0.3232    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
> Erreur dans VIF(md) : impossible de trouver la fonction "VIF"
> Message d'avis :
Dans fitTMB(TMBStruc) :
  Model convergence problem; non-positive-definite Hessian matrix. See vignette('troubleshooting')
> Erreur dans model.frame.default(data = list(id_site = c(122L, 122L, 122L,  : 
  les longueurs des variables diffèrent (trouvé pour 'offset("log_surface")')
> > > Erreur dans model.frame.default(data = list(id_site = c(122L, 122L, 122L,  : 
  type (list) incorrect pour la variable 'offset(value_offset)'
> class(value_offset)
[1] "data.table" "data.frame"
> > class(value_offset)
[1] "list"
> head(value_offset)
*** output flushed ***
> dim(value_offset)
NULL
> > 
> class(value_offset)
[1] "numeric"
> Erreur dans model.frame.default(data = list(id_site = c(122L, 122L, 122L,  : 
  les longueurs des variables diffèrent (trouvé pour 'offset(value_offset)')
> > > > > Erreur dans setorderv(x, cols, order, na.last) : 
  some columns are not in the data.table: abondance
> Erreur dans setorderv(x, cols, order, na.last) : 
  some columns are not in the data.table: abondance,tot
> >     code abondance_tot occurence nb_sample prop_occurence
 1:  TRF        313562      3486      3996          0.872
 2:  LOF        250096      3129      3724          0.840
 3:  VAI        599544      3056      3822          0.800
 4:  GOU        304756      2800      3549          0.789
 5:  CHA        251814      2680      3220          0.832
 6:  CHE        165141      2641      3217          0.821
 7:  GAR        210153      2246      3235          0.694
 8:  ANG         41203      1955      2831          0.691
 9:  PER         23752      1471      2704          0.544
10:  ABL         98364      1226      1854          0.661
11:  VAN         31183      1122      2150          0.522
12:  PES         16888      1066      2199          0.485
13:  BAF         35689      1061      1530          0.693
14:  BRO          3647       873      1968          0.444
15:  BOU         44952       678      1122          0.604
16:  TAN          2533       637      1877          0.339
17:  ROT          4051       630      2149          0.293
18:  SPI         45470       557      1000          0.557
19:  BRE          7918       515      1480          0.348
20:  EPT          8545       512       951          0.538
21:  BRB         12584       456      1186          0.384
22:  CCO          1500       426      1683          0.253
23:  HOT          5057       402       775          0.519
24:  GRE          2366       370       895          0.413
25:  CAS          2189       348      1428          0.244
26:  EPI          6626       340       808          0.421
27:  SIL          1812       322       597          0.539
28:  PCH          4620       269       876          0.307
29:  PSR          7928       257       668          0.385
30:  BLN         19017       221       282          0.784
31:  TAC           558       216      1198          0.180
32:  SAN           636       196       713          0.275
33:  LOR          1076       142       299          0.475
34:  TOX          5382       139       302          0.460
35:  LOT          1602       121       224          0.540
36:  LPP          1209       114       588          0.194
37:  OBR           709       103       264          0.390
38:  BAM          1369        98       134          0.731
39:  ABH           592        91       623          0.146
40:  BBG           804        76       346          0.220
41:  GOX          2059        52       301          0.173
42:  VAR           387        51       372          0.137
43:  GAM           677        44       128          0.344
44:  PHX          3228        40       328          0.122
45:  PAP          1229        36        58          0.621
46:  ASP            88        28        69          0.406
47:  GOO           627        23       123          0.187
48:  IDE           144        19        68          0.279
49:  GOL          1256        15        94          0.160
50:  BLE           300        15        31          0.484
51:  GTN          4392        14        45          0.311
52:  GDL           335        13        62          0.210
53:  GKS           212        13        45          0.289
54:  BRX           102        10        88          0.114
55:  VAB           119         3        20          0.150
    code abondance_tot occurence nb_sample prop_occurence
> > > >     code abondance_tot occurence_tot nb_sample_tot prop_occurence_tot
 1:  TRF        313562          3486          3996              0.872
 2:  LOF        250096          3129          3724              0.840
 3:  VAI        599544          3056          3822              0.800
 4:  GOU        304756          2800          3549              0.789
 5:  CHA        251814          2680          3220              0.832
 6:  CHE        165141          2641          3217              0.821
 7:  GAR        210153          2246          3235              0.694
 8:  ANG         41203          1955          2831              0.691
 9:  PER         23752          1471          2704              0.544
10:  ABL         98364          1226          1854              0.661
11:  VAN         31183          1122          2150              0.522
12:  PES         16888          1066          2199              0.485
13:  BAF         35689          1061          1530              0.693
14:  BRO          3647           873          1968              0.444
15:  BOU         44952           678          1122              0.604
16:  TAN          2533           637          1877              0.339
17:  ROT          4051           630          2149              0.293
18:  SPI         45470           557          1000              0.557
19:  BRE          7918           515          1480              0.348
20:  EPT          8545           512           951              0.538
21:  BRB         12584           456          1186              0.384
22:  CCO          1500           426          1683              0.253
23:  HOT          5057           402           775              0.519
24:  GRE          2366           370           895              0.413
25:  CAS          2189           348          1428              0.244
26:  EPI          6626           340           808              0.421
27:  SIL          1812           322           597              0.539
28:  PCH          4620           269           876              0.307
29:  PSR          7928           257           668              0.385
30:  BLN         19017           221           282              0.784
31:  TAC           558           216          1198              0.180
32:  SAN           636           196           713              0.275
33:  LOR          1076           142           299              0.475
34:  TOX          5382           139           302              0.460
35:  LOT          1602           121           224              0.540
36:  LPP          1209           114           588              0.194
37:  OBR           709           103           264              0.390
38:  BAM          1369            98           134              0.731
39:  ABH           592            91           623              0.146
40:  BBG           804            76           346              0.220
41:  GOX          2059            52           301              0.173
42:  VAR           387            51           372              0.137
43:  GAM           677            44           128              0.344
44:  PHX          3228            40           328              0.122
45:  PAP          1229            36            58              0.621
46:  ASP            88            28            69              0.406
47:  GOO           627            23           123              0.187
48:  IDE           144            19            68              0.279
49:  GOL          1256            15            94              0.160
50:  BLE           300            15            31              0.484
51:  GTN          4392            14            45              0.311
52:  GDL           335            13            62              0.210
53:  GKS           212            13            45              0.289
54:  BRX           102            10            88              0.114
55:  VAB           119             3            20              0.150
    code abondance_tot occurence_tot nb_sample_tot prop_occurence_tot
> > > > >       code abondance_tot occurence_tot nb_sample_tot prop_occurence_tot year
   1:  TRF        313562          3486          3996              0.872 2002
   2:  LOF        250096          3129          3724              0.840 2002
   3:  TRF        313562          3486          3996              0.872 1999
   4:  TRF        313562          3486          3996              0.872 2005
   5:  VAI        599544          3056          3822              0.800 2002
  ---                                                                       
1294:  VAR           387            51           372              0.137 2008
1295:  VAR           387            51           372              0.137 2005
1296:  VAR           387            51           372              0.137 2006
1297:  VAR           387            51           372              0.137 2004
1298:  VAR           387            51           372              0.137 2003
      abondance_y occurence_y nb_sample_y prop_occurence_y
   1:       19420         199         224            0.888
   2:       20011         196         224            0.875
   3:       16789         194         225            0.862
   4:       18207         191         217            0.880
   5:       43236         190         224            0.848
  ---                                                     
1294:           0           1          18            0.056
1295:           0           1          16            0.062
1296:           0           1          17            0.059
1297:           0           1          17            0.059
1298:           0           1          15            0.067
> >       code abondance_tot occurence_tot nb_sample_tot prop_occurence_tot year
   1:  TRF        313562          3486          3996              0.872 2002
   2:  LOF        250096          3129          3724              0.840 2002
   3:  TRF        313562          3486          3996              0.872 1999
   4:  TRF        313562          3486          3996              0.872 2005
   5:  VAI        599544          3056          3822              0.800 2002
  ---                                                                       
1294:  GOO           627            23           123              0.187 2018
1295:  GOX          2059            52           301              0.173 2018
1296:  IDE           144            19            68              0.279 2018
1297:  PHX          3228            40           328              0.122 2018
1298:  VAB           119             3            20              0.150 2018
      abondance_y occurence_y nb_sample_y prop_occurence_y
   1:       19420         199         224            0.888
   2:       20011         196         224            0.875
   3:       16789         194         225            0.862
   4:       18207         191         217            0.880
   5:       43236         190         224            0.848
  ---                                                     
1294:           0           1           8            0.125
1295:           0           1          15            0.067
1296:           0           1           2            0.500
1297:           0           1          14            0.071
1298:           0           1           1            1.000
> >       code abondance_tot occurence_tot nb_sample_tot prop_occurence_tot year
   1:  TRF        313562          3486          3996              0.872 2002
   2:  LOF        250096          3129          3724              0.840 2002
   3:  TRF        313562          3486          3996              0.872 1999
   4:  TRF        313562          3486          3996              0.872 2005
   5:  VAI        599544          3056          3822              0.800 2002
  ---                                                                       
1294:  GOO           627            23           123              0.187 2018
1295:  GOX          2059            52           301              0.173 2018
1296:  IDE           144            19            68              0.279 2018
1297:  PHX          3228            40           328              0.122 2018
1298:  VAB           119             3            20              0.150 2018
      abondance_y occurence_y nb_sample_y prop_occurence_y
   1:       19420         199         224            0.888
   2:       20011         196         224            0.875
   3:       16789         194         225            0.862
   4:       18207         191         217            0.880
   5:       43236         190         224            0.848
  ---                                                     
1294:           0           1           8            0.125
1295:           0           1          15            0.067
1296:           0           1           2            0.500
1297:           0           1          14            0.071
1298:           0           1           1            1.000
> >       code year abondance_y occurence_y nb_sample_y
   1:  GOU 2013        4080          77         113
   2:  TAN 2013          80          17          67
   3:  LOF 2013        3946          87         112
   4:  ANG 2013        1793          69          92
   5:  TAC 2013          26          10          41
  ---                                              
1294:  BAM 2013          29           7           7
1295:  BAM 2008          31           6           6
1296:  BAM 2010          13           4           4
1297:  BAM 2011          62           7           6
1298:  BAM 2007          49           6           6
> >       code year abondance_y occurence_y nb_sample_y
   1:  ABH 1995           3           3          27
   2:  ABH 1996           6           5          26
   3:  ABH 1997          11           3          31
   4:  ABH 1998          10           7          25
   5:  ABH 1999          55           8          34
  ---                                              
1294:  VAR 2014           7           4          16
1295:  VAR 2015          67          11          18
1296:  VAR 2016          22           8          17
1297:  VAR 2017          20           7          16
1298:  VAR 2018          39           6          14
> > >       code abondance_tot occurence_tot nb_sample_tot prop_occurence_tot year
   1:  ABH           592            91           623              0.146 1995
   2:  ABH           592            91           623              0.146 1996
   3:  ABH           592            91           623              0.146 1997
   4:  ABH           592            91           623              0.146 1998
   5:  ABH           592            91           623              0.146 1999
  ---                                                                       
1294:  VAR           387            51           372              0.137 2014
1295:  VAR           387            51           372              0.137 2015
1296:  VAR           387            51           372              0.137 2016
1297:  VAR           387            51           372              0.137 2017
1298:  VAR           387            51           372              0.137 2018
      abondance_y occurence_y nb_sample_y prop_occurence_y
   1:           3           3          27            0.111
   2:           6           5          26            0.192
   3:          11           3          31            0.097
   4:          10           7          25            0.280
   5:          55           8          34            0.235
  ---                                                     
1294:           7           4          16            0.250
1295:          67          11          18            0.611
1296:          22           8          17            0.471
1297:          20           7          16            0.438
1298:          39           6          14            0.429
> > > > >       code year abondance_y occurence_y nb_sample_y prop_occurence_y
   1:  ABH 1995           3           3          27            0.111
   2:  ABH 1996           6           5          26            0.192
   3:  ABH 1997          11           3          31            0.097
   4:  ABH 1998          10           7          25            0.280
   5:  ABH 1999          55           8          34            0.235
  ---                                                               
1294:  VAR 2014           7           4          16            0.250
1295:  VAR 2015          67          11          18            0.611
1296:  VAR 2016          22           8          17            0.471
1297:  VAR 2017          20           7          16            0.438
1298:  VAR 2018          39           6          14            0.429
      abondance_tot occurence_tot nb_sample_tot prop_occurence_tot
   1:           592            91           623              0.146
   2:           592            91           623              0.146
   3:           592            91           623              0.146
   4:           592            91           623              0.146
   5:           592            91           623              0.146
  ---                                                             
1294:           387            51           372              0.137
1295:           387            51           372              0.137
1296:           387            51           372              0.137
1297:           387            51           372              0.137
1298:           387            51           372              0.137
> >       code year abondance_y occurence_y nb_sample_y prop_occurence_y
   1:  TRF 2002       19420         199         224            0.888
   2:  LOF 2002       20011         196         224            0.875
   3:  TRF 1999       16789         194         225            0.862
   4:  TRF 2005       18207         191         217            0.880
   5:  VAI 2002       43236         190         224            0.848
  ---                                                               
1294:  VAR 2005           0           1          16            0.062
1295:  VAR 2006           0           1          17            0.059
1296:  VAR 2007           0           1          16            0.062
1297:  VAR 2008           0           1          18            0.056
1298:  VAR 2010           0           1          19            0.053
      abondance_tot occurence_tot nb_sample_tot prop_occurence_tot
   1:        313562          3486          3996              0.872
   2:        250096          3129          3724              0.840
   3:        313562          3486          3996              0.872
   4:        313562          3486          3996              0.872
   5:        599544          3056          3822              0.800
  ---                                                             
1294:           387            51           372              0.137
1295:           387            51           372              0.137
1296:           387            51           372              0.137
1297:           387            51           372              0.137
1298:           387            51           372              0.137
> >       code year abondance_y occurence_y nb_sample_y prop_occurence_y
   1:  TRF 1995       13141         165         188            0.878
   2:  TRF 1996       14190         167         185            0.903
   3:  TRF 1997       18002         181         203            0.892
   4:  TRF 1998       17222         178         200            0.890
   5:  TRF 1999       16789         194         225            0.862
  ---                                                               
1294:  VAB 2013          88           2           1            2.000
1295:  VAB 2014          31           2           1            2.000
1296:  VAB 2016           0           1           1            1.000
1297:  VAB 2017           0           1           1            1.000
1298:  VAB 2018           0           1           1            1.000
      abondance_tot occurence_tot nb_sample_tot prop_occurence_tot
   1:        313562          3486          3996              0.872
   2:        313562          3486          3996              0.872
   3:        313562          3486          3996              0.872
   4:        313562          3486          3996              0.872
   5:        313562          3486          3996              0.872
  ---                                                             
1294:           119             3            20              0.150
1295:           119             3            20              0.150
1296:           119             3            20              0.150
1297:           119             3            20              0.150
1298:           119             3            20              0.150
> >       code year abondance_y occurence_y nb_sample_y prop_occurence_y
   1:  TRF 1995       13141         165         188            0.878
   2:  TRF 1996       14190         167         185            0.903
   3:  TRF 1997       18002         181         203            0.892
   4:  TRF 1998       17222         178         200            0.890
   5:  TRF 1999       16789         194         225            0.862
  ---                                                               
1294:  VAB 2013          88           2           1            2.000
1295:  VAB 2014          31           2           1            2.000
1296:  VAB 2016           0           1           1            1.000
1297:  VAB 2017           0           1           1            1.000
1298:  VAB 2018           0           1           1            1.000
      abondance_tot occurence_tot nb_sample_tot prop_occurence_tot
   1:        313562          3486          3996              0.872
   2:        313562          3486          3996              0.872
   3:        313562          3486          3996              0.872
   4:        313562          3486          3996              0.872
   5:        313562          3486          3996              0.872
  ---                                                             
1294:           119             3            20              0.150
1295:           119             3            20              0.150
1296:           119             3            20              0.150
1297:           119             3            20              0.150
1298:           119             3            20              0.150
> > vecsp
 [1] "TRF" "LOF" "VAI" "GOU" "CHA" "CHE" "GAR" "ANG" "PER" "ABL" "VAN" "PES"
[13] "BAF" "BRO" "BOU" "TAN" "ROT" "SPI" "BRE" "EPT" "BRB" "CCO" "HOT" "GRE"
[25] "CAS" "EPI" "SIL" "PCH" "PSR" "BLN" "TAC" "SAN" "LOR" "TOX" "LOT" "LPP"
[37] "OBR" "BAM" "ABH" "BBG" "GOX" "VAR" "GAM" "PHX" "PAP" "ASP" "GOO" "IDE"
[49] "GOL" "BLE" "GTN" "GDL" "GKS" "BRX" "VAB"
> > > > > > > > . + 

 ( 1 / 55 )  TRFError in eval(predvars, data, env) : objet 'abundance' introuvable


 ( 2 / 55 )  LOFError in eval(predvars, data, env) : objet 'abundance' introuvable
De plus : Messages d'avis :
1: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
2: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
3: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'


 ( 3 / 55 )  VAIError in eval(predvars, data, env) : objet 'abundance' introuvable
De plus : Messages d'avis :
1: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
2: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
3: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'


 ( 4 / 55 )  GOUError in eval(predvars, data, env) : objet 'abundance' introuvable
De plus : Messages d'avis :
1: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
2: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
3: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'


 ( 5 / 55 )  CHAError in eval(predvars, data, env) : objet 'abundance' introuvable
De plus : Messages d'avis :
1: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
2: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
3: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'


 ( 6 / 55 )  CHEError in eval(predvars, data, env) : objet 'abundance' introuvable
De plus : Messages d'avis :
1: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
2: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
3: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'


 ( 7 / 55 )  GARError in eval(predvars, data, env) : objet 'abundance' introuvable
De plus : Messages d'avis :
1: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
2: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
3: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'


 ( 8 / 55 )  ANGError in eval(predvars, data, env) : objet 'abundance' introuvable
De plus : Messages d'avis :
1: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
2: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
3: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'


 ( 9 / 55 )  PERError in eval(predvars, data, env) : objet 'abundance' introuvable
De plus : Messages d'avis :
1: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
2: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
3: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'


 ( 10 / 55 )  ABLError in eval(predvars, data, env) : objet 'abundance' introuvable
De plus : Messages d'avis :
1: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
2: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
3: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'


 ( 11 / 55 )  VANError in eval(predvars, data, env) : objet 'abundance' introuvable
De plus : Messages d'avis :
1: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
2: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
3: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'


 ( 12 / 55 )  PESError in eval(predvars, data, env) : objet 'abundance' introuvable
De plus : Messages d'avis :
1: Dans fitTMB(TMBStruc) :
  Model convergence problem; non-positive-definite Hessian matrix. See vignette('troubleshooting')
2: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
3: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
4: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'


 ( 13 / 55 )  BAFError in eval(predvars, data, env) : objet 'abundance' introuvable
De plus : Messages d'avis :
1: Dans fitTMB(TMBStruc) :
  Model convergence problem; non-positive-definite Hessian matrix. See vignette('troubleshooting')
2: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
3: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
4: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'


 ( 14 / 55 )  BROError in eval(predvars, data, env) : objet 'abundance' introuvable
De plus : Messages d'avis :
1: Dans fitTMB(TMBStruc) :
  Model convergence problem; non-positive-definite Hessian matrix. See vignette('troubleshooting')
2: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
3: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
4: Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
  C-c C-c
Message d'avis :
Dans checkTerms(data.tmb1$terms, data.tmb0$terms) :
  Predicting new random effect levels for terms: 1 | id_site:bassin_name
Disable this warning with 'allow.new.levels=TRUE'
> d
       id_site season protocol_type opcod       date year surface nb_sp nb_ind
    1:     122   fall      complete  4833 2013-10-09 2013   500.5     4    519
    2:     122   fall      complete  4833 2013-10-09 2013   500.5     4    519
    3:     122   fall      complete  4833 2013-10-09 2013   500.5     4    519
    4:     122   fall      complete  4833 2013-10-09 2013   500.5     4    519
    5:     122   fall      complete  4833 2013-10-09 2013   500.5     4    519
   ---                                                                        
64295:   27190 spring      complete 38067 2001-07-18 2001   234.0     2    100
64296:   27190 spring      complete 38059 2016-06-30 2016   144.5     2    109
64297:   27190 spring      complete 38059 2016-06-30 2016   144.5     2    109
64298:   27190 spring      complete 38065 2002-06-14 2002   269.5     2    221
64299:   27190 spring      complete 38065 2002-06-14 2002   269.5     2    221
                bassin_name code        common_name          latin_name
    1:      Artois-Picardie  GOU             Goujon         Gobio gobio
    2:      Artois-Picardie  TAN             Tanche         Tinca tinca
    3:      Artois-Picardie  LOF      Loche franche Barbatula barbatula
    4:      Artois-Picardie  ANG  Anguille d'Europe   Anguilla anguilla
    5:      Artois-Picardie  TAC Truite arc-en-ciel Oncorhynchus mykiss
   ---                                                                 
64295: Rhône - Méditerranée  TRF  Truite de riviere  Salmo trutta fario
64296: Rhône - Méditerranée  CHA             Chabot        Cottus gobio
64297: Rhône - Méditerranée  TRF  Truite de riviere  Salmo trutta fario
64298: Rhône - Méditerranée  CHA             Chabot        Cottus gobio
64299: Rhône - Méditerranée  TRF  Truite de riviere  Salmo trutta fario
          length   biomass abondance trend_dortel_2023 alien_dortel_2023
    1:        NA    0.0000         0            Baisse             FALSE
    2:        NA    0.0000         0            Baisse             FALSE
    3: 106.50000  373.8854        26            Baisse             FALSE
    4:        NA    0.0000         0            Baisse             FALSE
    5: 300.00000  964.2248         3            Hausse              TRUE
   ---                                                                  
64295: 117.42857 2996.8545        98              <NA>                NA
64296:  76.94872  215.4071        39            Stable             FALSE
64297: 115.98571 4149.3901        70              <NA>                NA
64298:  71.42254  322.1847        71            Stable             FALSE
64299:  63.66000 1241.5622       150              <NA>                NA
       log_surface year_txt
    1:    6.215608     2013
    2:    6.215608     2013
    3:    6.215608     2013
    4:    6.215608     2013
    5:    6.215608     2013
   ---                     
64295:    5.455321     2001
64296:    4.973280     2016
64297:    4.973280     2016
64298:    5.596568     2002
64299:    5.596568     2002
> . + > 
Requete distance:

 
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
SELECT s.id_site, season, protocol_type, s.opcod, date,year,surface,nb_sp,nb_ind,bassin_name, code, common_name,latin_name,length,biomass, abondance AS abundance, trend_dortel_2023, alien_dortel_2023
FROM sp_abs sps
NATURAL JOIN sample s
JOIN species sp ON sps.species = sp.code;
 

> 
Process R exited abnormally with code 1 at Fri Oct 20 11:38:46 2023
