library(rlpi)
library(data.table)
library(ggplot2)

file.copy(from=system.file("extdata", "example_data.zip", package = "rlpi"), to=getwd())

unzip("example_data.zip")

data_mammalia <- fread("example_data/T_Nearctic_mammalia_pops.txt")
head(data_mammalia)

data_aves <- fread("example_data/T_Nearctic_aves_pops.txt")
head(data_aves)

data_herps <- fread("example_data/T_Nearctic_herps_pops.txt")
head(data_herps)


# Make a Nearctic LPI

# Default gives 100 bootstraps (this takes a couple of minutes to run on a 2014 MacBook)
Nearc_lpi <- LPIMain("example_data/T_Nearctic_aves_pops.txt", use_weightings = 1, VERBOSE=FALSE)

# Remove NAs (trailing years with no data)
Nearc_lpi <- Nearc_lpi[complete.cases(Nearc_lpi), ]
# This produces a simple plot, but we can use ggplot_lpi to produce a nicer version
ggplot_lpi(Nearc_lpi, ylims=c(0, 2))

Nearc_mams_lpi <- LPIMain("example_data/T_Nearctic_mammalia_infile.txt", VERBOSE=FALSE)


lpi2016 <- fread("example_data/LPI_LPR2016data_public.csv")
dim(lpi2016)
dim(lpi2016[Country == "France",])

dim(lpi2016[Region == "Paleartic",])

lpi2016[,.(n = .N),by = .(Region)]





lpi2022 <- fread("data/LivingPlanetIndex_2022_PublicData/LPD2022_public.csv")
dim(lpi2022)
dim(lpi2022[Country == "France",])

lpi2022f <- lpi2022[Country == "France",]

lpi2022f_c <- lpi2022f[,.(Npop = .N), by = .(Class)]

v_poisson  <-  c("Elasmobranchii","Petromyzonti","Actinopteri")
lpi2022f_c[,Groupe := ifelse(Class %in% v_poisson,"Poisson",Class)]

lpi2022f_c[Groupe == "Aves",Groupe := "Oiseaux"]
lpi2022f_c[Groupe == "Reptilia",Groupe := "Reptiles"]
lpi2022f_c[Groupe == "Amphibia",Groupe := "Amphibiens"]

gg <- ggplot(lpi2022f_c, aes(x="",y = Npop, fill = Groupe))
gg <- gg + geom_bar(stat="identity", width=1) +  coord_polar("y", start=0)
gg <- gg + labs(y="Nombre d'espèces",fill="Groupe")
gg


lpi2022f[,.(Npop = .N), by = .(Class,Common_name)]

lpi2022[,.(n = .N),by = .(Region)]



source("functions/fun_data_preparation.r")
library(data.table)
library(rlpi)
library(ggplot2)


d <- fread("data/data_FrenchBBS_carre_trend_Benoit_2022-09-14_trend_carre_2001_2019fr.csv")

d44 <- d[departement == 44,]
fbbs2lpi(d44,file.write="data/data_stoc_2019_44_LPI_pops.txt")

lpi_stoc_44 <-  LPIMain("data/data_stoc_2019_44_LPI_infile.txt", VERBOSE=TRUE, REF_YEAR = 2001)
lpi_stoc_44$year  <-  as.numeric(row.names(lpi_stoc_44))
setDT(lpi_stoc_44)
gg  <-  ggplot(data = lpi_stoc_44, aes(x=year,y=LPI_final)) + geom_line()+ geom_pointrange(aes(ymin=CI_low, ymax=CI_high), shape=20)
gg <- gg + geom_ribbon(aes(ymin=CI_low, ymax=CI_high),alpha=0.2)
gg
