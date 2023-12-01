library(data.table)
library(sf)
library(ggplot)

load("data/fish_metro/op_analysis.rda")
setDT(op_analysis)
head(op_analysis)


load("data/fish_metro/station_analysis.rda")
station <- station_analysis
setDT(station)
head(station)


gg <- ggplot(data = station
