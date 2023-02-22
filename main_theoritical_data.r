################################################################
#### script to produce temporal series of population dynamics
################################################################




## parameters

## mean of initial abundance
N0_mean  <-  10

## variance of initial abundance
N0_var <- 2


## mean of replacement rate
r_mean <- 0.01

## initial variance of replacement rate
r_init_var <- 0.01

## temporal variance of replacement rate
r_temporal_var <- 0.01

## number of population
nb_pop <- 3


## number of years of the temporal series
nb_year <- 100




pops_N0 <- round(rnorm(nb_pop,N0_mean,N0_var))
pops_N0[pops_N0 < 0]  <- 0

tpops <- array(NA,dim=c(nb_year,nb_pop))
tpops

rpops <- rnorm(nb_pop,r_mean,r_init_var)
rpops

tpops[1,] <- pops_N0
tpops

##tpops[2,] <- rpois(nb_pop,tpops[1,]+ tpops[1,] * rpops)
##tpops


dyn_pop <- function(t,rpops,rvar,i=2) {
      t[i,] <- ifelse(t[i-1,] >0, rpois(ncol(t),t[i-1,]+ t[i-1,] * rnorm(ncol(t),rpops,rvar)),0)
     if(i == nrow(t)) return(t) else t  <-  dyn_pop(t,rpops,rvar,i+1)
}


tpops <- dyn_pop(tpops,rpops,rvar = r_temporal_var)




library(reshape2)
library(data.table)
library(ggplot2)

d_pops <- melt(tpops)
setDT(d_pops)
colnames(d_pops) <- c("year","pop","N")

d_pops[,pop := as.character(pop)]
gg <- ggplot(data = d_pops,mapping=aes(x=year,y=N,colour=pop,group=pop)) + geom_point() + geom_line()
gg

