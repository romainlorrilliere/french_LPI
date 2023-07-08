################################################################
#### script to produce temporal series of population dynamics
################################################################




## parameters

## mean of initial abundance
N0_mean  <-  20

## variance of initial abundance
N0_var <- 50


## mean of replacement rate
r_mean <- 0.01

## initial variance of replacement rate
r_init_var <- 0.01

## temporal variance of replacement rate
r_temporal_var <- 0.01

## number of population
nb_pop <- 100


## number of years of the temporal series
nb_year <- 50




pops_N0 <- round(rnorm(nb_pop,N0_mean,N0_var))
pops_N0[pops_N0 < 0]  <- 0

pops_N <- array(NA,dim=c(nb_year,nb_pop))


pops_r <- rnorm(nb_pop,r_mean,r_init_var)


pops_N[1,] <- pops_N0


##pops_N[2,] <- rpois(nb_pop,pops_N[1,]+ pops_N[1,] * pops_r)
##pops_N


dyn_pop <- function(t,pops_r,rvar,i=2) {
      t[i,] <- ifelse(t[i-1,] >0, rpois(ncol(t),t[i-1,]+ t[i-1,] * rnorm(ncol(t),pops_r,rvar)),0)
     if(i == nrow(t)) return(t) else t  <-  dyn_pop(t,pops_r,rvar,i+1)
}


pops_N <- dyn_pop(pops_N,pops_r,rvar = r_temporal_var)




library(reshape2)
library(data.table)
library(ggplot2)

d_pops <- reshape2::melt(pops_N)
setDT(d_pops)
colnames(d_pops) <- c("year","pop","N")


gg <- ggplot(data = d_pops[year < 20,],mapping=aes(x=year,y=N,colour=pop,group=pop)) + geom_point() + geom_line()
gg

