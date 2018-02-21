#************************************************************************************************
#Project Name: COOK INLET CHINOOK ENVIRONMENTAL EFFECTS - Dynamic Factor Analysis for Stream Flow
#Creator: Curry James Cunningham, CFOS, University of Alaska, Fairbanks
#Date: 6.25.17
#
#Purpose: Given that flow metrics are not available for all streams, but common trends emerge, lets do a DFA to identify common trend and use that as covariate. 
#
#
#*************************************************************************************************
#NOTES:
#
#*************************************************************************************************

require(MARSS)
require(rstan)

#Set Working Directory
wd <- '/Users/curryc2/Documents/2016/Shoen Chinook/JAGS'
setwd(wd)

dat <- t(scale(temp.dat.2))
fit <- fit_dfa(y=dat, num_trends=2, chains=2, iter=1e4)