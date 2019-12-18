#==================================================================================================
#Project Name: COOK INLET CHINOOK ENVIRONMENTAL EFFECTS - Plot Manuscript Figures
#Creator: Curry James Cunningham, College of Fisheries and Ocean Science (CFOS), UAF
#Date: 12.17.2019
#
#Purpose: To text for autocorrelation in model residuals
#

require(ggplot2)
require(ggthemes)
require(ggridges)
require(R2jags)
require(reshape2)
require(mcmcplots)
require(BEST)
require(dplyr)
require(tidyverse)
require(viridis)
require(bayesplot)
require(tidybayes)
require(brms)
require(corrplot)
require(cowplot)

#Define Workflow Paths ====================================================
# *Assumes you are working from the Cook Inlet Chinook R project
wd <- file.path(getwd(),"JAGS")
# setwd(wd)
dir.output <- file.path(wd,"Output")
dir.figs <- file.path(wd,"Plots")
model.version <- "LinearRicker_04.11.19"
dir.data <- file.path(wd,"Data","04.09.19_update")


# Load Data ================================================================

out <- readRDS(file=file.path(dir.output,model.version,"out.rds"))

# Extract Observed Data =====================================================
# #Read in Spawner-Recruit and Covariate Data =============================================
# All SR and covariate data, indexed to brood year and standardized
dat <- read_csv(file.path(dir.data,"covarsSR.csv")) 

#Create summary table
summary.dat <- dat %>% group_by(Population,EscapementMethod) %>% summarize('startBY'=min(BroodYear), 
                                                                           'endBY'=max(BroodYear), 
                                                                           'n.year'=length(unique(BroodYear))) %>% 
  arrange(Population)

#Create Inputs for JAGS ===================================================
pops <- sort(unique(dat$Population))
n.pops <- length(pops)

#Save names of populations ====================================

#Number of brood years for each population
n.years <- vector(length=n.pops)
years <- matrix(nrow=n.pops,ncol=50)

p <- 1
for(p in 1:n.pops) {
  n.years[p] <- length(unique(dat$BroodYear[dat$Population==pops[p]]))
  years[p,1:n.years[p]] <- sort(unique(dat$BroodYear[dat$Population==pops[p]]))
}#next i

# Spawners and Recruits
spawn <- matrix(nrow=n.pops,ncol=max(n.years))
rec <- matrix(nrow=n.pops,ncol=max(n.years))
ln.rec <- matrix(nrow=n.pops,ncol=max(n.years))

p <- 1
for(p in 1:n.pops) {
  y <- 1
  for(y in 1:n.years[p]) {
    year <- years[p,y]
    
    #Spawners
    spawn[p,y] <- dat$Spawners[dat$Population==pops[p] & dat$BroodYear==year]
    #Recruits
    rec[p,y] <- dat$CoreRecruits[dat$Population==pops[p] & dat$BroodYear==year]
    ln.rec[p,y] <- log(dat$CoreRecruits[dat$Population==pops[p] & dat$BroodYear==year])
  }#next y
}#next p

# Calculate Residuals from Posterior Mean =============================================
# Likelihood observations are ln.rec
ln.rec

n.years

resids <- matrix(nrow=n.pops,ncol=max(n.years)) #Residuals for log(recruitment)
lag1.ar <- vector(length=n.pops)

pdf(file=file.path(dir.figs,paste(model.version, "_Residual Autocorr.pdf")), height=9, width=7)
# Loop through pops
par(mfrow=c(5,3), mar=c(2,2,4,0), oma=c(2,2,1,1))
p <- 1
for(p in 1:n.pops) {
  #Calculate posterior mean
  ln.pred.mean <- apply(log(out$BUGSoutput$sims.list$pred.rec[,p,]), c(2), mean)[1:n.years[p]]
  
  # Calculate residuals
  resids[p,1:n.years[p]] <- (ln.pred.mean - ln.rec[p,1:n.years[p]])
  
  plot(x=ln.rec[p,1:n.years[p]], y=ln.pred.mean, type='p', pch=19, col=rgb(1,0,0, alpha=0.5),
         main=pops[p])
  abline(a=0, b=1)
  
  # Calculate autocorrelation
  temp.acf <- acf(resids[p,1:n.years[p]], plot=FALSE)
  lag1.ar[p] <- as.vector(temp.acf$acf)[2]
  legend('topleft', legend=paste("AR-1:", round(lag1.ar[p],2)))
  
  
  

}
dev.off()

