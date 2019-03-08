#==================================================================================================
#Project Name: COOK INLET CHINOOK ENVIRONMENTAL EFFECTS - Plot Manuscript Figures
#Creator: Curry James Cunningham, NOAA/NMFS, ABL
#Date: 2.26.2019
#
#Purpose: To plot final figures for the manuscript
#
# 1) 
#
#
#==================================================================================================
#NOTES:
#
#==================================================================================================

require(ggplot2)
require(R2jags)
require(reshape2)
require(mcmcplots)
require(xlsx)
require(BEST)
require(dplyr)
require(tidyverse)
require(viridis)
require(bayesplot)
require(tidybayes)
require(brms)
require(brmstools)
require(corrplot)
require(cowplot)

#Define Workflow Paths ====================================================
# *Assumes you are working from the Sergent_Streamflow R project
wd <- file.path(getwd(),"JAGS")
# setwd(wd)
dir.output <- file.path(wd,"Output")
dir.figs <- file.path(wd,"Plots")
dir.data <- file.path(wd,"Data","10.25.18_update")


#CONTROL SECTION ==========================================================
#############################
phases <- c(3,4) #3 4 5
n.phases <- length(phases)

#Update Fig and Output directories
# dir.output <- file.path(dir.output,"Final_New_LinearRicker",paste0("Phase_",phase))
# dir.create(dir.output, recursive=TRUE)
dir.figs <- file.path(dir.figs,"Final_New_LinearRicker")
# dir.create(dir.figs, recursive=TRUE)

# Load Data ================================================================

out.3 <- readRDS(file=file.path(dir.output,"Final_New_LinearRicker",paste0("Phase_",3),"out.rds"))
out.4 <- readRDS(file=file.path(dir.output,"Final_New_LinearRicker",paste0("Phase_",4),"out.rds"))

# Get Covariate Names ================================================================

names.covars.3 <- c('wksGT13','wksGT15', 
                  'maxP_spawn','RB_spawn','RB_emerge',
                  'avgP_grow','medianQ',
                  'breakup','NPGO')

names.covars.4 <- c('maxT_spawn','avgT_grow', 
                  'maxP_spawn','RB_spawn','RB_emerge',
                  'avgP_grow','medianQ',
                  'breakup','NPGO')

# Plot Group Means ================================================================
mu.list <- cbind(out.3$BUGSoutput$sims.list$mu.coef, out.4$BUGSoutput$sims.list$mu.coef[,1:2])
names.covars.all <- c(names.covars.3, names.covars.4[1:2])
names(mu.list) <- names.covars.all
n.plot <- length(names.covars.all)

#PLOT IT
png(file.path(dir.figs, "MuCoef-plotPost.png"), height=6, width=8, res=500, units='in')

par(mfcol=c(3,4), mar=c(5,1,1,0), oma=c(1,1,3,1))
c <- 1
for(c in 1:n.plot) {
  if(c %in% c(10:11)) {
    plotPost(mu.list[,c], showCurve=FALSE, main='', xlab=names.covars.all[c],
             xlim=c(-0.25,0.25), rope=0, col=rgb(0,0.5,0, alpha=0.5))
    abline(v=0, lty=1, lwd=2, col=rgb(1,0,0,alpha=0.5))
  }else {
    plotPost(mu.list[,c], showCurve=FALSE, main='', xlab=names.covars.all[c],
             xlim=c(-0.25,0.25), rope=0, col=rgb(0,0,1, alpha=0.5))
    abline(v=0, lty=1, lwd=2, col=rgb(1,0,0,alpha=0.5))
  }


}
mtext(paste0('Group-level Mean Effects'), side=3, outer=TRUE, font=2, line=1)

dev.off()

# Caterpillar Plots ==============================================================


#Get Population Names
pops <- readRDS(file.path(dir.output,"Final_New_LinearRicker",paste0("Phase_",3),"pops.rds"))
n.pops <- length(pops)

#Get original dimensions
dims.out <- dim(out.3$BUGSoutput$sims.list$coef)

#
covar.list <- array(dim=c(dims.out[1], dims.out[2], dims.out[3]+2),
                      dimnames=list(c(1:dims.out[1]), pops, names.covars.all))  

#Fill in...
covar.list[,,1:dims.out[3]] <- out.3$BUGSoutput$sims.list$coef #Phase 3 Model

get.ref <- c(dims.out[3]-1,dims.out[3]) #Last two covariates 8-9
assign.ref <- c(dims.out[3]+1,dims.out[3]+2) #Location to put them in combined data object: 10-11

covar.list[,,assign.ref] <- out.4$BUGSoutput$sims.list$coef[,,get.ref] #Phase 3 Model

#PLOT IT
png(file.path(dir.figs, "Covar Effects-Caterplot.png"), height=7, width=10, res=500, units='in')

par(mfcol=c(3,4), mar=c(2,5,3,1), oma=c(2,2,1,1))
c <- 1
for(c in 1:n.plot) {
  caterplot(covar.list[,,c],
            labels=pops, reorder=FALSE, quantiles=list(0.025,0.25,0.75,0.975), style='gray', col='blue')
  mtext(names.covars.all[c], side=3, outer=FALSE, line=1)
  caterpoints(apply(covar.list[,,c],2,median), reorder=FALSE, pch=21, col='red', bg='orange')
  abline(v=0, lty=1, lwd=2, col=rgb(1,0,0, alpha=0.5))
}
mtext('Coefficient (Effect)', side=1, outer=TRUE, font=2, line=0.5)
mtext('Population', side=2, outer=TRUE, font=2, line=0.5)

dev.off()

# Plot Group Means as Overlaid Histograms from Each model ==========================
# Benefit - Shows consistence in other effects. 

#Extract Effects
eff.list.3 <- data.frame(out.3$BUGSoutput$sims.list$mu.coef)
names(eff.list.3) <- 
                 
#Add Phase 3 Effects

#Add Phase 4 Effects.





