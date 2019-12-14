#==================================================================================================
#Project Name: COOK INLET CHINOOK ENVIRONMENTAL EFFECTS - Plot Manuscript Figures
#Creator: Curry James Cunningham, NOAA/NMFS, ABL
#Date: 2.26.2019
#Modified by Erik Schoen, UAF on 3.26.2019
#
#Purpose: To plot final figures for the manuscript
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


# Load Data ================================================================

out <- readRDS(file=file.path(dir.output,model.version,"out.rds"))

# Set Covariate Names ================================================================

names.covars <- c('maxT_spawn','avgT_rear', 
                    'maxP_spawn','avgP_rear',
                    'medianQ_rear','RB_spawn','RB_emerge',
                    'breakup','NPGO')

# Plot Group Means ================================================================
mu.list <- cbind(out$BUGSoutput$sims.list$mu.coef)
names(mu.list) <- names.covars
n.plot <- length(names.covars)

#PLOT IT

# Histograms
# png(file.path(dir.figs, "Regional mean effects_histograms.png"), height=6, width=8, res=500, units='in')
pdf(file.path(dir.figs, "Regional mean effects_histograms.pdf"), height=6, width=8)

par(mfrow=c(3,3), mar=c(5,1,1,0), oma=c(1,1,3,1))
c <- 1
for(c in 1:n.plot) {
    plotPost(mu.list[,c], showCurve=FALSE, main='', xlab=names.covars[c],
             xlim=c(-0.25,0.25), rope=0, col=rgb(0,0,1, alpha=0.5))
    abline(v=0, lty=1, lwd=2, col=rgb(1,0,0,alpha=0.5))
}
mtext(paste0('Regional Mean Effects'), side=3, outer=TRUE, font=2, line=1)

dev.off()

# Caterpillar Plot

pdf(file.path(dir.figs, "Fig 3_Regional mean effects_caterpillar.pdf"), height=5, width=7)

par(mfrow=c(1,1), mar=c(2,7,1,1), oma=c(3,3,0,0))
caterplot(mu.list,
          labels=names.covars, reorder=FALSE, quantiles=list(0.025,0.25,0.75,0.975),
          style='gray', col='blue', cex = 1.1)
caterpoints(apply(mu.list,2,median), pch=21, col='red', bg='orange')
abline(v=0, lty=1, lwd=2, col=rgb(1,0,0, alpha=0.5))
mtext('Effect', side=1, outer=TRUE, font=2, line=1, cex = 1.3)
mtext('Covariate', side=2, outer=TRUE, font=2, line=1, cex = 1.3)

dev.off()

png(file.path(dir.figs, "Fig 3_Regional mean effects_caterpillar.png"), height=5, width=7, res=500, units='in')
par(mfrow=c(1,1), mar=c(2,7,1,1), oma=c(3,3,0,0))
caterplot(mu.list,
          labels=names.covars, reorder=FALSE, quantiles=list(0.025,0.25,0.75,0.975),
          style='gray', col='blue', cex = 1.1)
caterpoints(apply(mu.list,2,median), pch=21, col='red', bg='orange')
abline(v=0, lty=1, lwd=2, col=rgb(1,0,0, alpha=0.5))
mtext('Effect', side=1, outer=TRUE, font=2, line=1, cex = 1.3)
mtext('Covariate', side=2, outer=TRUE, font=2, line=1, cex = 1.3)

dev.off()


# Population-specific effects==============================================================
# Set population names for plots
pops <- c("Alexander", "Anchor", "Campbell", "Chuitna", "Chulitna", "Crooked", "Deep",
               "Deshka", "Kenai late run", "Little Susitna", "Little Willow", "Montana", "Ninilchik",
               "Theodore", "Willow")
n.pops <- length(pops)

#Get original dimensions
dims.out <- dim(out$BUGSoutput$sims.list$coef)

#Make array to hold results
covar.list <- array(dim=c(dims.out[1], dims.out[2], dims.out[3]),
                      dimnames=list(c(1:dims.out[1]), pops, names.covars))  

#Fill in the array with covariate posterior values

covar.list <- out$BUGSoutput$sims.list$coef

#PLOT IT
png(file.path(dir.figs, "Fig 4_Population-specific Effects.png"), height=8, width=10, res=500, units='in')
par(mfrow=c(3,3), mar=c(2,7,3,1), oma=c(3,3,1,1))
c <- 1
for(c in 1:n.plot) {
  caterplot(covar.list[,,c],
            labels=pops, reorder=FALSE, quantiles=list(0.025,0.25,0.75,0.975), 
            style='gray', col='blue', cex = 1.1, val.lim = c(-0.45, 0.4))
  mtext(names.covars[c], side=3, outer=FALSE, line=1)
  caterpoints(apply(covar.list[,,c],2,median), reorder=FALSE, pch=21, col='red', bg='orange')
  abline(v=0, lty=1, lwd=2, col=rgb(1,0,0, alpha=0.5))
}
mtext('Effect', side=1, outer=TRUE, font=2, line=1, cex = 1.3)
mtext('Population', side=2, outer=TRUE, font=2, line=1, cex = 1.3)

dev.off()


pdf(file.path(dir.figs, "Fig 4_Population-specific Effects.pdf"), height=8, width=10)

par(mfrow=c(3,3), mar=c(2,7,3,1), oma=c(3,3,1,1))
c <- 1
for(c in 1:n.plot) {
  caterplot(covar.list[,,c],
            labels=pops, reorder=FALSE, quantiles=list(0.025,0.25,0.75,0.975), 
            style='gray', col='blue', cex = 1.1, val.lim = c(-0.45, 0.4))
  mtext(names.covars[c], side=3, outer=FALSE, line=1)
  caterpoints(apply(covar.list[,,c],2,median), reorder=FALSE, pch=21, col='red', bg='orange')
  abline(v=0, lty=1, lwd=2, col=rgb(1,0,0, alpha=0.5))
}
mtext('Effect', side=1, outer=TRUE, font=2, line=1, cex = 1.3)
mtext('Population', side=2, outer=TRUE, font=2, line=1, cex = 1.3)

dev.off()

# # Same plot but with free x-axes (shows more detail for each indicator, but overall patterns
# # are less clear at a glance)
# png(file.path(dir.figs, "Population-specific Effects_free x-axes.png"), height=7, width=10, res=500, units='in')
# 
# par(mfrow=c(3,3), mar=c(2,5,3,1), oma=c(3,3,1,1))
# c <- 1
# for(c in 1:n.plot) {
#   caterplot(covar.list[,,c],
#             labels=pops, reorder=FALSE, quantiles=list(0.025,0.25,0.75,0.975), 
#             style='gray', col='blue', cex = 0.9)
#   mtext(names.covars[c], side=3, outer=FALSE, line=1)
#   caterpoints(apply(covar.list[,,c],2,median), reorder=FALSE, pch=21, col='red', bg='orange')
#   abline(v=0, lty=1, lwd=2, col=rgb(1,0,0, alpha=0.5))
# }
# mtext('Effect', side=1, outer=TRUE, font=2, line=1, cex = 1.5)
# mtext('Population', side=2, outer=TRUE, font=2, line=1, cex = 1.5)
# 
# dev.off()

# Simplified version of population-specific effects for graphical abstract #################
# Just show the top 3 regional effects, plus temp and NPGO
# Replace covariate name abbrevs with meaningful labels

# Filter down to the 6 covariates of most interest
covar.list.simple <- covar.list[,,c(3, 4, 5, 1, 2, 9)]

names.covars.simple <- c('Max. precip. (spawning/incubation)',
                         'Mean precip. (rearing)',
                         'Median discharge (rearing)',
                         'Max. temp. (spawning/incubation)',
                         'Mean temp. (rearing)', 
                         'NPGO (smolt/early marine)')

n.plot.simple <- length(names.covars.simple)

#PLOT IT
pdf(file.path(dir.figs, "Graphical abstract.pdf"), height=8, width=10)
par(mfrow=c(2,3), mar=c(2,2,3,2), oma=c(3,10,2,2))
c <- 1
for(c in 1:n.plot.simple) {
  caterplot(covar.list.simple[,,c],
            labels=pops, reorder=FALSE, quantiles=list(0.025,0.25,0.75,0.975), 
            style='gray', col='blue', cex = 1.1, val.lim = c(-0.45, 0.4))
  mtext(names.covars.simple[c], side=3, outer=FALSE, line=1)
  caterpoints(apply(covar.list.simple[,,c],2,median), reorder=FALSE, pch=21, col='red', bg='orange')
  abline(v=0, lty=1, lwd=2, col=rgb(1,0,0, alpha=0.5))
}
mtext('Covariate effect', side=1, outer=TRUE, font=2, line=1, cex = 1.3)
mtext('Chinook salmon population', side=2, outer=TRUE, font=2, line=6, cex = 1.3)

dev.off()


