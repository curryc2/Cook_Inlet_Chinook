#==================================================================================================
#Project Name: COOK INLET CHINOOK ENVIRONMENTAL EFFECTS - Plot Manuscript Figures
#Creator: Curry James Cunningham, NOAA/NMFS, ABL
#Date: 2.26.2019
#Modified by Erik Schoen, UAF on 3.26.2019
#
#Purpose: To plot final figures for the manuscript
#
#
#==================================================================================================
#NOTES:
#
#==================================================================================================

require(ggplot2)
require(ggthemes)
require(ggridges)
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
# *Assumes you are working from the Cook Inlet Chinook R project
wd <- file.path(getwd(),"JAGS")
# setwd(wd)
dir.output <- file.path(wd,"Output")
dir.figs <- file.path(wd,"Plots")
dir.data <- file.path(wd,"Data","03.26.19_update")


#CONTROL SECTION ==========================================================
#############################
phases <- c(3,4) #3 4 5
n.phases <- length(phases)

#Update Fig and Output directories
# dir.output <- file.path(dir.output,"Final_New_LinearRicker",paste0("Phase_",phase))
# dir.create(dir.output, recursive=TRUE)
dir.figs <- file.path(dir.figs,"Final_New_LinearRicker_ES")
# dir.create(dir.figs, recursive=TRUE)

# Load Data ================================================================

out.3 <- readRDS(file=file.path(dir.output,"Final_New_LinearRicker_ES",paste0("Phase_",3),"out.rds"))
out.4 <- readRDS(file=file.path(dir.output,"Final_New_LinearRicker_ES",paste0("Phase_",4),"out.rds"))

# Get Covariate Names ================================================================

#### ORIGINAL CODE #######################
# names.covars.3 <- c('wksGT13','wksGT15', 
#                   'maxP_spawn','RB_spawn','RB_emerge',
#                   'avgP_grow','medianQ',
#                   'breakup','NPGO')
# 
# names.covars.4 <- c('maxT_spawn','avgT_grow', 
#                   'maxP_spawn','RB_spawn','RB_emerge',
#                   'avgP_grow','medianQ',
#                   'breakup','NPGO')
###########################################

names.covars.3 <- c('wksGT13_spawn','wksGT15_grow', 
                    'maxP_spawn','RB_spawn','RB_emerge',
                    'avgP_grow','medianQ_grow',
                    'breakup','NPGO')

names.covars.4 <- c('maxT_spawn','avgT_grow', 
                    'maxP_spawn','RB_spawn','RB_emerge',
                    'avgP_grow','medianQ_grow',
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
# pops <- readRDS(file.path(dir.output,"Final_New_LinearRicker_ES",paste0("Phase_",3),"pops.rds"))
# Set population names for pretty labels
pops <- c("Alexander", "Anchor", "Campbell", "Chuitna", "Chulitna", "Crooked", "Deep",
               "Deshka", "Kenai late run", "Little Susitna", "Little Willow", "Montana", "Ninilchik",
               "Theodore", "Willow")
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

par(mfcol=c(3,4), mar=c(2,5,3,1), oma=c(3,3,1,1))
c <- 1
for(c in 1:n.plot) {
  caterplot(covar.list[,,c],
            labels=pops, reorder=FALSE, quantiles=list(0.025,0.25,0.75,0.975), 
            style='gray', col='blue', cex = 0.9)
  mtext(names.covars.all[c], side=3, outer=FALSE, line=1)
  caterpoints(apply(covar.list[,,c],2,median), reorder=FALSE, pch=21, col='red', bg='orange')
  abline(v=0, lty=1, lwd=2, col=rgb(1,0,0, alpha=0.5))
}
mtext('Effect', side=1, outer=TRUE, font=2, line=1, cex = 1.5)
mtext('Population', side=2, outer=TRUE, font=2, line=1, cex = 1.5)

dev.off()

# Plot Group Means as Overlaid Histograms from Each model ==========================
# Benefit - Shows consistence in other effects. 

#Extract Effects
eff.df.3 <- data.frame(out.3$BUGSoutput$sims.list$mu.coef)
names(eff.df.3) <- names.covars.3
eff.list.3 <- eff.df.3 %>% melt()
eff.list.3$Model <- "Model 1"

eff.df.4 <- data.frame(out.4$BUGSoutput$sims.list$mu.coef)
names(eff.df.4) <- names.covars.4
eff.list.4 <- eff.df.4 %>% melt()
eff.list.4$Model <- "Model 2"

#Combine
eff.list.comb <- rbind(eff.list.3, eff.list.4)
eff.list.comb$title <- "Regional Mean Effect Across Populations"

#Prder Covariate factor
eff.list.comb$variable <- factor(eff.list.comb$variable,
                                   levels=c('wksGT13_spawn','wksGT15_grow', 
                                            'maxT_spawn','avgT_grow',
                                            'maxP_spawn','avgP_grow',
                                            'RB_spawn','RB_emerge',
                                            'medianQ_grow',
                                            'breakup','NPGO'))




#Plot It Out
g <- ggplot(eff.list.comb, aes(value, group=Model, fill=Model)) +
       theme_linedraw() +
       # scale_fill_gdocs() +
       geom_vline(xintercept=0, col='red') +
       geom_density(alpha=0.25) + 
       facet_wrap(~variable) +
       
       # xlab('Covariate') +
       xlab('Coefficient Value') +
       ylab('Relative Probability Density') +
       theme(legend.position='top',
             axis.text.y=element_blank())
ggsave(file.path(dir.figs,'2-Model Mu Posteriors.png'), plot=g,
         height=6, width=7, dpi=500, units='in')

#Necessary functions to calculate caterplots
q.50 <- function(x) { return(quantile(x, probs=c(0.25,0.75))) }
q.95 <- function(x) { return(quantile(x, probs=c(0.025,0.975))) }
q_0.025 <- function(x) { return(quantile(x, probs=0.025)) }
q_0.975 <- function(x) { return(quantile(x, probs=0.975)) }

#Boxplots
g <- ggplot(eff.list.comb, aes(x=variable, y=value, fill=Model)) +
  theme_linedraw() +
  # scale_fill_gdocs() +
  geom_hline(yintercept=0, col='red') +
  # geom_violin(alpha=0.25) +
  geom_boxplot(outlier.shape=NA, alpha=0.5) +
  # geom_violin(alpha = 0.5, lwd=0.1, scale='width') +
  
  # stat_summary(fun.y="q.95", colour="black", geom="line", lwd=0.5) +
  # stat_summary(fun.y="q.50", colour="black", geom="line", lwd=1.25) +
  # stat_summary(fun.y="median", colour="black", size=1.75, geom="point", pch=21) +

  # geom_density_ridges2() +

  xlab('Covariate') +
  ylab('Effect') +
  theme(legend.position='top') +
  coord_flip(ylim=c(-0.25,0.25)) +
  scale_x_discrete(limits=rev(levels(eff.list.comb$variable)))+
  facet_wrap(~title)
  # coord_cartesian(xlim=c(-0.3,0.3))
g
ggsave(file.path(dir.figs,'2-Model Mu Boxplots.png'), plot=g,
       height=5, width=5, dpi=500, units='in')

#Violin plots
g <- ggplot(eff.list.comb, aes(x=variable, y=value, fill=Model)) +
  theme_linedraw() +
  # scale_fill_gdocs() +
  geom_hline(yintercept=0, col='red') +
  geom_violin(alpha=0.25) +
  xlab('Covariate') +
  ylab('Coefficient Value') +
  theme(legend.position='top') +
  coord_flip(ylim=c(-0.25,0.25)) +
  facet_wrap(~title)
# coord_cartesian(xlim=c(-0.3,0.3))
g




