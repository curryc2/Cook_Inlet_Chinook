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

# Source Useful Functions ==================================================
q.50 <- function(x) { return(quantile(x, probs=c(0.25,0.75))) }
q.95 <- function(x) { return(quantile(x, probs=c(0.025,0.975))) }
q_0.025 <- function(x) { return(quantile(x, probs=0.025)) }
q_0.975 <- function(x) { return(quantile(x, probs=0.975)) }

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

# PLOT: Group Mean Caterpillar Plots ==============================================

temp.list <- data.frame(out$BUGSoutput$sims.list$mu.coef)
names(temp.list) <- names.covars
temp.list.2 <- melt(temp.list)



g <- temp.list.2 %>% 
       ggplot(aes(x=variable, y=value)) +
       theme_bw() +
       geom_hline(yintercept = 0, colour='red')+
       stat_summary(fun.y="q.95", colour="darkblue", geom="line", lwd=0.5) +
       stat_summary(fun.y="q.50", colour="blue", geom="line", lwd=1.25) +
       stat_summary(fun.y="median", colour="yellow", size=1.75, geom="point", pch=19) +
       stat_summary(fun.y="median", colour="red", size=1.75, geom="point", pch=21) +
       scale_x_discrete(limits=rev(levels(temp.list.2$variable))) +
       coord_flip() + theme(axis.title=element_blank()) +
       ggtitle("Group Mean Effects")
       
g
ggsave(file=file.path(dir.figs,"New_Group Mean Effects.pdf"), plot=g,
         height=5, width=5, units='in')


# PLOT: Stock-specific Caterpillar Plots ==========================================
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
dimnames(covar.list) <- list(c(1:dims.out[1]), , )
# 

#PLOT IT
png(file.path(dir.figs, "Population-specific Effects.png"), height=8, width=10, res=500, units='in')
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


pdf(file.path(dir.figs, "Population-specific Effects.pdf"), height=8, width=10)

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