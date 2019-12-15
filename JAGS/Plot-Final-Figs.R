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
require(RColorBrewer)
require(forcats)

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

# CONTROL SECTION ==========================================================
plot.Fig3 <- FALSE

plot.Fig4 <- TRUE

# Load Data ================================================================

out <- readRDS(file=file.path(dir.output,model.version,"out.rds"))

# Set Covariate Names ================================================================

names.covars <- c('maxT_spawn','avgT_rear', 
                  'maxP_spawn','avgP_rear',
                  'medianQ_rear','RB_spawn','RB_emerge',
                  'breakup','NPGO')

covar.scale.lookup <- data.frame(names.covars,
                                 c(rep("Local",5),
                                   rep("Regional",3),
                                   "Broad"))
names(covar.scale.lookup) <- c("variable","Covariate_Scale")
covar.scale.lookup$color <-  c(rep("#66c2a5",5),
                               rep("#fc8d62",3),
                               "#8da0cb")

# Plot Group Means ================================================================
mu.list <- cbind(out$BUGSoutput$sims.list$mu.coef)
names(mu.list) <- names.covars
n.plot <- length(names.covars)

# PLOT: Group Mean Caterpillar Plots ==============================================

# Plotting Terms
fig.height <- 4.25; fig.width <- 5.25

# Data
temp.list <- data.frame(out$BUGSoutput$sims.list$mu.coef)
names(temp.list) <- names.covars
temp.list.2 <- melt(temp.list)
temp.list.3 <- temp.list.2 %>% left_join(covar.scale.lookup)



g <- temp.list.3 %>% 
       ggplot(aes(x=variable, y=value)) +
       theme_linedraw() +
       geom_hline(yintercept = 0, colour='red')+
       stat_summary(fun.y="q.95", colour="darkblue", geom="line", lwd=0.5) +
       stat_summary(fun.y="q.50", colour="blue", geom="line", lwd=1.25) +
       stat_summary(fun.y="median", colour="yellow", size=1.75, geom="point", pch=19) +
       stat_summary(fun.y="median", colour="red", size=1.75, geom="point", pch=21) +
       scale_x_discrete(limits=rev(levels(temp.list.2$variable))) +
       coord_flip() +# theme(axis.title=element_blank())
       # ggtitle("Group Mean Effects")
       xlab("Covariate") + ylab("Group Mean Effect")
       
g

if(plot.Fig3==TRUE) {
ggsave(file=file.path(dir.figs,"New Fig 3.1.pdf"), plot=g,
         height=fig.height, width=fig.width, units='in')
}

# Add Covariate definitions
temp.cols <- c("#66c2a5", "#fc8d62", "#8da0cb")


g.2 <- g + annotate("rect", xmin=4.5, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.2, fill=temp.cols[1]) +
           annotate("text", x = 7, y = -0.2, label = "Local Scale", angle=90) +
           annotate("rect", xmin=1.5, xmax=4.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill=temp.cols[2]) +
           annotate("text", x = 3, y = -0.2, label = "Regional Scale", angle=90) +
           annotate("rect", xmin=-Inf, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill=temp.cols[3]) +
           annotate("text", x = 1, y = -0.2, label = "Broad", angle=90)
           
g.2 

if(plot.Fig3==TRUE) {
ggsave(file=file.path(dir.figs,"New Fig 3.2 Shaded Scales.pdf"), plot=g.2,
       height=fig.height, width=fig.width, units='in')
}

# Covariate definition horizontal
g.3 <- g + annotate("rect", xmin=4.5, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.2, fill=temp.cols[1]) +
  annotate("label", x = 9.3, y = -0.15, label = "Local Scale") +
  annotate("rect", xmin=1.5, xmax=4.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill=temp.cols[2]) +
  annotate("label", x = 4.3, y = -0.15, label = "Regional Scale") +
  annotate("rect", xmin=-Inf, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill=temp.cols[3]) +
  annotate("label", x = 1.3, y = -0.15, label = "Broad")

g.3 

if(plot.Fig3==TRUE) {
ggsave(file=file.path(dir.figs,"New Fig 3.3 Shaded Scales Horizontal Text.pdf"), plot=g.3,
       height=fig.height, width=fig.width, units='in')

}

# Covariate definition horizontal - Right Justified
g.4 <- g + annotate("rect", xmin=4.5, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.2, fill=temp.cols[1]) +
  annotate("label", x = 7, y = 0.1, label = "Local\nScale") +
  annotate("rect", xmin=1.5, xmax=4.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill=temp.cols[2]) +
  annotate("label", x = 3, y = 0.1, label = "Regional\nScale") +
  annotate("rect", xmin=-Inf, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill=temp.cols[3]) +
  annotate("label", x = 1, y = 0.1, label = "Broad")

g.4

if(plot.Fig3==TRUE) {
ggsave(file=file.path(dir.figs,"New Fig 3.4 Shaded Scales Horizontal Text - Right Justified.pdf"), plot=g.4,
       height=fig.height, width=fig.width, units='in')
}


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

# Convert to a true list
dimnames(covar.list) <- list(c(1:dims.out[1]), pops, names.covars)
#
covar.list.2 <- data.frame(melt(covar.list))

# Rename columns in new list
head(covar.list.2)
names(covar.list.2) <- c("samp","stock","variable","value")

# add in scales
covar.list.3 <- covar.list.2 %>% left_join(covar.scale.lookup)

str(covar.list.3)

# Re-order covariate factor level
# covar.list.4 <- covar.list.3 %>% fct_relevel("variable", names.covars)

covar.list.3$variable <- fct_relevel(covar.list.3$variable, names.covars)

# PLOT: NEW FIGURE 4 ===========================================

# Plotting Terms
fig.height <- 6.5; fig.width <- 7.5

fig4 <- covar.list.3 %>% 
          ggplot(aes(x=stock, y=value)) +
            theme_linedraw() +
            geom_hline(yintercept = 0, colour='red')+
            stat_summary(fun.y="q.95", colour="darkblue", geom="line", lwd=0.5) +
            stat_summary(fun.y="q.50", colour="blue", geom="line", lwd=1.25) +
            stat_summary(fun.y="median", colour="yellow", size=1.75, geom="point", pch=19) +
            stat_summary(fun.y="median", colour="red", size=1.75, geom="point", pch=21) +
            scale_x_discrete(limits=rev(levels(covar.list.3$stock))) +
            facet_wrap(~variable) +
            coord_flip() +
            xlab("Population") + ylab("Effect") +
            theme(axis.title=element_text(face="bold"))

fig4

ggsave(file.path(dir.figs,"New Fig 4.1.pdf"), plot=fig4, 
         height=fig.height, width=fig.width, units="in")

# Remove scale expansion on effect axis
fig4.2 <- fig4 + scale_y_continuous(expand=c(0.01,0.01))

ggsave(file.path(dir.figs,"New Fig 4.2.pdf"), plot=fig4.2, 
       height=fig.height, width=fig.width, units="in")


# Free effect axes
fig4.3  <- fig4.2 + facet_wrap(~variable, scales="free_x")


ggsave(file.path(dir.figs,"New Fig 4.3 - Free xAxis.pdf"), plot=fig4.3, 
       height=fig.height, width=fig.width, units="in")

# Color background based on scale of covariate
# fig4.4 <- fig4.2 + geom_rect(aes(fill = Covariate_Scale),xmin = -Inf,xmax = Inf,
#                              ymin = -Inf,ymax = Inf, alpha = 0.2) 
temp.fills <- vector(length=0)

for(i in 1:nrow(covar.scale.lookup)) {
  temp.fills <- append(temp.fills, rep(covar.scale.lookup$color[i], 9))
}



# fig4.4 <- fig4.2 + annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.2, fill=covar.list.3$color)
                     # scale_fill_colorblind()
                     


# fig4.4 <- fig4.2 + theme(strip.background =element_rect(fill=covar.scale.lookup$color))

# fig4.4

# ggsave(file.path(dir.figs,"New Fig 4.4 - Colored Backgrounds.pdf"), plot=fig4.4,
       # height=fig.height, width=fig.width, units="in")


