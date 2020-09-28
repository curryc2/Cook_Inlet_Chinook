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
wd <- file.path(getwd())
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


# Figure 3 and 4 alternatives: R Shaftel ----------------------------------------------------------------------

#Figures 3 and 4 took a long time to run on my machine with the stat_summary inside
# ggplot so I summarized the data and removed the large data frames.

temp.list.3.summ <- temp.list.3 %>% 
  group_by(variable) %>% 
  summarize(q.025 = quantile(value, probs = 0.025),
            q.975 = quantile(value, probs = 0.975),
            q.25 = quantile(value, probs = 0.25),
            q.75 = quantile(value, probs = 0.75),
            median = quantile(value, probs = 0.5))

temp.list.3.summ$variable <- fct_relevel(temp.list.3.summ$variable, names.covars)


g1 <- ggplot(data = temp.list.3.summ) + 
  geom_hline(yintercept = 0, colour='red') +
  geom_segment(aes(x = reorder(variable, desc(variable)), xend = variable, y = q.025, yend = q.975), 
               color = "darkblue", lwd = 0.5) +
  geom_segment(aes(x = variable, xend = variable, y = q.25, yend = q.75), 
               color = "blue", lwd = 1.25) +
  geom_point(data = temp.list.3.summ,
             aes(x = variable, y = median), color = "red", shape = 16, size = 2) +
  coord_flip() +
  theme_bw() +
  annotate("rect", xmin=-Inf, xmax=5.5, ymin=-Inf, ymax=Inf, alpha=0.3, fill='lightgreen') +
  labs(y = "Group mean effect", x = "Covariate") +
  theme(text = element_text(size = 15))

g1

# save as 3.5.
ggsave("JAGS/Plots/New Figure 3.5.png", plot = g1, width = 6, height = 5, units = "in")
ggsave("JAGS/Plots/New Figure 3.5.pdf", plot = g1, width = 6, height = 5, units = "in")

#Black and white version of figure 3 for Oncorhynchus article. 9/21/20.
#Also rename covariates on y axis.

g1 <- temp.list.3.summ %>% 
  mutate(variable2 = case_when(variable == "maxT_spawn" ~ "Max. Temp. Spawning",
                               variable == "avgT_rear" ~ "Avg. Temp. Rearing",
                               variable == "maxP_spawn" ~ "Max. Precip. Spawning",
                               variable == "avgP_rear" ~ "Avg. Precip. Rearing",
                               variable == "medianQ_rear" ~ "Median Streamflow Rearing",
                               variable == "RB_spawn" ~ "Flashy Streamflow Spawning",
                               variable == "RB_emerge" ~ "Flashy Streamflow Emergence",
                               variable == "breakup" ~ "River Breakup Timing",
                               variable == "NPGO" ~ "North Pacific Gyre Oscillation")) %>% 
  ggplot() + 
  geom_hline(yintercept = 0, colour='black') +
  geom_segment(aes(x = reorder(variable2, desc(variable2)), xend = variable2, y = q.025, yend = q.975), 
               color = "black", lwd = 0.5) +
  geom_segment(aes(x = variable2, xend = variable2, y = q.25, yend = q.75), 
               color = "black", lwd = 1.25) +
  geom_point(aes(x = variable2, y = median), color = "black", shape = 16, size = 2) +
  coord_flip() +
  theme_bw() +
  annotate("rect", xmin=-Inf, xmax=5.5, ymin=-Inf, ymax=Inf, alpha=0.3, fill='lightgray') +
  labs(y = "Group mean effect") +
  theme(text = element_text(size = 15), axis.title.y = element_blank())

g1

# save as 3.6.
ggsave("JAGS/Plots/New Figure 3.6.png", plot = g1, width = 6, height = 5, units = "in")
ggsave("JAGS/Plots/New Figure 3.6.pdf", plot = g1, width = 6, height = 5, units = "in")

# For figure 4, create each plot and then arrange using grid. I can't find another
# way to do different background colors.

library(gridExtra)
library(grid)

covar.list.3.summ <- covar.list.3 %>% 
  group_by(Covariate_Scale, stock, variable) %>% 
  summarize(q.025 = quantile(value, probs = 0.025),
            q.975 = quantile(value, probs = 0.975),
            q.25 = quantile(value, probs = 0.25),
            q.75 = quantile(value, probs = 0.75),
            median = quantile(value, probs = 0.5))

xlimits <- c(min(covar.list.3.summ$q.025), max(covar.list.3.summ$q.975) + 0.05)


p1 <- ggplot(data = covar.list.3.summ %>% filter(variable == "maxT_spawn")) +
  geom_hline(yintercept = 0, colour='red') +
  geom_segment(aes(x = reorder(stock, desc(stock)), xend = stock, y = q.25, yend = q.75),
               color = "blue", lwd = 1) +
  geom_segment(aes(x = stock, xend = stock, y = q.025, yend = q.975), 
               color = "darkblue", lwd = 0.5) +
  geom_point(aes(x = stock, y = median), color = "red", shape = 16, size = 1.5) +
  scale_y_continuous(limits = xlimits, breaks = c(-0.4, -0.2, 0, 0.2, 0.4)) +
  coord_flip() +
  labs(x = "", y = "Effect", title = "maxT_spawn") + 
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(0,0,1.2,0)),
        plot.margin = margin(1,1,0,1))

p2 <- ggplot(data = covar.list.3.summ %>% filter(variable == "avgT_rear")) +
  geom_hline(yintercept = 0, colour='red') +
  geom_segment(aes(x = reorder(stock, desc(stock)), xend = stock, y = q.25, yend = q.75),
               color = "blue", lwd = 1) +
  geom_segment(aes(x = stock, xend = stock, y = q.025, yend = q.975), 
               color = "darkblue", lwd = 0.5) +
  geom_point(aes(x = stock, y = median), color = "red", shape = 16, size = 1.5) +
  scale_y_continuous(limits = xlimits) +
  coord_flip() +
  labs(x = "Population", y = "Effect", title = "avgT_rear") + 
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(0,0,1.2,0)),
        plot.margin = margin(1,1,0,1))

p3 <- ggplot(data = covar.list.3.summ %>% filter(variable == "maxP_spawn")) +
  geom_hline(yintercept = 0, colour='red') +
  geom_segment(aes(x = reorder(stock, desc(stock)), xend = stock, y = q.25, yend = q.75),
               color = "blue", lwd = 1) +
  geom_segment(aes(x = stock, xend = stock, y = q.025, yend = q.975), 
               color = "darkblue", lwd = 0.5) +
  geom_point(aes(x = stock, y = median), color = "red", shape = 16, size = 1.5) +
  scale_y_continuous(limits = xlimits) +
  coord_flip() +
  labs(x = "Population", y = "Effect", title = "maxP_spawn") + 
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(0,0,1.2,0)),
        plot.margin = margin(1,1,0,1))

p4 <- ggplot(data = covar.list.3.summ %>% filter(variable == "avgP_rear")) +
  geom_hline(yintercept = 0, colour='red') +
  geom_segment(aes(x = reorder(stock, desc(stock)), xend = stock, y = q.25, yend = q.75),
               color = "blue", lwd = 1) +
  geom_segment(aes(x = stock, xend = stock, y = q.025, yend = q.975), 
               color = "darkblue", lwd = 0.5) +
  geom_point(aes(x = stock, y = median), color = "red", shape = 16, size = 1.5) +
  scale_y_continuous(limits = xlimits) +
  coord_flip() +
  labs(x = "Population", y = "Effect", title = "avgP_rear") + 
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(0,0,1.2,0)),
        plot.margin = margin(1,1,0,1))

p5 <- ggplot(data = covar.list.3.summ %>% filter(variable == "medianQ_rear")) +
  geom_hline(yintercept = 0, colour='red') +
  geom_segment(aes(x = reorder(stock, desc(stock)), xend = stock, y = q.25, yend = q.75),
               color = "blue", lwd = 1) +
  geom_segment(aes(x = stock, xend = stock, y = q.025, yend = q.975), 
               color = "darkblue", lwd = 0.5) +
  geom_point(aes(x = stock, y = median), color = "red", shape = 16, size = 1.5) +
  scale_y_continuous(limits = xlimits) +
  coord_flip() +
  labs(x = "Population", y = "Effect", title = "medianQ_rear") + 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha=0.2, fill="lightgreen") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(0,0,1.2,0)),
        plot.margin = margin(1,1,0,1))

p6 <- ggplot(data = covar.list.3.summ %>% filter(variable == "RB_spawn")) +
  geom_hline(yintercept = 0, colour='red') +
  geom_segment(aes(x = reorder(stock, desc(stock)), xend = stock, y = q.25, yend = q.75),
               color = "blue", lwd = 1) +
  geom_segment(aes(x = stock, xend = stock, y = q.025, yend = q.975), 
               color = "darkblue", lwd = 0.5) +
  geom_point(aes(x = stock, y = median), color = "red", shape = 16, size = 1.5) +
  scale_y_continuous(limits = xlimits) +
  coord_flip() +
  labs(x = "Population", y = "Effect", title = "RB_spawn") + 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha=0.2, fill="lightgreen") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(0,0,1.2,0)),
        plot.margin = margin(1,1,0,1))

p7 <- ggplot(data = covar.list.3.summ %>% filter(variable == "RB_emerge")) +
  geom_hline(yintercept = 0, colour='red') +
  geom_segment(aes(x = reorder(stock, desc(stock)), xend = stock, y = q.25, yend = q.75),
               color = "blue", lwd = 1) +
  geom_segment(aes(x = stock, xend = stock, y = q.025, yend = q.975), 
               color = "darkblue", lwd = 0.5) +
  geom_point(aes(x = stock, y = median), color = "red", shape = 16, size = 1.5) +
  scale_y_continuous(limits = xlimits, breaks = c(-0.4, -0.2, 0, 0.2, 0.4)) +
  coord_flip() +
  labs(x = "", y = "", title = "RB_emerge") + 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha=0.2, fill="lightgreen") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(0,0,1.2,0)),
        plot.margin = margin(1,1,0,1))

p8 <- ggplot(data = covar.list.3.summ %>% filter(variable == "breakup")) +
  geom_hline(yintercept = 0, colour='red') +
  geom_segment(aes(x = reorder(stock, desc(stock)), xend = stock, y = q.25, yend = q.75),
               color = "blue", lwd = 1) +
  geom_segment(aes(x = stock, xend = stock, y = q.025, yend = q.975), 
               color = "darkblue", lwd = 0.5) +
  geom_point(aes(x = stock, y = median), color = "red", shape = 16, size = 1.5) +
  scale_y_continuous(limits = xlimits, breaks = c(-0.4, -0.2, 0, 0.2, 0.4)) +
  coord_flip() +
  labs(x = "Population", y = "Effect", title = "breakup") + 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha=0.2, fill="lightgreen") +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(0,0,1.2,0)),
        plot.margin = margin(1,1,0,1))

p9 <- ggplot(data = covar.list.3.summ %>% filter(variable == "NPGO")) +
  geom_hline(yintercept = 0, colour='red') +
  geom_segment(aes(x = reorder(stock, desc(stock)), xend = stock, y = q.25, yend = q.75),
               color = "blue", lwd = 1) +
  geom_segment(aes(x = stock, xend = stock, y = q.025, yend = q.975), 
               color = "darkblue", lwd = 0.5) +
  geom_point(aes(x = stock, y = median), color = "red", shape = 16, size = 1.5) +
  scale_y_continuous(limits = xlimits, breaks = c(-0.4, -0.2, 0, 0.2, 0.4)) +
  coord_flip() +
  labs(x = "Population", y = "", title = "NPGO") + 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha=0.2, fill="lightgreen") +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(0,0,1.2,0)),
        plot.margin = margin(1,1,0,1))

p3by3 <- grid.arrange(grobs = list(p1, p2, p3, p4, p5, p6, p7, p8, p9), 
             layout_matrix = rbind(c(1,2,3),
                                   c(4,5,6),
                                   c(7,8,9)),
             widths = list(1.5,1,1),
             heights = list(1,1,1.2))

ggsave("JAGS/Plots/New Figure 4.4.png", plot = p3by3, width = 7.5, height = 6.5, units = "in")
ggsave("JAGS/Plots/New Figure 4.4.pdf", plot = p3by3, width = 7.5, height = 6.5, units = "in")

# Graphical abstract base figure #################
# Simplified version of population-specific effects 
# Just show the top 3 regional effects, plus both temp indicators and NPGO
# Replace covariate name abbrevs with meaningful labels


ga1 <- ggplot(data = covar.list.3.summ %>% filter(variable == "maxP_spawn")) +
  geom_hline(yintercept = 0, colour='red') +
  geom_segment(aes(x = reorder(stock, desc(stock)), xend = stock, y = q.25, yend = q.75),
               color = "blue", lwd = 1) +
  geom_segment(aes(x = stock, xend = stock, y = q.025, yend = q.975), 
               color = "darkblue", lwd = 0.5) +
  geom_point(aes(x = stock, y = median), color = "red", shape = 16, size = 1.5) +
  scale_y_continuous(limits = xlimits, breaks = c(-0.4, -0.2, 0, 0.2, 0.4)) +
  coord_flip() +
  labs(x = "", y = "Effect", title = "Max. precip.\n(spawning)") + 
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(0,0,1.2,0), size = rel(1.1)),
        plot.margin = margin(1,1,0,1))

ga2 <- ggplot(data = covar.list.3.summ %>% filter(variable == "avgP_rear")) +
  geom_hline(yintercept = 0, colour='red') +
  geom_segment(aes(x = reorder(stock, desc(stock)), xend = stock, y = q.25, yend = q.75),
               color = "blue", lwd = 1) +
  geom_segment(aes(x = stock, xend = stock, y = q.025, yend = q.975), 
               color = "darkblue", lwd = 0.5) +
  geom_point(aes(x = stock, y = median), color = "red", shape = 16, size = 1.5) +
  scale_y_continuous(limits = xlimits, breaks = c(-0.4, -0.2, 0, 0.2, 0.4)) +
  coord_flip() +
  labs(x = "Population", y = "Effect", title = "Mean precip.\n(rearing)") + 
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(0,0,1.2,0), size = rel(1.1)),
        plot.margin = margin(1,1,0,1))

ga3 <- ggplot(data = covar.list.3.summ %>% filter(variable == "medianQ_rear")) +
  geom_hline(yintercept = 0, colour='red') +
  geom_segment(aes(x = reorder(stock, desc(stock)), xend = stock, y = q.25, yend = q.75),
               color = "blue", lwd = 1) +
  geom_segment(aes(x = stock, xend = stock, y = q.025, yend = q.975), 
               color = "darkblue", lwd = 0.5) +
  geom_point(aes(x = stock, y = median), color = "red", shape = 16, size = 1.5) +
  scale_y_continuous(limits = xlimits, breaks = c(-0.4, -0.2, 0, 0.2, 0.4)) +
  coord_flip() +
  labs(x = "Population", y = "Effect", title = "Median discharge\n(rearing)") + 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha=0.2, fill="lightgreen") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(0,0,1.2,0), size = rel(1.1)),
        plot.margin = margin(1,1,1,1))

ga4 <- ggplot(data = covar.list.3.summ %>% filter(variable == "maxT_spawn")) +
  geom_hline(yintercept = 0, colour='red') +
  geom_segment(aes(x = reorder(stock, desc(stock)), xend = stock, y = q.25, yend = q.75),
               color = "blue", lwd = 1) +
  geom_segment(aes(x = stock, xend = stock, y = q.025, yend = q.975), 
               color = "darkblue", lwd = 0.5) +
  geom_point(aes(x = stock, y = median), color = "red", shape = 16, size = 1.5) +
  scale_y_continuous(limits = xlimits, breaks = c(-0.4, -0.2, 0, 0.2, 0.4)) +
  coord_flip() +
  labs(x = "", y = "Effect", title = "Max. temp.\n(spawning)") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(0,0,1.2,0), size = rel(1.1)),
        axis.title.x = element_blank(),
        plot.margin = margin(1,1,1,1))

ga5 <- ggplot(data = covar.list.3.summ %>% filter(variable == "avgT_rear")) +
  geom_hline(yintercept = 0, colour='red') +
  geom_segment(aes(x = reorder(stock, desc(stock)), xend = stock, y = q.25, yend = q.75),
               color = "blue", lwd = 1) +
  geom_segment(aes(x = stock, xend = stock, y = q.025, yend = q.975), 
               color = "darkblue", lwd = 0.5) +
  geom_point(aes(x = stock, y = median), color = "red", shape = 16, size = 1.5) +
  scale_y_continuous(limits = xlimits, breaks = c(-0.4, -0.2, 0, 0.2, 0.4)) +
  coord_flip() +
  labs(x = "Population", y = "Effect", title = "Mean temp.\n(rearing)") + 
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(0,0,1.2,0), size = rel(1.1)),
        plot.margin = margin(1,1,1,1))

ga6 <- ggplot(data = covar.list.3.summ %>% filter(variable == "NPGO")) +
  geom_hline(yintercept = 0, colour='red') +
  geom_segment(aes(x = reorder(stock, desc(stock)), xend = stock, y = q.25, yend = q.75),
               color = "blue", lwd = 1) +
  geom_segment(aes(x = stock, xend = stock, y = q.025, yend = q.975), 
               color = "darkblue", lwd = 0.5) +
  geom_point(aes(x = stock, y = median), color = "red", shape = 16, size = 1.5) +
  scale_y_continuous(limits = xlimits, breaks = c(-0.4, -0.2, 0, 0.2, 0.4)) +
  coord_flip() +
  labs(x = "Population", y = "Effect", title = "NPGO\n(early marine)") + 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha=0.2, fill="lightgreen") +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(0,0,1.2,0), size = rel(1.1)),
        plot.margin = margin(1,1,1,1))

ga3by2 <- grid.arrange(grobs = list(ga1, ga2, ga3, ga4, ga5, ga6), 
                      layout_matrix = rbind(c(1,2,3),
                                            c(4,5,6)),
                      widths = list(1.45,1,1),
                      heights = list(1,1.1),
                      left = "Chinook salmon population",
                      bottom = "Covariate effect",
                      padding = unit(0.5, "line"))
ggsave("Graphical Abstract base.pdf", plot = ga3by2)

# Original code generating graphical abstract base figure
# # Filter down to the 6 covariates of most interest
# covar.list.simple <- covar.list[,,c(3, 4, 5, 1, 2, 9)]
# 
# names.covars.simple <- c('Max. precip. (spawning/incubation)',
#                          'Mean precip. (rearing)',
#                          'Median discharge (rearing)',
#                          'Max. temp. (spawning/incubation)',
#                          'Mean temp. (rearing)', 
#                          'NPGO (smolt/early marine)')
# 
# n.plot.simple <- length(names.covars.simple)
# 
# #PLOT IT
# pdf(file.path(dir.figs, "Graphical abstract.pdf"), height=8, width=10)
# par(mfrow=c(2,3), mar=c(2,2,3,2), oma=c(3,10,2,2))
# c <- 1
# for(c in 1:n.plot.simple) {
#   caterplot(covar.list.simple[,,c],
#             labels=pops, reorder=FALSE, quantiles=list(0.025,0.25,0.75,0.975), 
#             style='gray', col='blue', cex = 1.1, val.lim = c(-0.45, 0.4))
#   mtext(names.covars.simple[c], side=3, outer=FALSE, line=1)
#   caterpoints(apply(covar.list.simple[,,c],2,median), reorder=FALSE, pch=21, col='red', bg='orange')
#   abline(v=0, lty=1, lwd=2, col=rgb(1,0,0, alpha=0.5))
# }
# mtext('Covariate effect', side=1, outer=TRUE, font=2, line=1, cex = 1.3)
# mtext('Chinook salmon population', side=2, outer=TRUE, font=2, line=6, cex = 1.3)
# 
# dev.off()