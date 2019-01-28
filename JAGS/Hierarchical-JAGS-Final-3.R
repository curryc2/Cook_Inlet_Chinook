#==================================================================================================
#Project Name: COOK INLET CHINOOK ENVIRONMENTAL EFFECTS - Bayesian Hierarchical Model - FINAL
#Creator: Curry James Cunningham, NOAA/NMFS, ABL
#Date: 6.10.18
#
#Purpose: To fit a Bayesian hierarchical model
#
# 1) 
#
#
#==================================================================================================
#NOTES:
#  1) Counts of weeks over temperature threshold were not Z-standardized: 'wksGT13-0','wksGT21-0','wksGT15-1'

#  2) Phase 1 includes Adult: wk16 & wk10, and Juvenile: wk15
#  3) Phase 2 includes Adult: wk13, and Juvenile: wk18
#  4) For FINAL round there will be no maxTemp for cumDD
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
phase <- 3 #3 4 5

fit <- FALSE

#Update Fig and Output directories
dir.output <- file.path(dir.output,"Final_New",paste0("Phase_",phase))
dir.create(dir.output, recursive=TRUE)
dir.figs <- file.path(dir.figs,"Final_New",paste0("Phase_",phase))
dir.create(dir.figs, recursive=TRUE)

n.sim <- 5e4#5e4
n.thin <- 10
n.chain <- 3

#Covariate Offset
# ofst <- 1

#Whether to plot SR relationships
plot.SR <- FALSE

#############################

#Read in Spawner-Recruit Data =============================================
data <- read.csv(file.path(dir.data,"SpawnersRecruits.csv"))

# Use only the core age classes - and remove NA's
sr.dat <- data %>% select(c("Population","BroodYear","EscapementMethod","Spawners2.nom","CoreRecruits2.nom","CoreRecruitsPerSpawner")) %>% filter(!is.na(Spawners2.nom), !is.na(CoreRecruits2.nom), !is.na(CoreRecruitsPerSpawner))

#Create summary table
summary.dat <- sr.dat %>% group_by(Population,EscapementMethod) %>% summarize('startBY'=min(BroodYear), 
                                                                              'endBY'=max(BroodYear), 
                                                                              'n.year'=length(unique(BroodYear))) %>% 
  arrange(Population)

# 'n.method'=length(unique(EscapementMethod)))
# 'method'=unique(EscapementMethod)[1])
# 'method'=ifelse(length(unique(EscapementMethod))>1,
#                 unique(EscapementMethod)[2],
#                 unique(EscapementMethod)[1]))

#Add covariate year to SR dataset
# sr.dat$covar.year <- sr.dat$BroodYear+ofst

#Join Covariate Stream ID to SR Data ======================================
LookupStreamID <- read.csv(file.path(dir.data,'LookupStreamID.csv'), stringsAsFactors=TRUE)

#Join up
# sr.dat <- sr.dat %>% left_join(LookupStreamID)


#Plot Spawner-Recruit Relationships =======================================
if(plot.SR==TRUE) {
  pdf(file.path(dir.figs,"SR Relationships.pdf"), height=7, width=8)
  g <- ggplot(sr.dat, aes(x=Spawners2.nom/1e3, y=CoreRecruits2.nom/1e3, color=BroodYear)) +
    theme_bw() +
    geom_point( alpha=0.75) +
    scale_color_viridis() +
    facet_wrap(~Population, scales='free') +
    xlab('Spawning Abundance (thousands)') +
    ylab('Recruitment (thousands)')
  plot(g)
  g2 <- g + geom_smooth()
  plot(g2)
  
  
  g3 <- ggplot(sr.dat, aes(x=Spawners2.nom/1e3, y=log(CoreRecruitsPerSpawner), color=BroodYear)) +
    theme_bw() +
    geom_point( alpha=0.75) +
    scale_color_viridis() +
    facet_wrap(~Population, scales='free') +
    xlab('Spawning Abundance (thousands)') +
    ylab('LN(Recruits/Spawner)') +
    geom_smooth(method='lm')
  plot(g3)
  dev.off()
  
  #Save SR Summary
  write.csv(summary.dat, file=file.path(dir.output,"SR Data Summary.csv"))
}

#Load Environmental Data ==================================================

# #SNAP Streamflow data
# precip.dat <- read.csv(file.path(dir.data,'precip.csv'), header=TRUE, stringsAsFactors=FALSE)[,-1]
# #Spread it out
# precip.dat.2 <- precip.dat %>%  spread(Covariate, Value)
# 
# #Temperature
# temp.dat <- read.csv(file.path(dir.data,'tempCovars.csv'), header=TRUE, stringsAsFactors=FALSE)

#Covariates
temp.dat <- read.csv(file.path(dir.data,'covars.csv'), header=TRUE, stringsAsFactors=FALSE)

#Breakup 
breakup.dat <- read.csv(file.path(dir.data,'breakup.csv'), header=TRUE, stringsAsFactors=FALSE)

#NPGO
npgo.dat <- read.csv(file.path(dir.data,'covars.list.csv'), header=TRUE, stringsAsFactors=FALSE)

# g <- ggplot(ason.dat, aes(x=year, y=ASON_avg)) +
#       theme_bw() +
#       geom_line() +
#       facet_wrap(~siteID)
# g
#Standardize Covariates ===================================================
#Flow Metrics - Standardized Within Populations
temp.dat.2 <- temp.dat %>% group_by(Population) %>% 
                mutate('std.ASON_max'=(ASON_max - mean(ASON_max , na.rm=TRUE))/sd(ASON_max, na.rm=TRUE),
                       'std.RB_ASON'=(RB_ASON - mean(RB_ASON , na.rm=TRUE))/sd(RB_ASON, na.rm=TRUE),
                       'std.RB_MJ'=(RB_MJ - mean(RB_MJ , na.rm=TRUE))/sd(RB_MJ, na.rm=TRUE),
                       'std.MJJA_avg'=(MJJA_avg - mean(MJJA_avg , na.rm=TRUE))/sd(MJJA_avg, na.rm=TRUE),
                       'std.MDIS_MJJA'=(MDIS_MJJA - mean(MDIS_MJJA , na.rm=TRUE))/sd(MDIS_MJJA, na.rm=TRUE))

#Temperature Metrics - (Standardized across all populations)


temp.dat.3 <- temp.dat.2 %>% mutate('std.wksGT13'=(wksGT13 - mean(wksGT13 , na.rm=TRUE))/sd(wksGT13, na.rm=TRUE),
                                    'std.maxWkJA'=(maxWkJA - mean(maxWkJA , na.rm=TRUE))/sd(maxWkJA, na.rm=TRUE),
                                    'std.wksGT15'=(wksGT15 - mean(wksGT15 , na.rm=TRUE))/sd(wksGT15, na.rm=TRUE),
                                    'std.meanWkJJA'=(meanWkJJA - mean(meanWkJJA , na.rm=TRUE))/sd(meanWkJJA, na.rm=TRUE))
# 
# temp.dat.4 <- temp.dat.2 %>% ungroup() %>%  mutate('std.wksGT13'=(wksGT13 - mean(wksGT13 , na.rm=TRUE))/sd(wksGT13, na.rm=TRUE),
#                                     'std.maxWkJA'=(maxWkJA - mean(maxWkJA , na.rm=TRUE))/sd(maxWkJA, na.rm=TRUE),
#                                     'std.wksGT15'=(wksGT15 - mean(wksGT15 , na.rm=TRUE))/sd(wksGT15, na.rm=TRUE),
#                                     'std.meanWkJJA'=(meanWkJJA - mean(meanWkJJA , na.rm=TRUE))/sd(meanWkJJA, na.rm=TRUE))

#Standardize the 
breakup.dat.2 <- breakup.dat %>% mutate('std.BreakupDOY'=(BreakupDOY - mean(BreakupDOY))/sd(BreakupDOY))

npgo.dat.2 <- npgo.dat %>% filter(Covar=='NPGO')

#Join Covariate Data to SR data ===========================================

cov.dat <- temp.dat.3
#Join Breakup data
# cov.dat.2 <- cov.dat %>% left_join(breakup.dat.2, by=c('Year'='Year'))
#Join npgo
# cov.dat.3 <- cov.dat.2 %>% left_join(npgo.dat.2, by=c('Year'='Year'))
# cov.dat.4 <- cov.dat # Completely pointless, but saves me time recoding below
#Join the Location ID lookup
# cov.dat.4 <- cov.dat.3 %>% left_join(LookupStreamID, by=c('perName'='perName'))

#Create Inputs for JAGS ===================================================
intersect(cov.dat$Population, sr.dat$Population)
length(intersect(cov.dat$Population, sr.dat$Population))

# setdiff(cov.dat.3$perName, sr.dat$Population) #In covar not in sr
# setdiff(sr.dat$Population, cov.dat.3$perName) #In sr not in covar

input.dat <- cov.dat[!is.na(cov.dat$Population),]

# pops <- sort(unique(sr.dat$Population))
pops <- na.omit(intersect(sr.dat$Population, cov.dat$Population))
n.pops <- length(pops)

#Look at correlations
# correlation <- input.dat %>%  filter(Population %in% pops) %>% select(std.ASON_avg, std.ASON_max,
# std.MJJA_avg, std.MJJA_max,
# maxTemp, wksGT15, wksGT21, wksGT13, std.BreakupDOY)

# correlation <- cor(correlation[,-1], use='pairwise.complete.obs')
# pdf(file.path(dir.figs,"Correlations.pdf"), height=8, width=8)
# corrplot.mixed(correlation)
# dev.off()
#Number of brood years for each population
n.years <- vector(length=n.pops)
years <- matrix(nrow=n.pops,ncol=50)

p <- 1
for(p in 1:n.pops) {
  n.years[p] <- length(unique(sr.dat$BroodYear[sr.dat$Population==pops[p]]))
  years[p,1:n.years[p]] <- sort(unique(sr.dat$BroodYear[sr.dat$Population==pops[p]]))
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
    spawn[p,y] <- sr.dat$Spawners2.nom[sr.dat$Population==pops[p] & sr.dat$BroodYear==year]
    #Recruits
    rec[p,y] <- sr.dat$CoreRecruits2.nom[sr.dat$Population==pops[p] & sr.dat$BroodYear==year]
    ln.rec[p,y] <- log(sr.dat$CoreRecruits2.nom[sr.dat$Population==pops[p] & sr.dat$BroodYear==year])
  }#next y
}#next p

#Make sure we are specifying the correct phase
if(phase!=3 & phase!=4 & phase!=5) {
  stop(paste("Ye fucked up, phase needs to be 3-5, currently it is:",phase))
}

#Plot Parameter Correlation ================
out.file <- file.path(wd(),"Final_New.png")
cor.input <- input.dat %>% select(c('std.wksGT13','std.wksGT15',
                                    'std.maxWkJA','std.meanWkJJA',
                                    'std.ASON_max','std.RB_ASON','std.RB_MJ',
                                    'std.MJJA_avg','std.MDIS_MJJA'))
cor.input.2 <- cor.input[,-1]

names(cor.input.2) <- c('wksGT13','wksGT15',
                        'maxT_spawn','avgT_grow',
                        'maxP_spawn','RB_spawn','RB_emerge',
                        'avgP_grow','medianQ')
#Calculate correlations
corrplot.mixed(cor(cor.input.2, use="na.or.complete"), upper='ellipse')

#Phase 3 ==========================================
if(phase==3) {
  
  #After update to remove correlated parameters
  names.covars <- c('wksGT13','wksGT15', 
                    'maxP_spawn','RB_spawn','RB_emerge',
                    'avgP_grow','medianQ',
                    'breakup','NPGO')
  
  # if(do.temp==TRUE) { names.covars <- c(names.covars,'Max.temp') }
  # names.covars <- c('ASON_max','MJJA_max')
  n.covars <- length(names.covars)
  covars <- array(data=NA,dim=c(n.pops, max(n.years), n.covars))
  
  p <- 1
  for(p in 1:n.pops) {
    y <- 1
    for(y in 1:n.years[p]) {
      year <- years[p,y]
      
      #TEMPERATURE ========
      #wksGT13-0
      temp.cov <- input.dat$std.wksGT13[input.dat$Population==pops[p] & input.dat$Year==(year+0)]
      covars[p,y,1] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #wksGT15-1
      temp.cov <- input.dat$std.wksGT15[input.dat$Population==pops[p] & input.dat$Year==(year+1)]
      covars[p,y,2] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #DISCHARGE =========
      #ASON_max-0
      temp.cov <- input.dat$std.ASON_max[input.dat$Population==pops[p] & input.dat$Year==(year+0)]
      covars[p,y,3] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #RB_ASON-0
      temp.cov <- input.dat$std.RB_ASON[input.dat$Population==pops[p] & input.dat$Year==(year+0)]
      covars[p,y,4] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #RB_MJ-1
      temp.cov <- input.dat$std.RB_MJ[input.dat$Population==pops[p] & input.dat$Year==(year+1)]
      covars[p,y,5] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #MJJA_avg-1
      temp.cov <- input.dat$std.MJJA_avg[input.dat$Population==pops[p] & input.dat$Year==(year+1)]
      covars[p,y,6] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #MDIS_MJJA-1
      temp.cov <- input.dat$std.MDIS_MJJA[input.dat$Population==pops[p] & input.dat$Year==(year+1)]
      covars[p,y,7] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #GENERAL COVARS ========
      
      #breakup-2 - Date of Breakup in year of outmigration influences 
      temp.cov <- breakup.dat.2$std.BreakupDOY[breakup.dat.2$Year==(year+2)]
      covars[p,y,8] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #NPGO-2
      temp.cov <- npgo.dat.2$std[npgo.dat.2$Year==(year+2)]
      covars[p,y,9] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
    }#next y
  }#next p
  #Plot Covariate Correlation
  
}

#Phase 4 ==========================================
if(phase==4) {
  
  #After update to remove correlated parameters
  names.covars <- c('maxT_spawn','avgT_grow', 
                    'maxP_spawn','RB_spawn','RB_emerge',
                    'avgP_grow','medianQ',
                    'breakup','NPGO')
  
  # if(do.temp==TRUE) { names.covars <- c(names.covars,'Max.temp') }
  # names.covars <- c('ASON_max','MJJA_max')
  n.covars <- length(names.covars)
  covars <- array(data=NA,dim=c(n.pops, max(n.years), n.covars))
  
  p <- 1
  for(p in 1:n.pops) {
    y <- 1
    for(y in 1:n.years[p]) {
      year <- years[p,y]
      
      #TEMPERATURE ========
      #maxWkJA-0
      temp.cov <- input.dat$std.maxWkJA[input.dat$Population==pops[p] & input.dat$Year==(year+0)]
      covars[p,y,1] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #meanWKJJA-1
      temp.cov <- input.dat$std.meanWkJJA[input.dat$Population==pops[p] & input.dat$Year==(year+1)]
      covars[p,y,2] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #DISCHARGE =========
      #ASON_max-0
      temp.cov <- input.dat$std.ASON_max[input.dat$Population==pops[p] & input.dat$Year==(year+0)]
      covars[p,y,3] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #RB_ASON-0
      temp.cov <- input.dat$std.RB_ASON[input.dat$Population==pops[p] & input.dat$Year==(year+0)]
      covars[p,y,4] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #RB_MJ-1
      temp.cov <- input.dat$std.RB_MJ[input.dat$Population==pops[p] & input.dat$Year==(year+1)]
      covars[p,y,5] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #MJJA_avg-1
      temp.cov <- input.dat$std.MJJA_avg[input.dat$Population==pops[p] & input.dat$Year==(year+1)]
      covars[p,y,6] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #MDIS_MJJA-1
      temp.cov <- input.dat$std.MDIS_MJJA[input.dat$Population==pops[p] & input.dat$Year==(year+1)]
      covars[p,y,7] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #GENERAL COVARS ========
      
      #breakup-2 - Date of Breakup in year of outmigration influences 
      temp.cov <- breakup.dat.2$std.BreakupDOY[breakup.dat.2$Year==(year+2)]
      covars[p,y,8] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #NPGO-2
      temp.cov <- npgo.dat.2$std[npgo.dat.2$Year==(year+2)]
      covars[p,y,9] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
    }#next y
  }#next p
  
}

#Phase 4 ==========================================
if(phase==5) {
  
  #After update to remove correlated parameters
  names.covars <- c(#'maxT_spawn','avgT_grow', 
                    'Quad_maxT_spawn','Quad_avgT_grow', 
                    'maxP_spawn','RB_spawn','RB_emerge',
                    'avgP_grow','medianQ',
                    'breakup','NPGO')
  
  # if(do.temp==TRUE) { names.covars <- c(names.covars,'Max.temp') }
  # names.covars <- c('ASON_max','MJJA_max')
  n.covars <- length(names.covars)
  covars <- array(data=NA,dim=c(n.pops, max(n.years), n.covars))
  
  p <- 1
  for(p in 1:n.pops) {
    y <- 1
    for(y in 1:n.years[p]) {
      year <- years[p,y]
      
      #TEMPERATURE ========
      #maxWkJA-0
      # temp.cov <- input.dat$std.maxWkJA[input.dat$Population==pops[p] & input.dat$Year==(year+0)]
      # covars[p,y,1] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      # 
      # #meanWKJJA-1
      # temp.cov <- input.dat$std.meanWkJJA[input.dat$Population==pops[p] & input.dat$Year==(year+1)]
      # covars[p,y,2] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #QUADRATIC maxWkJA-0
      temp.cov <- (input.dat$maxWkJA[input.dat$Population==pops[p] & input.dat$Year==(year+0)])^2
      covars[p,y,1] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #QUADRATIC meanWKJJA-1
      temp.cov <- (input.dat$meanWkJJA[input.dat$Population==pops[p] & input.dat$Year==(year+1)])^2
      covars[p,y,2] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #DISCHARGE =========
      #ASON_max-0
      temp.cov <- input.dat$std.ASON_max[input.dat$Population==pops[p] & input.dat$Year==(year+0)]
      covars[p,y,3] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #RB_ASON-0
      temp.cov <- input.dat$std.RB_ASON[input.dat$Population==pops[p] & input.dat$Year==(year+0)]
      covars[p,y,4] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #RB_MJ-1
      temp.cov <- input.dat$std.RB_MJ[input.dat$Population==pops[p] & input.dat$Year==(year+1)]
      covars[p,y,5] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #MJJA_avg-1
      temp.cov <- input.dat$std.MJJA_avg[input.dat$Population==pops[p] & input.dat$Year==(year+1)]
      covars[p,y,6] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #MDIS_MJJA-1
      temp.cov <- input.dat$std.MDIS_MJJA[input.dat$Population==pops[p] & input.dat$Year==(year+1)]
      covars[p,y,7] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #GENERAL COVARS ========
      
      #breakup-2 - Date of Breakup in year of outmigration influences 
      temp.cov <- breakup.dat.2$std.BreakupDOY[breakup.dat.2$Year==(year+2)]
      covars[p,y,8] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
      #NPGO-2
      temp.cov <- npgo.dat.2$std[npgo.dat.2$Year==(year+2)]
      covars[p,y,9] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
      
    }#next y
  }#next p
  
}

# Try BRM model =============================================================
# brm <- brm(CoreRecruitsPerSpawner ~ Population + Population:Spawners2.nom, data=temp.dat.4)
# # pairs(brm) 
# mcmc <- as.array(brm)
# mcmc_areas(mcmc, pars=c("Population"))
# brmstools::forest(mcmc, pars = "Population")

# brm <- brm(
#   bf(coreRecruits2.nom ~ Spawners2.nom*exp(alpha * (1-)),
#      ult ~ 1 + (1|AY), omega ~ 1, theta ~ 1, 
#      nl = TRUE),
#   data = input.dat , family = gaussian(),
#   prior = c(
#     prior(normal(5000, 1000), nlpar = "ult"),
#     prior(normal(1, 2), nlpar = "omega"),
#     prior(normal(45, 10), nlpar = "theta")
#   ),
#   control = list(adapt_delta = 0.9)
# )

#JAGS Model ========================================================
JAGS_heir <- NULL
JAGS_heir <- function() {
  #PRIORS
  #Hyperpriors
  for(c in 1:n.covars) {
    mu.coef[c] ~ dnorm(0, pow(25,-2))
    sigma.coef[c] ~ dnorm(0, pow(5,-2));T(1e-3,25)
    dist.coef[c] ~ dnorm(mu.coef[c], pow(sigma.coef[c],-2))
  }#next c
  
  for(p in 1:n.pops) {
    # alpha[p] ~ dunif(-1,5)
    exp.alpha[p] ~ dunif(0,25)
    alpha[p] <- log(exp.alpha[p])#log(exp.alpha[p])
    beta[p] ~ dunif(1,1e+6)
    sigma.oe[p] ~ dnorm(0, pow(1,-2));T(1e-3,2)#dgamma(1,1)
    
    #Covariate Effects
    for(c in 1:n.covars) {
      coef[p,c] ~ dnorm(mu.coef[c],pow(sigma.coef[c],-2))
    }
  }#next p
  
  #PREDICTIONS
  for(p in 1:n.pops) {
    for(y in 1:n.years[p]) {
      
      for(c in 1:n.covars) {
        # cov.eff[p,y,c] <- prod(coef[p,c],covars[p,y,c])
        cov.eff[p,y,c] <- coef[p,c]*covars[p,y,c]
      }
      
      #Standard Hilborn Ricker
      pred.rec[p,y] <- spawn[p,y]*exp(alpha[p]*(1-(spawn[p,y]/beta[p])) + sum(cov.eff[p,y,1:n.covars]))
      #Linear Ricker
      # pred.rec[p,y] <- spawn[p,y]*exp(alpha[p]-(spawn[p,y]/beta[p]) + sum(cov.eff[p,y,])) #Gives same result
      
      #Additive Alpha
      # pred.rec[p,y] <- spawn[p,y]*exp((alpha[p]+ sum(cov.eff[p,y,]))*(1-(spawn[p,y]/beta[p])))
      
      
      #Corrected Predicted Recruitment 
      # corr.pred.rec[p,y] <- pred.rec[p,y]*exp(-(sigma.oe[p]*sigma.oe[p])/2)
      
      #Baseline Recruitment without Covariates
      base.rec[p,y] <- spawn[p,y]*exp(alpha[p]*(1-(spawn[p,y]/beta[p])))
      # corr.base.rec[p,y] <- base.rec[p,y]*exp(-(sigma.oe[p]*sigma.oe[p])/2)
      
      
    }#next y
    
    #Fill in Ragged Components
    for(y in (n.years[p]+1):max(n.years)) {
      pred.rec[p,y] <- 0
    }
  }#next p
  
  #LIKELIHOODS
  for(p in 1:n.pops) {
    for(y in 1:n.years[p]) {
      ln.rec[p,y] ~ dnorm(log(pred.rec[p,y]), pow(sigma.oe[p],-2))
      # loglik[counter] <- log(dnorm(log(pred.rec[p,y]), pow(sigma.oe[p],-2)))
    }#next y
  }#next p
  
}

#Run JAGS Model ====================================================
parameters.to.save <- c('alpha','exp.alpha',
                        'beta',
                        'mu.coef','sigma.coef',
                        'coef',
                        'sigma.oe',
                        'cov.eff',
                        'pred.rec',#'corr.pred.rec',
                        'base.rec',#'corr.base.rec',
                        'dist.coef')

Data=list("n.pops","n.years","n.covars",
          "spawn","ln.rec","covars")

InitFn = function() {
  exp.alpha <- runif(n.pops,1,2)
  beta <- runif(n.pops,1e3,1e5)
  obs.sigma <- runif(n.pops,0.1,1)
  
  mu.coef <- rnorm(n.covars, 0, 1)
  sigma.coef <- rgamma(n.covars, 1, 1)
  
  Return <- list(exp.alpha=exp.alpha, beta=beta, obs.sigma=obs.sigma,
                 mu.coef=mu.coef, sigma.coef=sigma.coef)
  return(Return)
}

#Sequential
#Run jags
Nsim = Nburnin = n.sim

out <- NULL
out.mcmc <- NULL

if(fit==TRUE) {
  
  out <- jags.parallel(model.file=JAGS_heir, inits=InitFn, working.directory=NULL, data=Data, parameters.to.save=parameters.to.save,
                       n.chains=n.chain, n.thin=n.thin, n.iter=Nsim+Nburnin, n.burnin=Nburnin,
                       export_obj_names=c('n.chain','n.thin','Nsim','Nburnin'))   
  #Save
  saveRDS(out, file=file.path(dir.output,"out.rds"))
}else {
  out <- readRDS(file=file.path(dir.output,"out.rds"))
}
out.mcmc <- as.mcmc(out)

# mcmcplot(out)

# Plot Model Output ===============================

pdf(file.path(dir.figs,paste0('Estimates Updated.pdf')), height=6, width=8)

#Hyper Mean
par(mfcol=c(3,3), mar=c(5,0,1,0), oma=c(1,1,3,1))
c <- 1
for(c in 1:n.covars) {
  plotPost(out$BUGSoutput$sims.list$mu.coef[,c], showCurve=TRUE, main='', xlab=names.covars[c],
           xlim=c(-0.5,0.5), rope=0)
  abline(v=0, lty=1, lwd=2, col=rgb(1,0,0,alpha=0.5))
}
mtext(paste0('Hyper Means'), side=3, outer=TRUE, font=2, line=1)


#Hyper SD
par(mfcol=c(3,3), mar=c(5,1,1,1), oma=c(1,1,3,1))
c <- 1
for(c in 1:n.covars) {
  plotPost(out$BUGSoutput$sims.list$sigma.coef[,c], showCurve=FALSE, xlab=names.covars[c])
}
mtext(paste0('Hyper StDevs'), side=3, outer=TRUE, font=2, line=1)

#Full Covariate Prior
par(mfcol=c(3,3), mar=c(5,0,1,0), oma=c(1,1,3,1))
c <- 1
for(c in 1:n.covars) {
  plotPost(out$BUGSoutput$sims.list$dist.coef[,c], showCurve=TRUE, main='', xlab=names.covars[c],
           xlim=c(-0.5,0.5), rope=0)
  abline(v=0, lty=1, lwd=2, col=rgb(1,0,0,alpha=0.5))
}
mtext(paste0('Full Covariate Prior Distribution (dist.coef)'), side=3, outer=TRUE, font=2, line=1)

#Individual Covariates
par(mfcol=c(3,3), mar=c(2,5,3,1), oma=c(2,2,1,1))
c <- 1
for(c in 1:n.covars) {
  caterplot(out$BUGSoutput$sims.list$coef[,,c],
            labels=pops, reorder=FALSE, quantiles=list(0.025,0.25,0.75,0.975), style='gray', col='blue')
  mtext(names.covars[c], side=3, outer=FALSE, line=1)
  caterpoints(apply(out$BUGSoutput$sims.list$coef[,,c],2,median), reorder=FALSE, pch=21, col='red', bg='orange')
  abline(v=0, lty=1, lwd=2, col=rgb(1,0,0, alpha=0.5))
}
mtext('Coefficient (Effect)', side=1, outer=TRUE, font=2, line=0.5)
mtext('Population', side=2, outer=TRUE, font=2, line=0.5)

#Plot with bayesplot
mu.coef.list <- out$BUGSoutput$sims.list$mu.coef
colnames(mu.coef.list) <- names.covars
# color_scheme_set(scheme='brightblue')
g <- mcmc_areas(mu.coef.list) + ggtitle('Group Means (mu.coef)') + vline_0()
#+ grid_lines()
plot(g)
g.a <- mcmc_areas_ridges(mu.coef.list) + ggtitle('Group Means (mu.coef)') + vline_0()
plot(g.a)

#Pairs plot
g2 <- mcmc_pairs(mu.coef.list[,1:5])
plot(g2)
g2.b <- mcmc_pairs(mu.coef.list[,4:n.covars])
plot(g2.b)

#Plot with bayesplot
sigma.coef.list <- out$BUGSoutput$sims.list$sigma.coef
colnames(sigma.coef.list) <- names.covars
# color_scheme_set(scheme='viridis')
color_scheme_set('blue')
g3 <- mcmc_areas(sigma.coef.list) + ggtitle('Group Standard Deviations (sigma.coef)') 
plot(g3)
g3.b <- mcmc_areas_ridges(sigma.coef.list)
plot(g3.b)

#Full Prior Distribution by coefficient
dist.coef.list <- out$BUGSoutput$sims.list$dist.coef
colnames(dist.coef.list) <- names.covars
# color_scheme_set(scheme='viridis')
color_scheme_set('blue')
g4 <- mcmc_areas(dist.coef.list) + ggtitle('Full Covariate Prior Distribution (dist.coef)') + coord_cartesian(xlim=c(-2,2)) 
plot(g4)
g4.b <- mcmc_areas_ridges(dist.coef.list) + coord_cartesian(xlim=c(-2,2))
plot(g4.b)

# Ricker Components ===============

#Alpha
alpha.list <- out$BUGSoutput$sims.list$alpha
colnames(alpha.list) <- pops
g5 <- mcmc_areas(alpha.list) +
  ggtitle('Ricker Alpha')
plot(g5)
g5.b <- mcmc_areas_ridges(alpha.list) +
  ggtitle('Ricker Alpha')
plot(g5.b)

#Max RpS
alpha.list <- exp(out$BUGSoutput$sims.list$alpha)
colnames(alpha.list) <- pops
g6 <- mcmc_areas(alpha.list) +
  ggtitle('Maximum Productivity RpS: exp(a)')
plot(g6)
g6.b <- mcmc_areas_ridges(alpha.list) +
  ggtitle('Maximum Productivity RpS: exp(a)')
plot(g6.b)

#Alpha
beta.list <- out$BUGSoutput$sims.list$beta/1e3
colnames(beta.list) <- pops
g7 <- mcmc_areas(beta.list) +
  ggtitle('Equilibrium Abundance (Beta) in Thousands')
plot(g7)
g7.b <- mcmc_areas_ridges(beta.list) +
  ggtitle('Equilibrium Abundance (Beta) in Thousands')
plot(g7.b)

#Observation error SD
sigma.oe.list <- out$BUGSoutput$sims.list$sigma.oe
colnames(sigma.oe.list) <- pops
g8 <- mcmc_areas(sigma.oe.list) +
  ggtitle('Process Error (SD)') + coord_cartesian(xlim=c(0,2))
plot(g8)
g8.b <- mcmc_areas_ridges(sigma.oe.list) +
  ggtitle('Process Error (SD)') + coord_cartesian(xlim=c(0,2))
plot(g8.b)


#Tidybayes
# data.frame(sigma.coef.list) %>%
#   gather(key='covar', value='value') %>% 
#   # spread_samples(condition_mean[condition]) %>%
#   ggplot(aes(x = covar, y = value)) +
#   geom_boxplot()


# Plot Total Distribuiton =======================
# out$BUGSoutput$sims.list$dist.coef
# out$BUGSoutput$sims.list

# Plot Fits =========
par(mfrow=c(2,2), mar=c(2,2,2,0), oma=c(2,2,1,1))
p <- 2
for(p in 1:n.pops) {
  pred <- apply(out$BUGSoutput$sims.list$pred.rec[,p,1:n.years[p]],2, 
                quantile, probs=c(0.025,0.25,0.5,0.75,0.975))
  
  ylim <- c(0,max(rec[p,], pred, na.rm=TRUE))
  
  temp.yrs <- years[p,1:n.years[p]]
  
  plot(x=temp.yrs, y=rec[p,1:n.years[p]], pch=21, bg=rgb(0,0,1,alpha=0.5), ylim=ylim)
  
  polygon(x=c(temp.yrs,rev(temp.yrs)), y=c(pred[1,],rev(pred[5,])), col=rgb(1,0,0, alpha=0.2),
            border=FALSE)
  polygon(x=c(temp.yrs,rev(temp.yrs)), y=c(pred[2,],rev(pred[4,])), col=rgb(1,0,0, alpha=0.2),
          border=FALSE)
  
  lines(x=temp.yrs, y=pred[3,], col='red')
  mtext(pops[p], side=3, line=0.25, font=2)
  if(p %in% c(1,5,9,13)) {
    mtext('Recruitment', side=2, font=2, outer=TRUE, line=0.5)
    mtext('Year', side=1, font=2, outer=TRUE, line=0.5)
  }
  
  
  # lines(apply(corr.pred.rec[,p,1:n.years[p]], 2, median), col='gray')
  # lines(apply(base.rec[,p,1:n.years[p]], 2, median), col='darkgreen')
}#next p

dev.off()

png(file=file.path(dir.figs,paste0("Log Fits_",phase,".png")), height=8, width=6, units='in', res=500)
#Log Space
par(mfrow=c(5,3), mar=c(2,2,2,1), oma=c(2,2,1,1))
p <- 2
for(p in 1:n.pops) {
  log.pred <- log(apply(out$BUGSoutput$sims.list$pred.rec[,p,1:n.years[p]],2, 
                quantile, probs=c(0.025,0.25,0.5,0.75,0.975)))
  log.rec <- log(rec)
  
  ylim <- c(min(log.pred, log.rec[p,], na.rm=TRUE),max(log.pred,log.rec[p,], na.rm=TRUE))
  
  temp.yrs <- years[p,1:n.years[p]]
  
  plot(x=temp.yrs, y=log.rec[p,1:n.years[p]], pch=21, bg=rgb(0,0,1,alpha=0.5), ylim=ylim)
  
  polygon(x=c(temp.yrs,rev(temp.yrs)), y=c(log.pred[1,],rev(log.pred[5,])), col=rgb(1,0,0, alpha=0.2),
          border=FALSE)
  polygon(x=c(temp.yrs,rev(temp.yrs)), y=c(log.pred[2,],rev(log.pred[4,])), col=rgb(1,0,0, alpha=0.2),
          border=FALSE)
  
  lines(x=temp.yrs, y=log.pred[3,], col='red')
  mtext(pops[p], side=3, line=0.25, font=2)
  if(p %in% c(1,5,9,13)) {
    mtext('Log Recruitment', side=2, font=2, outer=TRUE, line=0.5)
    mtext('Year', side=1, font=2, outer=TRUE, line=0.5)
  }
  
  
  # lines(apply(corr.pred.rec[,p,1:n.years[p]], 2, median), col='gray')
  # lines(apply(base.rec[,p,1:n.years[p]], 2, median), col='darkgreen')
}#next p
dev.off()

#Pairs plot

#Create Group Level .csv for Erik ==================================


temp.summary <- out$BUGSoutput$summary
locs <- grep("mu", dimnames(temp.summary)[[1]])

write.csv(cbind(names.covars, temp.summary[locs,]), file=file.path(dir.output,'Mu Coefs.csv'))


# exp(0.1)

# plotPost(out$BUGSoutput$sims.list)
# 
# p <- 3
# pred.rec <- out$BUGSoutput$sims.list$pred.rec
# plot(x=spawn[p,], y=rec[p,], pch=21, bg=rgb(0,0,1,alpha=0.5), ylim=c(0, max(rec[p,],apply(pred.rec[,p,1:n.years[p]], 2, median), na.rm=TRUE)))
# 
# points(x=spawn[p,1:n.years[p]], y=apply(pred.rec[,p,1:n.years[p]], 2, median), col='red')
# # points(x=spawn[p,1:n.years[p]], y=apply(base.rec[,p,1:n.years[p]], 2, median), col='red')
# 
# plot(x=rec[p,], y=apply(pred.rec[,p,], 2, median))
# 
# alpha <- out$BUGSoutput$sims.list$alpha[,p]
# beta <- out$BUGSoutput$sims.list$beta[,p]
# pred.rec <- spawn[p,]*exp(alpha*(1-spawn[p,]/beta))
# 
# pred.rec <- out$BUGSoutput$sims.list$pred.rec
# corr.pred.rec <- out$BUGSoutput$sims.list$corr.pred.rec
# base.rec <- out$BUGSoutput$sims.list$base.rec
# 
# ylim <- c(0,max(rec[p,], apply(pred.rec[,p,1:n.years[p]],2,median), na.rm=TRUE))
# 
# plot(rec[p,], pch=21, bg=rgb(0,0,1,alpha=0.5), ylim=ylim)
# lines(apply(pred.rec[,p,1:n.years[p]], 2, median), col='red')
# # lines(apply(corr.pred.rec[,p,1:n.years[p]], 2, median), col='gray')
# lines(apply(base.rec[,p,1:n.years[p]], 2, median), col='darkgreen')
# 
# 
# 
