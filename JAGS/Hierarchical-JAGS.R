#==================================================================================================
#Project Name: COOK INLET CHINOOK ENVIRONMENTAL EFFECTS - Bayesian Hierarchical Model
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
#
#==================================================================================================
require(ggplot2)
require(R2jags)
require(reshape2)
require(mgcv)
require(compiler)
require(mcmcplots)
require(xlsx)
require(bbmle)
require(beepr)
require(BEST)
require(dplyr)
require(tidyverse)
require(viridis)
require(bayesplot)
require(tidybayes)
require(brms)
require(brmstools)

#Define Workflow Paths ====================================================
# *Assumes you are working from the Sergent_Streamflow R project
wd <- file.path(getwd(),"JAGS")
# setwd(wd)
dir.output <- file.path(wd,"Output")
dir.figs <- file.path(wd,"Plots")
dir.data <- file.path(wd,"Data")


#CONTROL SECTION ==========================================================
#############################


n.sim <- 2e4
n.thin <- 10
n.chain <- 3

#Covariate Offset
ofst <- 1

#Whether to plot SR relationships
plot.SR <- FALSE

#Whether to include maximum temperature as a covariate
# Note: This loses: Campbell, Lewis, and Little Susitna
do.temp <- TRUE

#############################

#Read in Spawner-Recruit Data =============================================
data <- read.csv(file.path(dir.data,"SpawnersRecruits.csv"))

# Use only the core age classes - and remove NA's
temp.dat <- data %>% select(c("Population","BroodYear","EscapementMethod","Spawners2.nom","CoreRecruits2.nom","CoreRecruitsPerSpawner")) %>% filter(!is.na(Spawners2.nom), !is.na(CoreRecruits2.nom), !is.na(CoreRecruitsPerSpawner))

#Create summary table
summary.dat <- temp.dat %>% group_by(Population,EscapementMethod) %>% summarize('startBY'=min(BroodYear), 
                                                                                'endBY'=max(BroodYear), 
                                                               'n.year'=length(unique(BroodYear))) %>% 
                            arrange(Population)
                                                               # 'n.method'=length(unique(EscapementMethod)))
                                                               # 'method'=unique(EscapementMethod)[1])
                                                               # 'method'=ifelse(length(unique(EscapementMethod))>1,
                                                               #                 unique(EscapementMethod)[2],
                                                               #                 unique(EscapementMethod)[1]))

#Add covariate year to SR dataset
temp.dat$covar.year <- temp.dat$BroodYear+ofst

#Join Covariate Stream ID to SR Data ======================================
LookupStreamID <- read.csv(file.path(dir.data,'LookupStreamID.csv'))

#Join up
temp.dat <- temp.dat %>% left_join(LookupStreamID)


#Plot Spawner-Recruit Relationships =======================================
if(plot.SR==TRUE) {
  pdf(file.path(dir.figs,"SR Relationships.pdf"), height=7, width=8)
  g <- ggplot(temp.dat, aes(x=Spawners2.nom/1e3, y=CoreRecruits2.nom/1e3, color=BroodYear)) +
         theme_bw() +
         geom_point( alpha=0.75) +
         scale_color_viridis() +
         facet_wrap(~Population, scales='free') +
         xlab('Spawning Abundance (thousands)') +
         ylab('Recruitment (thousands)')
  plot(g)
  g2 <- g + geom_smooth()
  plot(g2)
  
  
  g3 <- ggplot(temp.dat, aes(x=Spawners2.nom/1e3, y=log(CoreRecruitsPerSpawner), color=BroodYear)) +
          theme_bw() +
          geom_point( alpha=0.75) +
          scale_color_viridis() +
          facet_wrap(~Population, scales='free') +
          xlab('Spawning Abundance (thousands)') +
          ylab('LN(Recruits/Spawner)') +
          geom_smooth(method='lm')
  plot(g3)
  dev.off()
}

#Load Environmental Data ==================================================
cov.max.temp <- read.csv(file=file.path(dir.data,'maxWeeklyTemps_V2.csv'), stringsAsFactors=FALSE)[,-1]

#Precipitation metrics
# precip.dat <- read.csv(file='Data/Precip_metrics.csv', header=TRUE, stringsAsFactors=FALSE)

#SNAP Streamflow data
ason.dat <- read.csv(file.path(dir.data,'ASON_METRICS.csv'), header=TRUE, stringsAsFactors=FALSE)
mjja.dat <- read.csv(file.path(dir.data,'MJJA_METRICS.csv'), header=TRUE, stringsAsFactors=FALSE)

# g <- ggplot(ason.dat, aes(x=year, y=ASON_avg)) +
#       theme_bw() +
#       geom_line() +
#       facet_wrap(~siteID)
# g
#Standardize Covariates ===================================================
cov.max.temp.2 <- cov.max.temp %>% group_by(Site) %>% mutate('std.Max.temp'=(Max.temp - mean(Max.temp, na.rm=TRUE))/
                                                               sd(Max.temp, na.rm=TRUE)) %>% arrange(Site)

ason.dat.2 <- ason.dat %>% group_by(siteID) %>% mutate('std.ASON_avg'=(ASON_avg-mean(ASON_avg, na.rm=TRUE))/
                                                               sd(ASON_avg, na.rm=TRUE),
                                                           'std.ASON_max'=(ASON_max-mean(ASON_max, na.rm=TRUE))/
                                                               sd(ASON_max, na.rm=TRUE))

mjja.dat.2 <- mjja.dat %>% group_by(siteID) %>% mutate('std.MJJA_avg'=(MJJA_avg-mean(MJJA_avg, na.rm=TRUE))/
                                                         sd(MJJA_avg, na.rm=TRUE),
                                                       'std.MJJA_max'=(MJJA_max-mean(MJJA_max, na.rm=TRUE))/
                                                         sd(MJJA_max, na.rm=TRUE))

#Join Covariate Data to SR data ===========================================
temp.dat.2 <- temp.dat %>% left_join(ason.dat.2, by=c('siteID'='siteID','covar.year'='year'))
# head(temp.dat.2)

temp.dat.3 <- temp.dat.2 %>% left_join(mjja.dat.2, by=c('siteID'='siteID','covar.year'='year'))
# head(temp.dat.3)

#Plot correlation
# pdf(file.path(dir.figs,"Flow corr.pdf"), height=4, width=6)
# par(mfrow=c(1,2))
# plot(std.ASON_avg ~ std.ASON_max, data=temp.dat.3, pch=21, bg=rgb(1,0,0,alpha=0.5))
# plot(std.MJJA_avg ~ std.MJJA_max, data=temp.dat.3, pch=21, bg=rgb(0,0,1, alpha=0.5))
# dev.off()

if(do.temp==TRUE) {
  temp.dat.4 <- temp.dat.3 %>% filter(!is.na(Site)) %>% left_join(cov.max.temp.2, by=c('Site'='Site', 'covar.year'='Year'))
}



#Create Inputs for JAGS ===================================================
if(do.temp==FALSE) {
  input.dat <- temp.dat.3
}else {
  input.dat <- temp.dat.4
}

pops <- sort(unique(input.dat$Population))
n.pops <- length(pops)

#Number of brood years for each population
n.years <- vector(length=n.pops)
years <- matrix(nrow=n.pops,ncol=50)

p <- 1
for(p in 1:n.pops) {
  n.years[p] <- length(unique(input.dat$BroodYear[input.dat$Population==pops[p]]))
  years[p,1:n.years[p]] <- sort(unique(input.dat$BroodYear[input.dat$Population==pops[p]]))
}#next i

# Spawners and Recruits
spawn <- matrix(nrow=n.pops,ncol=50)
rec <- matrix(nrow=n.pops,ncol=50)
ln.rec <- matrix(nrow=n.pops,ncol=50)

p <- 1
for(p in 1:n.pops) {
  y <- 1
  for(y in 1:n.years[p]) {
    year <- years[p,y]
    
    #Spawners
    spawn[p,y] <- input.dat$Spawners2.nom[input.dat$Population==pops[p] & input.dat$BroodYear==year]
    #Recruits
    rec[p,y] <- input.dat$CoreRecruits2.nom[input.dat$Population==pops[p] & input.dat$BroodYear==year]
    ln.rec[p,y] <- log(input.dat$CoreRecruits2.nom[input.dat$Population==pops[p] & input.dat$BroodYear==year])
  }#next y
}#next p


#Covariate input
names.covars <- c('ASON_avg','ASON_max','MJJA_avg','MJJA_max')
if(do.temp==TRUE) { names.covars <- c(names.covars,'Max.temp') }
# names.covars <- c('ASON_max','MJJA_max')
n.covars <- length(names.covars)
covars <- array(data=NA,dim=c(n.pops, 50, n.covars))


p <- 1
for(p in 1:n.pops) {
  y <- 1
  for(y in 1:n.years[p]) {
    year <- years[p,y]
    
    #ASON_avg
    temp.cov <- input.dat$std.ASON_avg[input.dat$Population==pops[p] & input.dat$BroodYear==year]
    covars[p,y,1] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
    #ASON_max
    temp.cov <- input.dat$std.ASON_max[input.dat$Population==pops[p] & input.dat$BroodYear==year]
    covars[p,y,2] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
    #MJJA_avg
    temp.cov <- input.dat$std.MJJA_avg[input.dat$Population==pops[p] & input.dat$BroodYear==year]
    covars[p,y,3] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
    #MJJA_max
    temp.cov <- input.dat$std.MJJA_max[input.dat$Population==pops[p] & input.dat$BroodYear==year]
    covars[p,y,4] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
    
    #Add Temp if necessary
    if(do.temp==TRUE) {
      #Max.temp
      temp.cov <- input.dat$std.Max.temp[input.dat$Population==pops[p] & input.dat$BroodYear==year]
      covars[p,y,5] <- ifelse(is.na(temp.cov),0,temp.cov) # Fill in with zero if unavailable
    }
    
  }#next y
}#next p

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
      mu.coef[c] ~ dnorm(0, pow(5,-2))
      sigma.coef[c] ~ dnorm(0, pow(5,-2));T(1e-3,10)
  }#next c
  
  for(p in 1:n.pops) {
    # alpha[p] ~ dunif(-1,5)
    exp.alpha[p] ~ dunif(0,10)
    alpha[p] <- log(exp.alpha[p])#log(exp.alpha[p])
    beta[p] ~ dunif(1,1e+6)
    sigma.oe[p] ~ dnorm(0, pow(1,-2));T(1e-3,10)#dgamma(1,1)
    
    #Covariate Effects
    for(c in 1:n.covars) {
      coef[p,c] ~ dnorm(mu.coef[c],pow(sigma.coef[c],-2))
    }
  }#next p
  
  #PREDICTIONS
  for(p in 1:n.pops) {
    for(y in 1:n.years[p]) {
      
      for(c in 1:n.covars) {
        cov.eff[p,y,c] <- prod(coef[p,c],covars[p,y,c])
      }
      
      #Standard Hilborn Ricker
      pred.rec[p,y] <- spawn[p,y]*exp(alpha[p]*(1-(spawn[p,y]/beta[p])) + sum(cov.eff[p,y,1:n.covars]))
      #Linear Ricker
      # pred.rec[p,y] <- spawn[p,y]*exp(alpha[p]-(spawn[p,y]/beta[p]) + sum(cov.eff[p,y,])) #Gives same result
      
      #Additive Alpha
      # pred.rec[p,y] <- spawn[p,y]*exp((alpha[p]+ sum(cov.eff[p,y,]))*(1-(spawn[p,y]/beta[p])))
      
      
      #Corrected Predicted Recruitment 
      corr.pred.rec[p,y] <- pred.rec[p,y]*exp(-(sigma.oe[p]*sigma.oe[p])/2)
      
      #Baseline Recruitment without Covariates
      base.rec[p,y] <- spawn[p,y]*exp(alpha[p]*(1-(spawn[p,y]/beta[p])))
      corr.base.rec[p,y] <- base.rec[p,y]*exp(-(sigma.oe[p]*sigma.oe[p])/2)
      
    }#next y
  }#next p
  
  #LIKELIHOODS
  for(p in 1:n.pops) {
    for(y in 1:n.years[p]) {
      ln.rec[p,y] ~ dnorm(log(pred.rec[p,y]), pow(sigma.oe[p],-2))
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
                        'pred.rec','corr.pred.rec',
                        'base.rec','corr.base.rec')

Data=list("n.pops","n.years","n.covars",
          "spawn","ln.rec","covars")

InitFn = function() {
  exp.alpha <- runif(n.pops,1,5)
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



out <- jags.parallel(model.file=JAGS_heir, inits=NULL, working.directory=NULL, data=Data, parameters.to.save=parameters.to.save,
                     n.chains=n.chain, n.thin=n.thin, n.iter=Nsim+Nburnin, n.burnin=Nburnin,
                     export_obj_names=c('n.chain','n.thin','Nsim','Nburnin'))   
out.mcmc <- as.mcmc(out)

# mcmcplot(out)

pdf(file.path(dir.figs,paste0('Estimates ofst_',ofst,' do.temp_',do.temp,'.pdf')), height=6, width=6)

#Hyper Mean
par(mfcol=c(2,3), mar=c(5,1,1,1), oma=c(1,1,3,1))
c <- 1
for(c in 1:n.covars) {
  plotPost(out$BUGSoutput$sims.list$mu.coef[,c], showCurve=TRUE, main='', xlab=names.covars[c],
             xlim=c(-0.5,0.5))
  abline(v=0, lty=2, lwd=2, col='red')
}
mtext(paste0('Hyper Means: ',ofst,'-year offset BY'), side=3, outer=TRUE, font=2, line=1)


#Hyper SD
par(mfcol=c(2,3), mar=c(5,1,1,1), oma=c(1,1,3,1))
c <- 1
for(c in 1:n.covars) {
  plotPost(out$BUGSoutput$sims.list$sigma.coef[,c], showCurve=FALSE, xlab=names.covars[c])
  abline(v=0, lty=2, lwd=2, col='red')
}
mtext(paste0('Hyper StDevs: ',ofst,'-year offset from BY'), side=3, outer=TRUE, font=2, line=1)

#Individual Covariates
par(mfcol=c(2,3), mar=c(2,5,3,1), oma=c(2,2,1,1))
c <- 1
for(c in 1:n.covars) {
  caterplot(out$BUGSoutput$sims.list$coef[,,c],
              labels=pops, reorder=FALSE, quantiles=list(0.0))
  mtext(names.covars[c], side=3, outer=FALSE, line=1)
  # caterplot(out$BUGSoutput$sims.list$coef[,,c], order=FALSE)
  abline(v=0, lty=2, lwd=2, col='red')
}
mtext('Coefficient (Effect)', side=1, outer=TRUE, font=2, line=0.5)
mtext('Population', side=2, outer=TRUE, font=2, line=0.5)

#Plot with bayesplot
mu.coef.list <- out$BUGSoutput$sims.list$mu.coef
colnames(mu.coef.list) <- names.covars
# color_scheme_set(scheme='brightblue')

g <- bayesplot::mcmc_areas(mu.coef.list) #+ grid_lines()
plot(g)



#Pairs plot
g2 <- mcmc_pairs(mu.coef.list)
plot(g2)

#Plot with bayesplot
sigma.coef.list <- out$BUGSoutput$sims.list$sigma.coef
colnames(sigma.coef.list) <- names.covars
# color_scheme_set(scheme='brightblue')

g3 <- bayesplot::mcmc_areas(sigma.coef.list)# + grid_lines()
plot(g3)


#Tidybayes
# data.frame(sigma.coef.list) %>%
#   gather(key='covar', value='value') %>% 
#   # spread_samples(condition_mean[condition]) %>%
#   ggplot(aes(x = covar, y = value)) +
#   geom_boxplot()

dev.off()
#Pairs plot





# exp(0.1)

# plotPost(out$BUGSoutput$sims.list)

p <- n.pops
pred.rec <- out$BUGSoutput$sims.list$pred.rec
plot(x=spawn[p,], y=rec[p,], pch=21, bg=rgb(0,0,1,alpha=0.5))
points(x=spawn[p,1:n.years[p]], y=apply(pred.rec[,p,1:n.years[p]], 2, median), col='red')
# points(x=spawn[p,1:n.years[p]], y=apply(base.rec[,p,1:n.years[p]], 2, median), col='red')


pred.rec <- out$BUGSoutput$sims.list$pred.rec
corr.pred.rec <- out$BUGSoutput$sims.list$corr.pred.rec
base.rec <- out$BUGSoutput$sims.list$base.rec

ylim <- c(0,max(rec[p,], apply(pred.rec[,p,1:n.years[p]],2,median), na.rm=TRUE))

plot(rec[p,], pch=21, bg=rgb(0,0,1,alpha=0.5), ylim=ylim)
lines(apply(pred.rec[,p,1:n.years[p]], 2, median), col='red')
# lines(apply(corr.pred.rec[,p,1:n.years[p]], 2, median), col='gray')
lines(apply(base.rec[,p,1:n.years[p]], 2, median), col='darkgreen')



