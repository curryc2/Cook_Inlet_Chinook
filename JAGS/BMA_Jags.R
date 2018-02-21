#************************************************************************************************
#Project Name: COOK INLET CHINOOK ENVIRONMENTAL EFFECTS - Bayesian Model Selection
#Creator: Curry James Cunningham, CFOS, University of Alaska, Fairbanks
#Date: 1.25.17
#
#Purpose: To model CI Chinook production dynamics with a hierarchical Bayesian model
#
# 1) Fit to indices of annual abundance (i.e assuming no density dependence)
#
# 2) Fit Ricker-type spawner-recruit model
#
#
#*************************************************************************************************
#NOTES:
#
#*************************************************************************************************
require(ggplot2)
require(R2jags)
require(reshape2)
require(mgcv)
require(LaplacesDemon)
require(compiler)
require(mcmcplots)
require(xlsx)
require(bbmle)
require(beepr)
require(BEST)



wd <- '/Users/curryc2/Documents/2016/Shoen Chinook/JAGS'
setwd(wd)

#Read in Data
data <- read.xlsx(file='Data/CI Chinook Data.xlsx', sheetName='RunSize', stringsAsFactors=FALSE)

#Read in covariate data
cov.max.temp <- read.csv(file='Data/maxWeeklyTemps_V2.csv', stringsAsFactors=FALSE)
min.yr.cov <- min(cov.max.temp$Year)

#Precipitation metrics
# precip.dat <- read.csv(file='Data/Precip_metrics.csv', header=TRUE, stringsAsFactors=FALSE)

#SNAP Streamflow data
ason.dat <- read.csv('Data/ASON_METRICS.csv', header=TRUE, stringsAsFactors=FALSE)
# mjja.dat <- read.csv('Data/MJJA_METRICS.csv', header=TRUE, stringsAsFactors=FALSE)

###
# SET MINIMUM YEAR
# offsets <- c(3:5)
offsets <- c(4)
#Offset 3: Year of emergence (by +1), for 1.2's
#Offset 4: Year of ocean entry (by +2) for 1.2's
#          Year of emergence (by+ +1) for 1.3's.

n.offsets <- length(offsets)
max.offset <- max(offsets)

#Determine starting year for evaluation
start.year <- min.yr.cov + max.offset

#Limit year range for Evaluation
data <- data[data$ReturnYear>=start.year,]

#Removed stream
# data[-which(data$ReturnYear>=start.year),]

head(data)

#Preplotting
# g <- ggplot(data, aes(x=ReturnYear, y=RunSizeIndex)) +
#        theme_gray() +
#        geom_line() +
#        geom_point() +
#        facet_wrap(~Stream, scales='free')
# g
# ggsave('Plots/Data Series.pdf')


#Remove YearXStream combinations without any data
# loc <- which(data$RunSizeIndex==0)
loc <- which(is.na(data$RunSizeIndex))

#Remove NA's
if(length(loc) > 0) {
  data <- data[-loc,]
}

#Compile Metadata
streams.data <- unique(data$Stream)
streams.cov.max.temp <- unique(cov.max.temp$Site)
streams.cov.precip.ason <- unique(ason.dat$siteID)



#Find common streams
streams <- Reduce(intersect, list(streams.data, streams.cov.max.temp))
n.streams <- length(streams)

#Range of Years
all.yr.range <- min(data$ReturnYear):max(data$ReturnYear)
n.all.yr.range <- length(all.yr.range)

n.yrs <- vector(length=n.streams)


s <- 1
for(s in 1:n.streams) {
  stream <- streams[s]
  n.yrs[s] <- length(data$ReturnYear[data$Stream==stream])
}#next s

#Maximum number of years for any stream
max.n.yrs <- max(n.yrs)

#Matrix of stream x year observations
#  -1 will be used for missing data
stream.yrs <- matrix(data=-1, nrow=n.streams, ncol=max.n.yrs)
stream.runSize <- matrix(data=-1, nrow=n.streams, ncol=max.n.yrs)
ln.stream.runSize <- matrix(data=-1, nrow=n.streams, ncol=max.n.yrs)
std.stream.runSize <- matrix(data=-1, nrow=n.streams, ncol=max.n.yrs)
stream.max.temp <- array(data=-1, dim=c(n.streams, max.n.yrs, n.offsets))

s <- 1
for(s in 1:n.streams) {
  stream <- streams[s]
  temp.yrs <- sort(data$ReturnYear[data$Stream==stream])
  n.temp.yrs <- length(temp.yrs)
  stream.yrs[s,c(1:n.yrs[s])] <- temp.yrs
  
  #Move through years
  y <- 1
  for(y in 1:n.temp.yrs) {
    stream.runSize[s,y] <- data$RunSizeIndex[data$Stream==stream & data$ReturnYear==temp.yrs[y]]
    
    #Fill in covariate values for each offset level
    o <- 1
    for(o in 1:n.offsets) {
      stream.max.temp[s,y,o] <- cov.max.temp$Max.temp[cov.max.temp$Site==stream & cov.max.temp$Year==(temp.yrs[y]-offsets[o])]
    }#next o
  }#next y
}#next s

#Z-Standardize the covariate matrices
# s <- 1
# for(s in 1:n.streams) {
#   o <- 1
#   for(o in 1:n.offsets) {
#     stream.max.temp[s,(1:n.yrs[s]),o] <- as.vector(scale(stream.max.temp[s,(1:n.yrs[s]),o]))
#   }#next o
# }#next s

#Calculate log of Run Size observations
s <- 1
for(s in 1:n.streams) {
  ln.stream.runSize[s,(1:n.yrs[s])] <- log(stream.runSize[s,(1:n.yrs[s])])
}

#Standardized Run Size observations
s <- 1
for(s in 1:n.streams) {
  std.stream.runSize[s,(1:n.yrs[s])] <- scale(as.vector(stream.runSize[s,(1:n.yrs[s])]))
}

#============================================================================================
#JAGS MODEL

jags_index_BMA <- NULL

jags_index_BMA <- function() {
  #HYPERPARAMETERS
  for(o in 1:n.offsets) {
    mu.max.temp[o] ~ dnorm(0,pow(1e4,-2))
    sigma.max.temp[o] ~ dunif(0.001,1e4)
  }#next offset
  pind ~ dbeta(1,1)
  #PRIORS
  for(s in 1:n.streams) {
    #Parameters
    intercept[s] ~ dunif(0,1e6)
    
    sigma.oe[s] ~ dunif(1e-3,1e4)#dgamma(1,1)
    for(o in 1:n.offsets) {
      coef.max.temp[s,o] ~ dnorm(mu.max.temp[o], pow(sigma.max.temp[o],-2))
      ind.prod[s,o] ~ dbern(pind)
      effect.max.temp[s,o] <- ind.prod[s,o]*coef.max.temp[s,o]
    }
  }#next s
  #LIKELIHOOD
  for(s in 1:n.streams) {
    for(y in 1:n.yrs[s]) {
      # pred[s,y] <- intercept[s] + coef.max.temp[s]*stream.max.temp[s,y]
      pred[s,y] <- intercept[s] + effect.max.temp[s,1]*stream.max.temp[s,y,1] +
        effect.max.temp[s,2]*stream.max.temp[s,y,2] +
        effect.max.temp[s,3]*stream.max.temp[s,y,3] #Can be simplified
      
      #Normal w/ standardized run size
      stream.runSize[s,y] ~ dnorm(pred[s,y], pow(sigma.oe[s],-2))
      
    }#next y
  }#next s
  
  #DERIVED PARAMETERS
}

#Using Standardized Run Size

jags_index_stdRS_BMA <- NULL

jags_index_stdRS_BMA <- function() {
  #HYPERPARAMETERS
  for(o in 1:n.offsets) {
    mu.max.temp[o] ~ dnorm(0,pow(10,-2))
    sigma.max.temp[o] ~ dunif(0.001,10)
  }#next offset
  pind ~ dbeta(1,1)
  #PRIORS
  for(s in 1:n.streams) {
    #Parameters
    # intercept[s] ~ dnorm(0,pow(10,-2))
    
    sigma.oe[s] ~ dunif(1e-3,5)#dgamma(1,1)
    for(o in 1:n.offsets) {
      coef.max.temp[s,o] ~ dnorm(mu.max.temp[o], pow(sigma.max.temp[o],-2))
      ind.prod[s,o] ~ dbern(pind)
      effect.max.temp[s,o] <- ind.prod[s,o]*coef.max.temp[s,o]
    }
  }#next s
  #LIKELIHOOD
  for(s in 1:n.streams) {
    for(y in 1:n.yrs[s]) {
      # pred[s,y] <- intercept[s] + coef.max.temp[s]*stream.max.temp[s,y]
      pred[s,y] <- effect.max.temp[s,1]*stream.max.temp[s,y,1] +
                    effect.max.temp[s,2]*stream.max.temp[s,y,2] +
                    effect.max.temp[s,3]*stream.max.temp[s,y,3] #Can be simplified
      
      #Normal w/ standardized run size
      std.stream.runSize[s,y] ~ dnorm(pred[s,y], pow(sigma.oe[s],-2))
      
    }#next y
  }#next s
  
  #DERIVED PARAMETERS
}

#Using Standardized Run Size

jags_index_lnRS <- NULL

# jags_index_lnRS <- function() {
#   #HYPERPARAMETERS
#   mu.max.temp ~ dnorm(0,pow(1e3,-2))
#   sigma.max.temp ~ dunif(0.001,1e6)
#   
#   #PRIORS
#   for(s in 1:n.streams) {
#     #Parameters
#     intercept[s] ~ dnorm(0,pow(10,-2))
#     coef.max.temp[s] ~ dnorm(mu.max.temp, pow(sigma.max.temp,-2))
#     sigma.oe[s] ~ dgamma(1,1)
#     
#   }#next s
#   #LIKELIHOOD
#   for(s in 1:n.streams) {
#     for(y in 1:n.yrs[s]) {
#       pred[s,y] <- intercept[s] + coef.max.temp[s]*stream.max.temp[s,y]
#       # pred[s,y] <- coef.max.temp[s]*stream.max.temp[s,y]
#       ln.pred[s,y] <- log(pred[s,y])
#       #Normal w/ standardized run size
#       ln.stream.runSize[s,y] ~ dnorm(ln.pred[s,y], pow(sigma.oe[s],-2))
#       
#     }#next y
#   }#next s
#   
#   #DERIVED PARAMETERS
# }

#============================================================================================
#RUN JAGS MODEL
parameters.to.save <- c('intercept',
                        'coef.max.temp',
                        'mu.max.temp','sigma.max.temp',
                        'sigma.oe',
                        'pred',
                        'pind','ind.prod')

Data=list("stream.runSize"=stream.runSize, "ln.stream.runSize"=ln.stream.runSize, "std.stream.runSize"=std.stream.runSize,
          "stream.max.temp"=stream.max.temp, "n.yrs"=n.yrs,
          "n.streams"=n.streams, 'n.offsets'=n.offsets)

#Initial values
Init_Fn <- function() {
  #Initialize
  ind.prod <- matrix(nrow=n.streams, ncol=n.offsets)
  sigma.oe <- vector(length=n.streams)
  
  for(s in 1:n.streams) {
    for(o in 1:n.offsets) {
      ind.prod[s,o] <- sample(c(0,1), 1)
    }
    # sigma.oe[s] <- runif(1,1e-3,0.25)
  }
  Return <- list(ind.prod=ind.prod)
                 # sigma.oe=sigma.oe)
  return(Return)
}

#Run the model
chains <- 3
sims <- 1e4
thins <- 10

Nsim <- sims
Nburnin <- sims
out <- FALSE

#Sequential
out <- jags(model.file=jags_index_stdRS_BMA, inits=Init_Fn, working.directory=NULL, data=Data, parameters.to.save=parameters.to.save,
            n.chains=chains, n.thin=thins, n.iter=Nsim+Nburnin, n.burnin=Nburnin)


out <- jags(model.file=jags_index_BMA, inits=Init_Fn, working.directory=NULL, data=Data, parameters.to.save=parameters.to.save,
            n.chains=chains, n.thin=thins, n.iter=Nsim+Nburnin, n.burnin=Nburnin)



n.sims <- out$BUGSoutput$n.sims




#Parallel
out
#============================================================================================
#PLOT RESULTS

out.mcmc <- as.mcmc(out)

#Caterplot
caterplot(out.mcmc, parms='intercept', labels=streams, collapse=FALSE, reorder=FALSE,
          quantiles=list(outer=c(0.025,0.975),inner=c(0.25,0.75)) )

caterplot(out.mcmc, parms='coef.max.temp', labels=streams, collapse=FALSE, reorder=FALSE,
          quantiles=list(outer=c(0.025,0.975),inner=c(0.25,0.75)) )

caterplot(out.mcmc, parms='sigma.oe', labels=streams, collapse=FALSE, reorder=FALSE,
          quantiles=list(outer=c(0.025,0.975),inner=c(0.25,0.75)) )

caterplot(out.mcmc, parms='pind', collapse=FALSE, reorder=FALSE,
          quantiles=list(outer=c(0.025,0.975),inner=c(0.25,0.75)) )

plotPost(out$BUGSoutput$sims.list$mu.max.temp[,2], xlab='Hyper: Mean for Max Temp')
plotPost(out$BUGSoutput$sims.list$sigma.max.temp[,3], xlab='Hyper: SD for Max Temp')


#Inclusion probability
ind.prod.mean <- apply(out$BUGSoutput$sims.list$ind.prod, c(2,3), mean)
dimnames(ind.prod.mean) <- list(streams,offsets)
list.ind.prod.mean <- melt(ind.prod.mean)
names(list.ind.prod.mean) <- c('Stream','Offset','value')
list.ind.prod.mean$Offset <- as.factor(list.ind.prod.mean$Offset)

g <- ggplot(list.ind.prod.mean, aes(x=Stream, y=value, fill=Stream)) +
       theme_gray() +
       geom_point() +
       facet_wrap(~Offset) +
       coord_flip() +
       stat_summary(fun.y=mean, geom='bar', position='stack') +
       ylim(c(0,1)) +
       guides(fill=FALSE)
g
ggsave('Plots/Inclusion Probability.png', plot=g, height=7, width=8, dpi=500, units='in')                      

#Covariate values
ind.prod.arr <- out$BUGSoutput$sims.list$ind.prod
dimnames(ind.prod.arr) <- list(c(1:n.sims), streams, offsets)
ind.prod.list <- melt(ind.prod.arr)
names(ind.prod.list) <- c('Sim','Stream','Offset','value')
int.prod.list$Offset <- as.factor(paste("-",int.prod.list$Offset))

coef.arr <- out$BUGSoutput$sims.list$coef.max.temp
dimnames(coef.arr) <- list(c(1:n.sims), streams, offsets)
coef.list <- melt(coef.arr)
names(coef.list) <- c('Sim','Stream','Offset','value')
coef.list$Offset <- as.factor(paste("-",coef.list$Offset))

#Remove Zeros
loc.zero <- which(ind.prod.list$value==0)
coef.samps <- coef.list[-loc.zero,]

#Plot coefficients
g2 <- ggplot(coef.samps, aes(x=Stream, y=value, fill=Stream)) +
       theme_gray() +
       facet_wrap(~Offset) +
       # coord_flip() +
       geom_boxplot(outlier.shape=NA) +
       ylab('Max Stream Temperature Coefficient')
       guides(fill=FALSE)
g2
ggsave('Plots/Temp Coef.png', height=7, width=8, dpi=500, units='in')


#Plot model fits
dim(out$BUGSoutput$sims.list$pred)

par(mfrow=c(4,3), oma=c(2,2,0,1), mar=c(2,4,0,0))
s <- 1
for(s in 1:n.streams) {
  locs <- 1:n.yrs[s]
  
  #Predicted
  temp.pred <- out$BUGSoutput$sims.list$pred[,s,locs]
  med <- apply(temp.pred, 2, median)
  
  y.lim <- c(min(temp.pred,std.stream.runSize[s,locs]), max(temp.pred,std.stream.runSize[s,locs]))
  plot(x=stream.yrs[s,locs], y=std.stream.runSize[s,locs], type='l', ylim=y.lim, col=rgb(0,0,1,alpha=0.5), ylab=streams[s])
  points(x=stream.yrs[s,locs], y=std.stream.runSize[s,locs], pch=21, bg='blue')
  

  
  lines(x=stream.yrs[s,locs], y=med, col=rgb(1,0,0, alpha=0.5), lwd=3)
}#next s

stock <- 6
temp.pred <- out$BUGSoutput$sims.list$pred[,stock,(1:n.yrs[stock])]
# temp.pred <- out$BUGSoutput$sims.list$pred[,stock,]

y.lim <- c(min(temp.pred,std.stream.runSize[stock,(1:n.yrs[stock])]), max(temp.pred,std.stream.runSize[stock,(1:n.yrs[stock])]))

boxplot(temp.pred, ylim=y.lim)
points(x=c(1:n.yrs[stock]), y=std.stream.runSize[stock,(1:n.yrs[stock])], pch=4, col='red')

# ggmcmc(ggs(out.mcmc))




