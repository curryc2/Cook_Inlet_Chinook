# ExplainDecline.R
# 6-20-2019
# Erik Schoen
# eschoen@alaska.edu

# Cook Inlet Chinook project

# Quantify and plot what proportion of the recent Chinook salmon population declines in Cook Inlet
# were explained by density dependence alone and density dependence + environmental covariates

# Load packages and read in data---------------------

library(tidyverse)

setwd("~/Desktop/Cook Inlet Chinook/Analysis")
covarsSR <- read_csv("./data/covarsSR.csv")
output <- readRDS("./JAGS/Output/LinearRicker_04.11.19/Phase_4/out.rds")

# Set population names for plots
pops <- c("Alexander", "Anchor", "Campbell", "Chuitna", "Chulitna", "Crooked", "Deep",
          "Deshka", "Kenai late run", "Little Susitna", "Little Willow", "Montana", "Ninilchik",
          "Theodore", "Willow")

theme_set(theme_bw(12))

# Quantify the decline: how much (%) did recruitment decline compared to long-term average?-------
SR <- covarsSR %>%
  select(Population, BroodYear, Spawners, CoreRecruits, CoreRecruitsPerSpawner)

# Calculate long-term mean recruitment for each population
recruitsPreDecline <- SR %>%
  filter(BroodYear < 2003) %>%
  group_by(Population) %>%
  summarize(n.pre = n(),
            meanRecruits.pre = mean(CoreRecruits))

# Calculate mean recruitment of 2003-2007 broods for each population
recruitsPostDecline <- SR %>%
  filter(between(BroodYear, 2003, 2007)) %>%
  group_by(Population) %>%
  summarize(n.post = n(),
            meanRecruits.post = mean(CoreRecruits))

# How much did observed recruitment decline during 2003-2007 compared to long-term mean
decline <- left_join(recruitsPreDecline, recruitsPostDecline, by = "Population") %>%
  mutate(decline.obs = (meanRecruits.pre - meanRecruits.post)/meanRecruits.pre)

# How much of the decline was explained by density dependence (spawning abundance) alone?-------
# Fit simple linear Ricker models (no covariates) for each population 

# Fit linearized simple Ricker models (no environmental covariates) for each population
SR <- SR %>%
  mutate(ln.CoreRPS = log(CoreRecruitsPerSpawner))

rickers <- lm(ln.CoreRPS ~ Population + Population:Spawners, data = SR)
# return the model predictions
SR$ln.CoreRPS.predDD <- predict(rickers)

# Convert predictions from ln(R/S) to recruits
SR <- SR %>%
  mutate(CoreRecruitsPerSpawner.predDD = exp(ln.CoreRPS.predDD),
         CoreRecruits.predDD = CoreRecruitsPerSpawner.predDD * Spawners)

# Plot predicted vs observed recruits for each population
recruits.predDDVobs <- ggplot(data = SR, aes(x = CoreRecruits, y = CoreRecruits.predDD)) +
  geom_point() +
  facet_wrap(.~Population, scales = "free", ncol = 3) +
  scale_x_continuous(name = "Core Recruits (observed)") +
  scale_y_continuous(name = "Core Recruits (predicted from density dependence alone)") 
recruits.predDDVobs
# Looks like density dependence explains some of the variability but there's lots of
# unexplained variability remaining

# Calculate long-term mean predicted recruitment for each population
recruitsPreDecline.predDD <- SR %>%
  filter(BroodYear < 2003) %>%
  group_by(Population) %>%
  summarize(meanRecruits.pre.predDD = mean(CoreRecruits.predDD))

# Calculate mean predicted recruitment of 2003-2007 broods for each population
recruitsPostDecline.predDD <- SR %>%
  filter(between(BroodYear, 2003, 2007)) %>%
  group_by(Population) %>%
  summarize(meanRecruits.post.predDD = mean(CoreRecruits.predDD))

declineExplained.DD <- left_join(decline, recruitsPreDecline.predDD, by = "Population") %>%
  left_join(recruitsPostDecline.predDD, by = "Population") %>%
  mutate(
    # How much did predicted recruitment decline during 2003-2007 compared to long-term mean
         decline.predDD = (meanRecruits.pre.predDD - meanRecruits.post.predDD) /
            meanRecruits.pre.predDD,
    # What fraction of the observed decline in recruitment was explained by density dependence alone?
         explainedByDD = decline.predDD / decline.obs)

# How much of the decline was explained by density dependence + environmental covariates?-------
# Extract predicted recruitment from model output
pred.rec <- output$BUGSoutput$mean$pred.rec

# Transform into a dataframe with populations and brood years
recruits.predDDCov <- pred.rec %>%
  as.data.frame() %>%
  bind_cols(as.data.frame(pops))

# TODO Holy shit how do I line up the brood years? This looks like a pain in the azz
  

# Calculate long-term mean predicted recruitment for each population

# Calculate mean predicted recruitment of 2003-2007 broods for each population

# Summarize results-----------------
# What was the mean, min, and max decline (%) in recruitment from the long-term average
# (broods pre-2003) to the 2003-2007 broods
declineSummary <- declineExplained.DD %>%
  summarize(meanDecline.obs = mean(decline.obs),
            minDecline.obs = min(decline.obs),
            maxDecline.obs = max(decline.obs),
            meanDecline.predDD = mean(decline.predDD),
            minDecline.predDD = min(decline.predDD),
            maxDecline.predDD = max(decline.predDD))