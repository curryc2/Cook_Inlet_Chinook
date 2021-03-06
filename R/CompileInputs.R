# CompileInputs.R
# Erik Schoen
# eschoen@alaska.edu
# 4-1-2019

# Compile all inputs for hierarchical bayesian stock-recruit model: environmental
# covariates and stock-recruit data. Index covariates to the corresponding brood
# year and standardize them. Test for correlation among covariates and
# spawning abundance

# # Load packages and read in data-------------

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(ggplot2)
# library(cowplot)
# library(corrplot)
library(GGally)

setwd("~/Desktop/Cook Inlet Chinook/Analysis")
# temperature, precip, and discharge predictors (compiled by us)
covarsFW <- read_csv("./data/covars.csv") 
# breakup timing index (compiled by us)
breakupRaw <- read_csv("./data/Breakup.csv")
# monthly npgo index (downloaded from Emanuele Di Lorenzo's website)
npgoRaw <- read_csv("./data/NPGO.csv")
# annual mean npgo index (from Curry's data)
npgoCC <- read_csv("./JAGS/Data/10.25.18_update/covars.list.csv")
spawnersRecruits <- read_csv("./data/SpawnersRecruits.csv")
theme_set(theme_bw(12))

# Set salmon population names for figures
siteNames <- c("Alexander", "Anchor", "Campbell", "Chuitna", "Chulitna", "Crooked", "Deep",
               "Deshka", "Kenai late run", "Little Susitna", "Little Willow", "Montana", "Ninilchik",
               "Theodore", "Willow")


# Index spawning indicators to brood year----------
covarsSpawn <- covarsFW %>%
  select(Year, Site, Population,
         maxT_spawn = maxWkJA,
         wksGT13_spawn = wksGT13,
         maxP_spawn = ASON_max,
         RB_spawn = RB_ASON) %>%
  mutate(BroodYear = Year,
         Population = factor(Population, labels = siteNames)) %>%
  select(-Year, -Site)

# Index juvenile rearing indicators to brood year + 1----------
covarsRear <- covarsFW %>%
  select(Year, Site, Population,
         avgT_rear = meanWkJJA,
         wksGT15_rear = wksGT15,
         avgP_rear = MJJA_avg,
         medianQ_rear = MDIS_MJJA,
         RB_emerge = RB_MJ) %>%
  mutate(BroodYear = Year - 1,
         Population = factor(Population, labels = siteNames)) %>% 
  select(-Year, -Site)

# Index breakup to brood year + 2 ----------
breakup <- breakupRaw %>%
  mutate(BroodYear = Year - 2) %>%
  select(BroodYear, breakup = BreakupDOY)

# Summarize NPGO data and index to brood year + 2---------
npgo <- npgoRaw %>%
  rename(Year = YEAR, Month = MONTH) %>%
  group_by(Year) %>%
  summarize(NPGO = mean(NPGO)) %>%
  mutate(BroodYear = Year - 2)

# Compare NPGO values from Di Lorenzo's website against those from Curry's data
# npgoCompare <- npgoCC %>%
#   filter(Covar == "NPGO") %>%
#   select(Year, NPGO_cc = value) %>%
#   left_join(npgo, by = "Year") %>%
#   rename(NPGO_edl = NPGO)
# 
# plot(npgoCompare$NPGO_edl, npgoCompare$NPGO_cc)
# 
# npgoCompareLong <- npgoCompare %>%
#   gather(NPGO_edl, NPGO_cc, key = "Variable", value = "NPGO") %>%
#   mutate(Source = ifelse(Variable == "NPGO_edl", "Di Lorenzo", "Cunningham"))
# 
# These are not identical, or even directly proportional. WTF?? Try comparing
# the time series
# 
# npgoCompareTimeseries <- ggplot(data = npgoCompareLong, 
#                                   aes(x = Year, y = NPGO, color = Source)) +
#   geom_line()
# npgoCompareTimeseries
# Hmmm, they are closely related but not the same. Is Curry's data summarized in 
# a different way than by calendar year? E.g. by brood year??

# Use Di Lorenzo's data going forward
npgo <- npgo %>%
  select(-Year)


# Plot standardized values for regional and oceanic-scale indicators----------
# First generate data frame of regional and oceanic-scale indicators, in long format
large.scale.indicators <- covarsSpawn %>%
  left_join(covarsRear, by = "BroodYear") %>%
  left_join(breakup, by = "BroodYear") %>%
  left_join(npgo, by = "BroodYear") %>%
  select(BroodYear, medianQ_rear, RB_spawn, RB_emerge, breakup, NPGO) %>%
  distinct() %>%
  filter(BroodYear < 2010) %>%
  # Standardize each indicator
  mutate(medianQ_rear.std = scale(medianQ_rear),
          RB_spawn.std = scale(RB_spawn),
          RB_emerge.std = scale(RB_emerge),
          breakup.std = scale(breakup),
          NPGO.std = scale(NPGO)) %>%
  # Reshape into long format for plotting
  select(BroodYear, medianQ_rear.std:NPGO.std) %>%
  gather(medianQ_rear.std:NPGO.std, key = "Covariate", value = "Value") %>%
  # Turn covariate names into a factor with proper labels and order for plotting
  mutate(Covariate = factor(Covariate, levels = c("medianQ_rear.std", "RB_spawn.std", 
                                                  "RB_emerge.std",
                                                  "breakup.std", "NPGO.std"),
                            labels = c("medianQ_rear", "RB_spawn", 
                                       "RB_emerge",
                                       "breakup", "NPGO")))

large.scale.indicators.plot <- ggplot(data = large.scale.indicators, 
                                      aes(x = BroodYear, y = Value)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "red", lty = "dashed") +
  facet_wrap(Covariate~., ncol = 1, scales = "free_y") +
  scale_x_continuous(name = "Brood Year") +
  scale_y_continuous(name = "Standardized covariate value (SD)", 
                     breaks = seq(from = -4, to = 4, by = 1))
large.scale.indicators.plot
ggsave("./figs/large.scale.indicators.plot.png", height = 6, width = 5)

npgo.std <- large.scale.indicators %>%
  filter(Covariate == "NPGO.std")
large.scale.indicators.plot <- ggplot(data = covarsSR, aes(x = BroodYear, y = Value)) +
  geom_line()
large.scale.indicators.plot
# Join all covariates to stock-recruit data by brood year and standardize----------

covarsSR <- spawnersRecruits %>%
  drop_na(CoreRecruitsPerSpawner) %>%
  mutate(Population = factor(Population, labels = siteNames)) %>%
  select(Population, BroodYear, EscapementMethod, Spawners = Spawners2.nom, 
         CoreRecruits = CoreRecruits2.nom, CoreRecruitsPerSpawner) %>%
  left_join(covarsSpawn, by = c("Population", "BroodYear")) %>%
  left_join(covarsRear, by = c("Population", "BroodYear")) %>%
  left_join(breakup, by = "BroodYear") %>%
  left_join(npgo, by = "BroodYear") %>%

  # Standardize temperature covariates across all populations and brood years
  mutate(maxT_spawn.std.all = scale(maxT_spawn),
         avgT_rear.std.all = scale(avgT_rear),
         wksGT13_spawn.std.all = scale(wksGT13_spawn),
         wksGT15_rear.std.all = scale(wksGT15_rear)) %>%
  # Standardize all covariates within each population (across years)
  # Also standardize spawning abundance within each popn to show in corr plot
  group_by(Population) %>%
  mutate(maxT_spawn.std = scale(maxT_spawn),
         avgT_rear.std = scale(avgT_rear),
         wksGT13_spawn.std = scale(wksGT13_spawn),
         wksGT15_rear.std = scale(wksGT15_rear),
         maxP_spawn.std = scale(maxP_spawn),
         avgP_rear.std = scale(avgP_rear),
         medianQ_rear.std = scale(medianQ_rear),
         RB_spawn.std = scale(RB_spawn),
         RB_emerge.std = scale(RB_emerge),
         breakup.std = scale(breakup),
         NPGO.std = scale(NPGO),
         Spawners.std = scale(Spawners)) %>%
  select(Population:Spawners, Spawners.std, CoreRecruits:NPGO.std) %>%
  # Replace NAs in "wksGTx" standardized values with 0s. These occurred in
  # populations that never exceeded the threshold value, resulting in zeros in
  # the denominator. By replacing with 0, we effectively z-score the time series
  # with every year equalling the mean value (0 weeks above the threshold).
  replace_na(list(wksGT13_spawn.std = 0, wksGT15_rear.std = 0))

# Double-check that covariates standardized properly
# Calculate means by population
covarMeansByPop <- covarsSR %>%
  group_by(Population) %>%
  summarize(mean.Spawners.std = mean(Spawners.std),
            mean.maxT_spawn.std.all = mean(maxT_spawn.std.all),
            mean.avgT_rear.std.all = mean(avgT_rear.std.all),
            mean.wksGT13_spawn.std.all = mean(wksGT13_spawn.std.all),
            mean.wksGT15_rear.std.all = mean(wksGT15_rear.std.all),
            mean.maxT_spawn.std = mean(maxT_spawn.std),
            mean.avgT_rear.std = mean(avgT_rear.std),
            mean.wksGT13_spawn.std = mean(wksGT13_spawn.std),
            mean.wksGT15_rear.std = mean(wksGT15_rear.std),
            mean.maxP_spawn.std = mean(maxP_spawn.std),
            mean.avgP_rear.std = mean(avgP_rear.std),
            mean.medianQ_rear.std = mean(medianQ_rear.std),
            mean.RB_spawn.std = mean(RB_spawn.std),
            mean.RB_emerge.std = mean(RB_emerge.std),
            mean.breakup.std = mean(breakup.std),
            mean.NPGO.std = mean(NPGO.std))
# Looks good. Means of temperature covariates are not zero, means of other
# covariates = 0.

# Means across all populations and brood years should equal zero for all covars
covarMeans <- covarsSR %>%
  ungroup() %>%
  summarize(mean.Spawners.std = mean(Spawners.std),
            mean.maxT_spawn.std.all = mean(maxT_spawn.std.all),
            mean.avgT_rear.std.all = mean(avgT_rear.std.all),
            mean.wksGT13_spawn.std.all = mean(wksGT13_spawn.std.all),
            mean.wksGT15_rear.std.all = mean(wksGT15_rear.std.all),
            mean.maxT_spawn.std = mean(maxT_spawn.std),
            mean.avgT_rear.std = mean(avgT_rear.std),
            mean.wksGT13_spawn.std = mean(wksGT13_spawn.std),
            mean.wksGT15_rear.std = mean(wksGT15_rear.std),
            mean.maxP_spawn.std = mean(maxP_spawn.std),
            mean.avgP_rear.std = mean(avgP_rear.std),
            mean.medianQ_rear.std = mean(medianQ_rear.std),
            mean.RB_spawn.std = mean(RB_spawn.std),
            mean.RB_emerge.std = mean(RB_emerge.std),
            mean.breakup.std = mean(breakup.std),
            mean.NPGO.std = mean(NPGO.std))
# Looks good. All equal 0.

# Write compiled data to csv
write_csv(covarsSR, "./data/covarsSR.csv")

# Test for correlations among covariates and spawning abundance
# First, with temperature covariates standardized across all pops
corrData.tempAcrossPops <- covarsSR %>%
  ungroup() %>%
  select(Spawners.std, maxT_spawn.std.all:wksGT15_rear.std.all,
         maxP_spawn.std:NPGO.std) 
ggcorr(corrData.tempAcrossPops, palette = "RdBu", label = T, 
       # adjust covariate name text to make it more readable
       hjust =0.9, layout.exp = 2)
ggsave("./figs/CorrelationMatrix_tempAcrossPops.png", height = 12, width = 13)
# If using temperatures standardized across all populations (i.e., taking into
# account the differences in mean temperature among sites), then all four
# candidate temperature metrics are tightly correlated. So we can only use one 
# of them at a time. Used maxT_spawn because it does not depend on a literature-
# based threshold value, all site-years have non-zero values, and it is more
# ecologically meaningful than avgT_rear.

# Alternative version using corrplot and base R plotting (ggsave doesn't work)
# Variable names are still unreadable
# pdf(file.path("./figs/CovarCorrelations_tempAcrossPops.pdf"), 
#     height = 16, width = 16)
# corrplot.mixed(correlation.tempAcrossPops)
# dev.off()

# Second, with temperature covariates standardized within each pop
corrData.tempWithinPops <- covarsSR %>%
  ungroup() %>%
  select(Spawners.std, maxT_spawn.std:NPGO.std)
ggcorr(corrData.tempWithinPops, palette = "RdBu", label = T, 
       # adjust covariate name text to make it more readable
       hjust =0.9, layout.exp = 2)
ggsave("./figs/CorrelationMatrix_tempWithinPops.png", height = 12, width = 13)
# If using temperatures standardized within each population (i.e., ignoring
# differences in mean temperature among sites): 
# maxT_spawn is tightly correlated with wksGT13_spawn
# avgT_rear is tightly correlated with wksGT15_rear

# Plot temperature metrics raw and standardized both ways

# complete the table, filling NAs for missing brood years to avoid interpolating in plot
covarsSR.plot <- covarsSR %>%
  complete(Population, BroodYear)
maxT_spawn.raw.timeseries <- ggplot(data = covarsSR.plot, aes(x = BroodYear, y = maxT_spawn,
                                                         color = Population)) +
  geom_point() +
  geom_line()  +
  scale_y_continuous(name = "maxT_spawn (˚C)", breaks = seq(8, 22, by = 2))
maxT_spawn.raw.timeseries
ggsave("./figs/maxT_spawn_raw_timeseries.png", height = 8, width = 8)

maxT_spawn.raw.timeseries.mean.sd <- maxT_spawn.raw.timeseries +
  geom_hline(aes(yintercept = mean(covarsSR$maxT_spawn))) +
  geom_hline(aes(yintercept = mean(covarsSR$maxT_spawn)+sd(covarsSR$maxT_spawn)),
             linetype = "dotted") +
  geom_hline(aes(yintercept = mean(covarsSR$maxT_spawn)-sd(covarsSR$maxT_spawn)),
             linetype = "dotted")  
maxT_spawn.raw.timeseries.mean.sd
ggsave("./figs/maxT_spawn_raw_timeseries_mean_SD.png", height = 8, width = 8)


maxT_spawn.std.timeseries <- ggplot(data = covarsSR.plot, aes(x = BroodYear, y = maxT_spawn.std,
                                                              color = Population)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(name = "maxT_spawn (SD; standardized within pops)")
maxT_spawn.std.timeseries
ggsave("./figs/maxT_spawn_std_timeseries.png", height = 8, width = 8)


maxT_spawn.std.all.timeseries <- ggplot(data = covarsSR.plot, aes(x = BroodYear, 
                                                                  y = maxT_spawn.std.all,
                                                                  color = Population)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(name = "maxT_spawn (SD; standardized across pops)")
maxT_spawn.std.all.timeseries
ggsave("./figs/maxT_spawn_std_across_timeseries.png", height = 8, width = 8)

