# SpawnersRecruits.R
# Erik Schoen
# eschoen@alaska.edu
# 4-2018

# Cook Inlet Chinook project

# Estimate the numbers of spawners and an index of recruits by brood year, 
# using 2 methods

# Method 1: Use escapement data only
# Spawners1 = Escapement Index (aerial counts not scaled up) 
# Recruits1 = Escapement Index (aerial counts not scaled up) + Weir Removals

# Method 2: Use escapement + terminal harvest data
# Spawners2 = Estimated Escapement (aerial counts scaled up)
# Recruits2 = Estimated Escapement (aerial counts scaled up) + Weir Removals 
#             + Terminal Harvest
# 3 Estimates for each variable, based on nominal, minimum, and maximum estimates of
# the rate at which spawners are counted in aerial surveys

# Load packages, source scripts, and read in data-------------

source("~/Desktop/Cook Inlet Chinook/Analysis/R/Harvest_compile.R")

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)

setwd("~/Desktop/Cook Inlet Chinook/Analysis")

theme_set(theme_bw(14))

sites <- read_csv("./data/Sites.csv")
escapeFull <- read_csv("./data/Escapement.csv")
ageSimple <- read_csv("./data/AgeSimple.csv")
agePredicted <- read_csv("./data/AgePredicted.csv")

# Define population names for plotting
siteNames <- c("Alexander", "Anchor", "Campbell", "Chuitna", "Chulitna", "Crooked", "Deep",
               "Deshka", "Kenai late run", "Little Susitna", "Little Willow", "Montana", "Ninilchik",
               "Theodore", "Willow")

# Aerial expansion factors: what fraction of spawners are counted in aerial surveys?
# Source: Oslund et al. 2017 (p. 10) and Adam St. Saviour pers. comm.
aerialExp.min <- 0.3
aerialExp.nom <- 0.45
aerialExp.max <- 0.6

# Reshape age composition data into "wide" format, with one row for each group and return year, and
# different columns for the escapement and sport harvest run components
ageSimpleWide <- ageSimple %>%
  # Rename "ESSN" group as "Kenai.Late" so the empirical ESSN age comps will be used for the 
  # Kenai late run (will need to come up with a more general solution if we end up incorporating
  # mixed stock harvest from other fisheries)
  mutate(Group = ifelse(Group == "ESSN", "Kenai.Late", Group)) %>%
  select(Group, Component, ReturnYear, Age, Prop) %>%
  spread(key = "Component", value = "Prop") %>%
  select(-`Subsistence Harvest`) %>%
  rename(PropEsc = Escapement, PropComm = `Commercial Harvest`, PropSport = `Sport Harvest`)

agePredictedWide <- agePredicted %>%
  select(-zReturnYear) %>%
  spread(key = "Component", value = "Prop") %>%
  rename(PropEscPredicted = Escapement, PropCommPredicted = `Commercial Harvest`, 
         PropSportPredicted = `Sport Harvest`)

# Spawners-----------
spawnersByRY <- escapeFull %>%
  select(Population, ReturnYear, EscapeIndex = Escapement, EscapementMethod,
         WeirRemovals) %>%
  filter(ReturnYear > 1979 & ReturnYear < 2016) %>%
  # For aerial survey data, expand Spawners2 using the min, nominal, and max
  # estimates of what fraction of spawners are counted
  mutate(Spawners1 = EscapeIndex,
         Spawners2.min = ifelse(EscapementMethod == "Aerial survey",
                                round(EscapeIndex / aerialExp.max),
                                EscapeIndex),
         Spawners2.nom = ifelse(EscapementMethod == "Aerial survey",
                                round(EscapeIndex / aerialExp.nom),
                                EscapeIndex),
         Spawners2.max = ifelse(EscapementMethod == "Aerial survey",
                                round(EscapeIndex / aerialExp.min),
                                EscapeIndex))

# Trim down the dataset for plotting
# Remove populations with < 10 years of escapement data
escapeYears <- spawnersByRY %>%
  filter(!is.na(Spawners2.nom)) %>%
  group_by(Population) %>%
  summarize(nYears = n()) %>%
  mutate(Include = nYears > 10)

spawnersByRY_trimmed <- spawnersByRY %>%
  left_join(escapeYears, by = "Population") %>%
  filter(Include == T) %>%
  # Remove hatchery-origin fish
  filter(Population != "Crooked.Hatchery" & Population != "Ninilchik.Hatchery") 

# Plot the trends in escapement by return year
EscapementAllPops.plot <- ggplot(data = spawnersByRY_trimmed,
                          aes(x = ReturnYear, y = Spawners2.nom / 1000)) +
  geom_line() +
  geom_point(size = 1) + # Plotting points as well as lines to show isolated points with NA on either side
  scale_y_continuous(name = "Escapement (1,000s)") +
  scale_x_continuous(name = "Return Year", breaks = seq(1980, 2015, 10)) +
  # facet_wrap(.~Population, scales = "fixed")
  facet_wrap(.~Population, scales = "free_y") +
  expand_limits(y = 0) +
  theme_bw(12)
EscapementAllPops.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/figs/Escapement_all pops.png",
       width = 8, height = 6)

# Make same plot for study populations only
spawnersByRY_studyPops <- spawnersByRY_trimmed %>%
  left_join(sites, by = c("Population" = "Group")) %>%
  filter(StockRecruit == T) %>%
  mutate(Population = factor(Population, labels = siteNames))

EscapementStudyPops.plot <- ggplot(data = spawnersByRY_studyPops,
                          aes(x = ReturnYear, y = Spawners2.nom / 1000)) +
  geom_line() +
  geom_point(size = 1) + # Plotting points as well as lines to show isolated points with NA on either side
  scale_y_continuous(name = "Escapement (1,000s)") +
  # scale_y_continuous(name = "Escapement (1,000s)", labels = scales::comma) +
  scale_x_continuous(name = "Return Year", breaks = seq(1980, 2015, 10)) +
  # facet_wrap(.~Population, scales = "fixed")
  facet_wrap(.~Population, scales = "free_y", ncol = 3) +
  expand_limits(y = 0) +
  theme_bw(12)
EscapementStudyPops.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/figs/Escapement_study pops.png",
       width = 6, height = 6)

# How much did escapement vary (%) between best and worst years, by population?
escapeVariability <- spawnersByRY_trimmed %>%
  group_by(Population) %>%
  summarize(nYears = n(),
            maxEsc = max(Spawners2.nom, na.rm = T),
            minEsc = min(Spawners2.nom, na.rm = T),
            sdEsc = sd(Spawners2.nom, na.rm = T),
            meanEsc = mean(Spawners2.nom, na.rm = T)) %>%
  mutate(cvEsc = sdEsc / meanEsc,
         variabilityEsc = maxEsc / minEsc)

# Recruits-----------
# Join the escapement, terminal harvest, and age comp data and the mean age comps 
# (for filling in missing age comps)
recruitsByRYxBY <- spawnersByRY_studyPops %>%
  filter(StockRecruit == T) %>%
  left_join(sport, by = c("Population", "ReturnYear" = "Year")) %>%
  left_join(kenai, by = c("Population", "ReturnYear" = "Year")) %>%
  left_join(mixedStockHarvestByPop, by = c("Population", "ReturnYear" = "Year")) %>%
  # Make 5 rows for each population X return year (1 for each age)
  mutate(Age1.1 = NA, Age1.2 = NA, Age1.3 = NA, Age1.4 = NA, Age1.5 = NA) %>%
  gather(key = "Age", value = "Prop", Age1.1:Age1.5) %>%
  select(-Prop) %>%
  # Join the age data and the global mean age composition
  # left_join(sites, by = c("Population" = "Group")) %>%
  left_join(ageSimpleWide, by = c("Population" = "Group", "ReturnYear", "Age")) %>%
  left_join(agePredictedWide, by = c("Region", "ReturnYear", "Age")) %>%
  # Remove the mixed-stock fisheries (have age comps but no escapement data)
  filter(Population != "NSN" & Population != "ESSN" & Population != "WSSN" & 
           Population != "Tyonek") %>%
  # Now make a bunch of calculations...
  mutate(
          # Use the population- and component-specific age composition data if available, 
          # or the predictions from the MLR age-composition model if not
         PropEscEst = if_else(is.na(PropEsc), PropEscPredicted, PropEsc), # Age comp of escapement
         PropSportEst = if_else(is.na(PropSport), PropSportPredicted, PropSport), # Age comp of sport harvest
         PropCommEst = if_else(is.na(PropComm), PropCommPredicted, PropComm), # Age comp of comm harvest
         # Calculate various age metrics in years
         AgeFW = as.numeric(str_sub(Age, start = 4, end = 4)),
         AgeSW = as.numeric(str_sub(Age, start = 6, end = 6)),
         AgeYr = AgeFW + AgeSW + 1,
         CoreAgeClass = if_else(AgeYr == 4 | AgeYr == 5 | AgeYr == 6, 1, 0),
         BroodYear = ReturnYear - AgeYr,
         # Calculate Spawners1, Spawners2 (min, nominal, and max),
         # weir removals, and terminal harvest (below and above the site where 
         # escapement is estimated, if applicable) for each combination of 
         # Population, ReturnYear, and BroodYear
         Spawners1ByRYxBY = Spawners1 * PropEscEst,
         Spawners2.minByRYxBY = Spawners2.min * PropEscEst,
         Spawners2.nomByRYxBY = Spawners2.nom * PropEscEst,
         Spawners2.maxByRYxBY = Spawners2.max * PropEscEst,
         WeirRemovalsByRYxBY = WeirRemovals * PropEscEst,
         TermHarvestByRYxBY = if_else(
           # For the Kenai late run, add up the 4 terminal fisheries
           Population == "Kenai.Late", rowSums(cbind(EducationalHarvest, 
                                                     SubsistenceHarvest, PUHarvest, 
                                                     SportHarvestBelowSonar,
                                                     SportHarvestAboveSonar, 
                                                     na.rm = T)) * PropSportEst,
           SportHarvest * PropSportEst),
         MixedStockHarvestByRYxBY = MixedStockHarvest * PropCommEst
  )

# Calculate spawners and recruits from the 5 major age classes (1.1, 1.2, 1.3, 1.4, 1.5)
# Calculate Spawners1, Spawners2 (min, nominal, and max),
# weir removals, and terminal harvest by brood year.  
recruitsByBrood <- recruitsByRYxBY %>%
  filter(!is.na(Spawners1)) %>%
  group_by(Population, BroodYear) %>%
  summarize(Spawners1 = sum(Spawners1ByRYxBY),
            Spawners2.min = sum(Spawners2.minByRYxBY),
            Spawners2.nom = sum(Spawners2.nomByRYxBY),
            Spawners2.max = sum(Spawners2.maxByRYxBY),
            WeirRemovals = sum(WeirRemovalsByRYxBY, na.rm = T),
            TermHarvest = sum(TermHarvestByRYxBY, na.rm = T),
            MixedStockHarvest = sum(MixedStockHarvestByRYxBY, na.rm = T),
            
            # Use these values to calculate Recruits1 and Recruits2 
            # (min, nominal, and max)
            Recruits1 = round(Spawners1 + WeirRemovals),
            Recruits2.min = round(Spawners2.min + WeirRemovals + TermHarvest + 
                                    MixedStockHarvest),
            Recruits2.nom = round(Spawners2.nom + WeirRemovals + TermHarvest + 
                                    MixedStockHarvest),
            Recruits2.max = round(Spawners2.max + WeirRemovals + TermHarvest + 
                                    MixedStockHarvest),
            # Remove recruit estimates for brood years lacking data from any of the
            # 5 major age classes
            NumAgeClasses = sum(!is.na(Spawners1ByRYxBY)))  %>%
  filter(NumAgeClasses == 5) %>%
  select(Population, BroodYear, Recruits1:NumAgeClasses)

# Calculate "core recruits", i.e., recruits from the 3 top age classes (1.2, 1.3, 1.4)
# First, calculate Spawners1, Spawners2 (min, nominal, and max),
# weir removals, and terminal harvest by brood year.  
coreRecruitsByBrood <- recruitsByRYxBY %>%
  filter(!is.na(Spawners1)) %>%
  filter(Age != "Age1.1" & Age != "Age1.5") %>%
  group_by(Population, BroodYear) %>%
  summarize(Spawners1 = sum(Spawners1ByRYxBY, na.rm = T),
            Spawners2.min = sum(Spawners2.minByRYxBY, na.rm = T),
            Spawners2.nom = sum(Spawners2.nomByRYxBY, na.rm = T),
            Spawners2.max = sum(Spawners2.maxByRYxBY, na.rm = T),
            WeirRemovals = sum(WeirRemovalsByRYxBY, na.rm = T),
            TermHarvest = sum(TermHarvestByRYxBY, na.rm = T),
            MixedStockHarvest = sum(MixedStockHarvestByRYxBY, na.rm = T),
            # Use these values to calculate CoreRecruits1 and CoreRecruits2 
            # (min, nominal, and max)
            CoreRecruits1 = round(Spawners1 + WeirRemovals),
            CoreRecruits2.min = round(Spawners2.min + WeirRemovals + TermHarvest + 
                                        MixedStockHarvest),
            CoreRecruits2.nom = round(Spawners2.nom + WeirRemovals + TermHarvest + 
                                        MixedStockHarvest),
            CoreRecruits2.max = round(Spawners2.max + WeirRemovals + TermHarvest + 
                                        MixedStockHarvest),
            NumCoreAgeClasses = sum(CoreAgeClass)) %>%
  filter(NumCoreAgeClasses == 3) %>%
  select(Population, BroodYear, CoreRecruits1:NumCoreAgeClasses)

# Combine the spawners (by return year) and recruits (by brood year) data into a
  # single dataframe and write to csv
spawnersRecruits <- spawnersByRY_studyPops %>%
  rename(BroodYear = ReturnYear) %>%
  left_join(recruitsByBrood, by = c("Population", "BroodYear")) %>%
  left_join(coreRecruitsByBrood, by = c("Population", "BroodYear")) %>%
  # Trim years and populations outside the study and remove intermediate variables
  filter(BroodYear > 1979 & BroodYear < 2010) %>%
  filter(Population != "Funny" & Population != "Slikok" & Population != "Quartz" & Population != 
           "Lewis" & Population != "Crooked.Hatchery" & Population != "Ninilchik.Hatchery") %>%
  
  # Calculate 2 indices of brood year productivity:
  # recruits [escapement + terminal harvest] / spawner (including top 5 age classes)
  # and core recruits [escapement + terminal harvest] / spawner (including top 3 age classes).
  # Both indices use the nominal estimate of aerial survey visibility
  mutate(RecruitsPerSpawner = ifelse(Spawners2.nom > 0 ,
                                          Recruits2.nom / Spawners2.nom, NA),
         CoreRecruitsPerSpawner = ifelse(Spawners2.nom > 0,
                                              CoreRecruits2.nom / Spawners2.nom, NA)
  ) %>%
  select(Population:Spawners2.max, Recruits1:CoreRecruitsPerSpawner) %>%
  write_csv("./data/SpawnersRecruits.csv")

# Evaluate how well core-age recruits predicted all recruits, and how many additional
# population-brood years of data we gained by analyzing core-age recruits
coreAge.lm <- lm(Recruits2.nom ~ CoreRecruits2.nom, data = spawnersRecruits)
summary(coreAge.lm)

# Call:
#   lm(formula = Recruits2.nom ~ CoreRecruits2.nom, data = spawnersRecruits)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1583.1   -42.7    -0.8    33.6  3506.2 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -36.089855  28.310469  -1.275    0.203    
# CoreRecruits2.nom   1.033512   0.001977 522.802   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 429.4 on 299 degrees of freedom
# (68 observations deleted due to missingness)
# Multiple R-squared:  0.9989,	Adjusted R-squared:  0.9989 
# F-statistic: 2.733e+05 on 1 and 299 DF,  p-value: < 2.2e-16

nRecruits <- spawnersRecruits %>%
  filter(!is.na(Recruits2.nom)) %>%
  summarize(nRecruits = n())
# 301 population-years of recruits (ages 1-5) / spawner
nCoreRecruits <- spawnersRecruits %>%
  filter(!is.na(CoreRecruits2.nom)) %>%
  summarize(nCoreRecruits = n())
# 332 population-years of core-age recruits (ages 2-4) / spawner

# Plot the results -----------------

# Remove site-years with NA values for recruits or spawners (messes up axis scaling)
spawnersRecruits.no.nas <- spawnersRecruits %>%
  filter(!is.na(CoreRecruits2.nom) & !is.na(Spawners2.nom))

# Plot core-age recruitment as a function of spawning abundance
rps.plot <- ggplot(data = spawnersRecruits.no.nas, aes(x = Spawners2.nom/1000, 
                                                y = CoreRecruits2.nom/1000,
                                                color = BroodYear)) +
  geom_point() +
  facet_wrap(.~Population, scales = "free", ncol = 3) +
  expand_limits(y = 0) +
  theme_bw(12) +
  scale_x_continuous(name = "Spawning abundance (thousands)") +
  scale_y_continuous(name = "Recruitment (thousands)") +
  scale_color_continuous()
rps.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/figs/Recruits_vs_Spawners.png",
       width = 8, height = 8)

# Plot natural log of core-age recruits per spawner regressed against spawner abundance
logRPSvSpawners.plot <- ggplot(data = spawnersRecruits.no.nas, aes(x = Spawners2.nom/1000, 
                                                y = log(CoreRecruitsPerSpawner),
                                                color = BroodYear)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(.~Population, scales = "free", ncol = 3) +
  expand_limits(y = 0) +
  theme_bw(12) +
  scale_x_continuous(name = "Spawning abundance (thousands)") +
  scale_y_continuous(name = "ln(Recruits/Spawner)") +
  scale_color_continuous()
logRPSvSpawners.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/figs/LogRPS_vs_Spawners.png",
       width = 8, height = 8)

# Plot an index of brood year productivity (recruits [escapement + terminal harvest] /
# spawner) for each population over time, using the nominal estimate for visibility of
# spawners to aerial surveys [45%]). Plot years with complete broods only
# productivity <- ggplot(data = spawnersRecruits, 
#                               aes(x = BroodYear, y = RecruitsPerSpawner)) +
#   geom_line() +
#   geom_point(size = 1) + # Plotting points as well as lines to show isolated points with NA on either side
#   geom_hline(yintercept = 1) +
#   scale_x_continuous(name = "Brood Year", breaks = seq(1980, 2010, by = 10), 
#                      limits = c(1980, 2010)) +
#   scale_y_continuous(name = "Recruits / Spawner") +
#   facet_wrap(.~Population, scales = "free_y") +
#   expand_limits(y = 0)
# productivity
# ggsave("./figs/Recruits per spawner.png", width = 8, height = 6)
# 
# coreProductivity <- ggplot(data = spawnersRecruits, 
#                           aes(x = BroodYear, y = CoreRecruitsPerSpawner)) +
#   geom_line() +
#   geom_point(size = 1) + # Plotting points as well as lines to show isolated points with NA on either side
#   geom_hline(yintercept = 1) +
#   scale_x_continuous(name = "Brood Year", breaks = seq(1980, 2010, by = 10), 
#                      limits = c(1980, 2010)) +
#   scale_y_continuous(name = "Core-Age Recruits / Spawner") +
#   facet_wrap(.~Population, scales = "free_y") +
#   expand_limits(y = 0)
# coreProductivity
# ggsave("./figs/Core-age recruits per spawner.png", width = 8, height = 6)
