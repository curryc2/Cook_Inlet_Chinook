# SpawnersRecruits.R
# Erik Schoen
# eschoen@alaska.edu
# 4-2018

# Cook Inlet Chinook project

# Estimate the numbers of spawners by return year and an index of recruits 
# by brood year, using 2 methods

# Method 1: Use escapement data only
# Spawners1 = Escapement Index (aerial counts not scaled up) 
# Recruits1 = Escapement Index (aerial counts not scaled up) + Weir Removals

# Method 2: Use escapement + terminal harvest data
# Spawners2 = Estimated Escapement (aerial counts scaled up)
# Recruits2 = Estimated Escapement (aerial counts scaled up) + Weir Removals 
#             + Terminal Harvest (below enumeration site)
# 3 Estimates for each variable, based on nominal, minimum, and maximum estimates of
# the rate at which spawners are counted in aerial surveys

# Load packages, source scripts, and read in data-------------

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)

theme_set(theme_classic(14))

source("~/Desktop/Cook Inlet Chinook/Analysis/R/AgeComp_estimate.R")
source("~/Desktop/Cook Inlet Chinook/Analysis/R/Harvest_compile.R")
setwd("~/Desktop/Cook Inlet Chinook/Analysis")
escapeFull <- read_csv("./data/Escapement.csv")

# Aerial expansion factors: what fraction of spawners are counted in aerial surveys?
# Source: Oslund et al. 2017 (p. 10) and Adam St. Saviour pers. comm.
aerialExp.min <- 0.3
aerialExp.nom <- 0.45
aerialExp.max <- 0.6

# TODO: check my notes on Ninilchik: are there real data breaking down sport harvest by
# hatchery vs. natural origin?  The spreadsheet from Mike Booz just divides them 50/50
# for some years and 75/25 for other years.  If they don't have data on the breakdown
# can I use the Crooked Creek model based on how many days the fishery was open to 
# retention of natural vs hatchery fish?
# # Expansion factor for Ninilchik River (this proportion of spawning occurs
# # above the weir, so divide escapement by this factor to account for spawning below weir).
# # Source: Mike Booz pers. comm.
# ninExp <- 0.65 

# Spawners-----------
spawnersByRY <- escapeFull %>%
  select(Population, ReturnYear, EscapeIndex = Escapement, WeirRemovals, 
         EscapementMethod) %>%
  filter(ReturnYear > 1979) %>%
  # For aerial survey data, expand Spawners2 using the min, nominal, and max
  # estimates of what fraction of spawners are counted
  mutate(Spawners1 = EscapeIndex,
         Spawners2.min = ifelse(EscapementMethod == "Single aerial survey",
                                round(EscapeIndex / aerialExp.max),
                                EscapeIndex),
         Spawners2.nom = ifelse(EscapementMethod == "Single aerial survey",
                                round(EscapeIndex / aerialExp.nom),
                                EscapeIndex),
         Spawners2.max = ifelse(EscapementMethod == "Single aerial survey",
                                round(EscapeIndex / aerialExp.min),
                                EscapeIndex))
  
# Recruits-----------
# Join the escapement, terminal harvest, and age comp data and the mean age comps 
# (for filling in missing age comps)
recruitsByRYxBY <- spawnersByRY %>%
  left_join(sport, by = c("Population", "ReturnYear" = "Year")) %>%
  left_join(kenai, by = c("Population", "ReturnYear" = "Year")) %>%
  # filter(!is.na(Escapement)) %>%
  # Make 5 rows for each population X return year (1 for each age)
  mutate(Age1.1 = NA, Age1.2 = NA, Age1.3 = NA, Age1.4 = NA, Age1.5 = NA) %>%
  gather(key = "Age", value = "Prop", Age1.1:Age1.5) %>%
  select(-Prop) %>%
  # Join the age data and the global mean age composition
  full_join(ageSimple, by = c("Population", "ReturnYear", "Age")) %>%
  full_join(meanAgeComp, by = c("ReturnYear", "Age")) %>%
  # Remove the marine fisheries (have age comps but no escapement data)
  filter(Population != "NSN" & Population != "ESSN" & Population != "WSSN" & 
           Population != "Tyonek") %>%
  # Now make a bunch of calculations...
  mutate(
          # Use the population-specific age composition data if available, 
          # or the average age comp for that return year if not
         PropEst = if_else(is.na(Prop), PropMean, Prop),
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
         Spawners1ByRYxBY = Spawners1 * PropEst,
         Spawners2.minByRYxBY = Spawners2.min * PropEst,
         Spawners2.nomByRYxBY = Spawners2.nom * PropEst,
         Spawners2.maxByRYxBY = Spawners2.max * PropEst,
         WeirRemovalsByRYxBY = WeirRemovals * PropEst,
         TermHarvestByRYxBY = if_else(
           # For the Kenai late run, add up the 4 terminal fisheries
           Population == "Kenai.Late", rowSums(cbind(EducationalHarvest, 
                                                     SubsistenceHarvest, PUHarvest, 
                                                     SportHarvestBelowSonar,
                                                     SportHarvestAboveSonar, 
                                                     na.rm = T)) * PropEst,
           SportHarvest * PropEst)
  )

# Calculate Spawners1, Spawners2 (min, nominal, and max),
# weir removals, and terminal harvest by brood year.  
recruitsByBrood <- recruitsByRYxBY %>%
  filter(!is.na(Spawners1)) %>%
  group_by(Population, BroodYear) %>%
  summarize(Spawners1 = sum(Spawners1ByRYxBY, na.rm = T),
            Spawners2.min = sum(Spawners2.minByRYxBY, na.rm = T),
            Spawners2.nom = sum(Spawners2.nomByRYxBY, na.rm = T),
            Spawners2.max = sum(Spawners2.maxByRYxBY, na.rm = T),
            WeirRemovals = sum(WeirRemovalsByRYxBY, na.rm = T),
            TermHarvest = sum(TermHarvestByRYxBY, na.rm = T),
            # Use these values to calculate Recruits1 and Recruits2 
            # (min, nominal, and max)
            Recruits1 = round(Spawners1 + WeirRemovals),
            Recruits2.min = round(Spawners2.min + WeirRemovals + TermHarvest),
            Recruits2.nom = round(Spawners2.nom + WeirRemovals + TermHarvest),
            Recruits2.max = round(Spawners2.max + WeirRemovals + TermHarvest),
            # Determine how many age classes with data are included in each brood year
            NumAgeClasses = sum(!is.na(Spawners1ByRYxBY)),
            Top5AgeClasses = NumAgeClasses == 5,
            NumCoreAgeClasses = sum(CoreAgeClass),
            Top3AgeClasses = NumCoreAgeClasses == 3)  %>%
  select(Population, BroodYear, Recruits1:Top3AgeClasses)

# Combine the spawners (by return year) and recruits (by brood year) data into a
  # single dataframe and write to csv
spawnersRecruits <- spawnersByRY %>%
  left_join(recruitsByBrood, by = c("Population", "ReturnYear" = "BroodYear")) %>%
  rename(Year = ReturnYear) %>%
  # Trim years and populations outside the study and remove intermediate variables
  filter(Year > 1979) %>%
  filter(Population != "Funny" & Population != "Slikok" & Population != "Quartz" & 
           Population != "Crooked.Hatchery" & Population != "Ninilchik.Hatchery") %>%
  write_csv("./data/SpawnersRecruits.csv")
  
# Plot the results -----------------

# Make a new versions of each variable "CB" for complete broods, listing values for incomplete 
# broods as NA (but not removing those rows) so ggplot doesn't interpolate across missing years
spawnersRecruitsCB <- spawnersRecruits %>%
  mutate(Spawners1CB = ifelse(Top5AgeClasses, Spawners1, NA),
       Spawners2CB.min = ifelse(Top5AgeClasses, Spawners2.min, NA),
       Spawners2CB.nom = ifelse(Top5AgeClasses, Spawners2.nom, NA),
       Spawners2CB.max = ifelse(Top5AgeClasses, Spawners2.max, NA),
       Recruits1CB = ifelse(Top5AgeClasses, Recruits1, NA),
       Recruits2CB.min = ifelse(Top5AgeClasses, Recruits2.min, NA),
       Recruits2CB.nom = ifelse(Top5AgeClasses, Recruits2.nom, NA),
       Recruits2CB.max = ifelse(Top5AgeClasses, Recruits2.max, NA),
       RecruitsPerSpawner1CB = ifelse(Spawners1CB > 0 ,
                                      Recruits1CB / Spawners1CB, NA),
       RecruitsPerSpawner2CB.nom = ifelse(Spawners2CB.nom > 0 ,
                                           Recruits2CB.nom / Spawners2CB.nom, NA)
       )

# Plot an index of brood year productivity (recruits [escapement + terminal harvest] /
# spawner) for each population over time, using the nominal estimate for visibility of
# spawners to aerial surveys [45%]). Plot years with complete broods only
productivity.lineplot <- ggplot(data = spawnersRecruitsCB, 
                              aes(x = Year, y = RecruitsPerSpawner2CB.nom,
                                  color = Population)) +
  geom_line() +
  geom_hline(yintercept = 1) +
  scale_x_continuous(name = "Brood Year", breaks = seq(1980, 2010, by = 10), 
                     limits = c(1980, 2010)) +
  scale_y_continuous(name = "Productivity index\n(escapement + terminal harvest / spawner)", 
                     labels = scales::comma) 
productivity.lineplot
ggsave("./figs/Brood year productivity index.png")

# Same plot but excluding aerial survey data
spawnersRecruitsCBNoAerial <- filter(spawnersRecruitsCB, 
                                     !str_detect(EscapementMethod, "aerial"))
productivityNoAerial.lineplot <- ggplot(data = spawnersRecruitsCBNoAerial, 
                                        aes(x = Year, y = RecruitsPerSpawner2CB.nom,
                                            color = Population)) +
  geom_line() +
  geom_hline(yintercept = 1) +
  scale_x_continuous(name = "Brood Year", breaks = seq(1980, 2010, by = 10), 
                     limits = c(1980, 2010)) +
  scale_y_continuous(name = "Productivity index\n(escapement + terminal harvest / spawner)", 
                     labels = scales::comma) 
productivityNoAerial.lineplot
ggsave("./figs/Brood year productivity index_no aerial.png")
