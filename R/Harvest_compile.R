# Harvest_compile.R
# Erik Schoen
# eschoen@alaska.edu
# 4-2018

# Compile, QA, and summarize harvest data for Cook Inlet Chinook salmon

# Load packages and read in data-------------
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)
library(stringr)
library(GGally)

theme_set(theme_classic(14))
setwd("~/Desktop/Cook Inlet Chinook/Analysis")

# Read in data
commRaw <- read_csv("./data/CommHarvest.csv")
sportRaw <- read_csv("./data/SportHarvest.csv") %>%
  mutate(KSP = as.numeric(KSP),
         KSA = as.numeric(KSA))
sportAreas <- read_csv("./data/SportHarvest_Areas.csv")
puOld <- read_csv("./data/PUHarvest_pre1996.csv")
puNew <- read_csv("./data/PUHarvest_post1996.csv")
sub <- read_csv("./data/SubHarvest.csv")
kenai <- read_csv("./data/KenaiLateRun.csv")
crooked <- read_csv("./data/CrookedCreek.csv")

# Munge and clean up data--------------------

# Commercial------------------
comm <- commRaw %>%
  select(-Reference, -Page) %>%
  gather(key = "Fishery", value = "Harvest", 2:5)

# Generate a time series of total king salmon commercial harvest in Cook Inlet
commHarvestByYear <- comm %>%
  group_by(Year) %>%
  summarize(Commercial = sum(Harvest))

# Sport---------------
# Select the Chinook data and join with population table
sportFull <- sportRaw %>%
  select(Year:KSA, KS) %>%
  left_join(sportAreas, by = c("Region", "Area.Fished"))

# Generate a time series of total king salmon sport harvest in fresh waters of Cook Inlet
sportHarvestByYear <- sportFull %>%
  filter(Value == "Estimates" & Type == "Freshwater") %>%
  group_by(Year) %>%
  summarize(SportFreshwater = sum(KS, na.rm = T))

# Filter out just our study streams and summarize multiple sites / population
sportSimple <- sportFull %>%
  filter(!is.na(Population) & Value == "Estimates") %>%
  group_by(Population, Year) %>%
  summarize(SportHarvest = sum(KS, na.rm = T))

# Compile the sport harvest data for the Kenai late run (from Begich et al. 2017)
sportKenaiLate <- kenai %>%
  mutate(SportHarvest = rowSums(cbind(SportHarvestBelowSonar, SportHarvestAboveSonar), 
                                na.rm = T)) %>%
  select(Population, Year, SportHarvest) %>%
  filter(Year < 2016) # remove 0 for 2016 (actually no data)

# Can we estimate the sport harvest of natural-origin Chinook in the Crooked Creek stock?
# The sport harvest data are broken down by hatchery vs. natural origin for only a subset of
# years.  I looked at whether the ratio of natural origin Chinook / total Chinook in the harvest
# was similar to the ratio in weir counts, but it wasn't even close.
# Conclusion: we don't have a reliable way to estimate the harvest of natural-origin Chinook
# for the entire time series, so we should exlcude this population from the sport harvest
# analysis
# sportCrooked <- crooked %>%
#   filter(Year > 2003) %>% # Filter out data pre-2004 (no distinction btw hatchery- and natural-
#   # origin Chinook)
#   mutate(SportHarvest = ifelse(!is.na(Harvest_natural), Harvest_natural, 
#                                Harvest_total * WeirCount_natural / WeirCount_total),
#          HarvestRatio = Harvest_natural / Harvest_total,
#          WeirRatio = WeirCount_natural / WeirCount_total)

# Combine the Kenai late run sport harvest data with the data for the other populations
sport <- bind_rows(sportSimple, sportKenaiLate) %>%
  filter(Population != "Kenai" & Population != "Crooked/Kasilof" & Population != "Crooked")

# Clean up data for populations lacking a complete timeseries in the Statewide Harvest Survey.
# Harvest can be assumed to be 0 in some population X year combinations when sport fishing was
# closed (closed rivers are generally not included in the survey).
# Closed rivers: Alexander 2008-2015, Campbell 1980-1982, Chuitna 1980-1983 and 2010-2015
# Lewis 1980-1983 and 1996-2001 and 2010-2015, Theodore 1980-1983 and 1997-1998 and 2010-2015
# (Source: ADF&G 2011. Chuitna River, Theodore River, and Lewis River King Salmon Stock Status
# and Action Plan, 2011. Report to Alaska Board of Fisheries.)

sportWide <- spread(sport, key = Population, value = SportHarvest)

sportWideCorr <- sportWide %>%
  mutate(Alexander = ifelse(Year > 2007, 0, Alexander),
         Campbell = ifelse(Year < 1983, 0, Campbell),
         Chuitna = ifelse(Year < 1984, 0, Chuitna),
         Chuitna = ifelse(Year > 2009, 0, Chuitna),
         Lewis = ifelse(Year < 1984, 0, Lewis),
         Lewis = ifelse(Year > 2009, 0, Lewis),
         Lewis = ifelse(between(Year, 1996, 2002), 0, Lewis),
         Theodore = ifelse(Year < 1984, 0, Theodore),
         Theodore = ifelse(Year > 2009, 0, Theodore),
         Theodore = ifelse(between(Year, 1997, 2002), 0, Theodore))

# Exclude time series with > 2 NAs for sport harvest (in years with escapement data)
# Based on this criterion, I excluded Chulitna and Lewis from the sport harvest analysis.
# Also exclude Ninilchik because we don't know the breakdown of hatchery vs natural origin
# fish in the sport harvest
sport <- sportWideCorr %>%
  gather(key = "Population", value = "SportHarvest", 2:16) %>%
  filter(Population != "Chulitna", Population != "Lewis", Population != "Ninilchik")

# Clean up
rm(sportFull, sportKenaiLate, sportSimple, sportWide, sportWideCorr)

# Personal use---------------

# Munge and combine old (pre-1996) and new personal use data
puOld.long <- puOld %>%
  select(-PermitsIssued, -PermitsReturned, -HarvestTotal, -Source, -Page) %>%
  gather(key = Species, value = Harvest, 3:7) %>%
  mutate(Species = str_sub(Species, start = 8)) %>%
  filter(Species == "Chinook") %>%
  select(Year, Fishery, Harvest)

puNew.corr <- puNew %>%
  # Assign harvest and effort from the "unknown fishery" (folks who reported harvest but not what fishery
  # it came from) to the real fisheries, in proportion to the reported harvest and effort
  mutate(
    TotalOld = Kenai + KasilofSet + KasilofDip + FishCr + Unknown,
    Kenai1 = Kenai + round(Kenai/(Kenai+KasilofSet+KasilofDip+FishCr)*Unknown),
    KasilofSet1 = KasilofSet + round(KasilofSet/(Kenai+KasilofSet+KasilofDip+FishCr)*Unknown),
    KasilofDip1 = KasilofDip + round(KasilofDip/(Kenai+KasilofSet+KasilofDip+FishCr)*Unknown),
    FishCr1 = FishCr + round(FishCr/(Kenai+KasilofSet+KasilofDip+FishCr)*Unknown),
    TotalNew = Kenai1 + KasilofSet1 + KasilofDip1 + FishCr1,
    Diff = TotalNew - TotalOld
  )

# The math checks out (diff = 0, or 1 or -1 due to rounding errors).  Finish the corrections.
puNew.long <- puNew.corr %>%
  mutate(Kenai = Kenai1,
         KasilofSet = KasilofSet1,
         KasilofDip = KasilofDip1,
         FishCr = FishCr1) %>%
  select(Species, Year, Kenai, KasilofSet, KasilofDip, FishCr) %>%
  gather(key = Fishery, value = Harvest, 3:6) %>%
  filter(Species == "Chinook")

# Join the new harvest, new effort, and old harvest + effort dataframes together
pu <- puNew.long %>%
  bind_rows(puOld.long) %>%
  select(Year, Fishery, Harvest) %>%
  arrange(Fishery, Year)

# Generate a time series of total king salmon sport harvest in fresh waters of Cook Inlet
puHarvestByYear <- pu %>%
  group_by(Year) %>%
  summarize(PersonalUse = sum(Harvest, na.rm = T))

# Subsistence------------

# Compile subsistence harvest data from Tyonek and Kenai (incl. Kenaitze educational fishery)
# Generate a time series of total king salmon sport harvest in fresh waters of Cook Inlet
subHarvestTyonek <- sub %>%
  select(Fishery, Year, Subsistence = EstHarvest)

subHarvestKenai <- kenai %>%
  mutate(Fishery = Population, 
         Subsistence = rowSums(cbind(EducationalHarvest, SubsistenceHarvest), na.rm = T)) %>%
  select(Fishery, Year, Subsistence)

subHarvestByYear <- rbind(subHarvestTyonek, subHarvestKenai) %>%
  group_by(Year) %>%
  summarize(Subsistence = sum(Subsistence, na.rm = T))

# Clean up
rm(commRaw, puNew, puNew.corr, puNew.long, puOld, sportAreas, sportRaw)

# Compile all harvest data--------------
harvestByYearWide <- commHarvestByYear %>%
  left_join(sportHarvestByYear, by = "Year") %>%
  left_join(puHarvestByYear, by = "Year") %>%
  left_join(subHarvestByYear, by = "Year") %>%
  filter(Year > 1979) 

harvestByYear <- harvestByYearWide %>%
  gather(key = "Fishery", value = "Harvest", 2:5) %>%
  mutate(Fishery = factor(as.factor(Fishery), labels = c("Commercial", "Personal Use",
                                                         "Sport (Freshwater)", "Subsistence")))

# Plots--------------
# Plot the harvest trends
harvest.plot <- ggplot(data = harvestByYear, aes(x = Year, y = Harvest, color = Fishery)) +
  geom_line() +
  scale_x_continuous(name = "Year", breaks = seq(1980, 2020, by = 10), limits = c(1980, 2020)) +
  scale_y_continuous(name = "Chinook salmon harvest", labels = scales::comma) 
harvest.plot
ggsave("./figs/HarvestByFishery.png")

# Pairwise correlations btw harvest in each fishing sector
# harvest.pairs.plot <- ggpairs(data = harvestByYearWide, columns = 2:5) +
#   scale_x_continuous(labels = scales::comma) +
#   scale_y_continuous(labels = scales::comma)
# harvest.pairs.plot
