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

theme_set(theme_bw(14))
setwd("~/Desktop/Cook Inlet Chinook/Analysis")

# Read in data
commRaw <- read_csv("./data/CommHarvest.csv")
sportRaw <- read_csv("./data/SportHarvest_SWHS.csv")  %>%
  # Calculate Chinook harvest as "KS" = king salmon + "KI" = immature king salmon (used from 1981-1992 only)
  mutate(Chinook = rowSums(cbind(KS, KI), na.rm = T)) %>%
  filter(Value == "Estimates") %>%
  rename(Region.SWHS = Region)
# sportAreas <- read_csv("./data/SportHarvest_Areas.csv") # delete
sportAreas <- read_csv("./data/sportHarvestAreas.csv") %>%
  select(-ChinookHarvestTotal)
marineSportRaw <- read_csv("./data/SportHarvest_Marine.csv")
mixedStock <- read_csv("./data/MixedStockAnalysis.csv")
puOld <- read_csv("./data/PUHarvest_pre1996.csv")
puNew <- read_csv("./data/PUHarvest_post1996.csv")
sub <- read_csv("./data/SubHarvest.csv")
kenai <- read_csv("./data/Kenai.csv")
kasilofLate <- read_csv("./data/KasilofLate.csv")
crooked <- read_csv("./data/CrookedCreek.csv")

# Munge and clean up data--------------------

# Freshwater Sport---------------

# Make a table of all Area.Fished values listed in the SWHS for Cook Inlet (freshwater only, with Chinook harvest > 0)
# sportHarvestAreasAll <- sportRaw %>%
#   filter(Type == "Freshwater") %>%
#   filter(Chinook > 0) %>%
#   # Remove regional totals
#   filter(!str_detect(Area.Fished, "Total")) %>%
#   filter(!str_detect(Area.Fished, "TOTAL")) %>%
#   select(Area.Fished, Region.SWHS = Region, Chinook) %>%
#   group_by(Area.Fished, Region.SWHS) %>%
#   summarize(ChinookHarvestTotal = sum(Chinook)) %>%
#   arrange(Region.SWHS, Area.Fished) %>%
#   distinct() %>%
#   write_csv("./data/SportHarvestAreasAll.csv")
# Study populations and MSA reporting groups added to this table manually to create the sportHarvestAreasAll.csv file

# Select the Chinook data and join with population table
sportFreshSWHS <- sportRaw %>%
  filter(Type == "Freshwater") %>%
  select(Year:Area.Fished, Chinook) %>%
  distinct() %>%
  left_join(sportAreas, by = c("Region.SWHS", "Area.Fished"))

# Generate a time series of total king salmon sport harvest in fresh waters of Cook Inlet
# 1st, using regional totals
fwSportHarvestByYear_regional <- sportFreshSWHS %>%
  filter(str_detect(Area.Fished, "Total") | str_detect(Area.Fished, "TOTAL")) %>%
  distinct() %>%
  group_by(Year) %>%
  summarize(SportFW.regional = sum(Chinook, na.rm = T))

# 2nd, using detailed data
fwSportHarvestByYear_detail <- sportFreshSWHS %>%
  filter(!str_detect(Area.Fished, "Total")) %>%
  filter(!str_detect(Area.Fished, "TOTAL")) %>%
  distinct() %>%
  group_by(Year) %>%
  summarize(SportFW = sum(Chinook, na.rm = T))

# Combine and compare
fwSportHarvestByYear <- fwSportHarvestByYear_detail %>%
  left_join(fwSportHarvestByYear_regional, by = "Year")

# The regional and detailed sums match in each year, and they match the regional totals I downloaded
# separately from the ADFG SWHS website for 1996-2015.

# Calculate the sums from the detailed data
fwSportHarvestByYear <- fwSportHarvestByYear %>%
  select(-SportFW.regional)

# Show the Deshka River only to verify against sport harvest numbers in Erickson et al. 2017
sportDeshka <- sportFreshSWHS %>%
  filter(Value == "Estimates") %>%
  distinct() %>%
  filter(Population == "Deshka") %>%
  group_by(Year) %>%
  summarize(Chinook = sum(Chinook))
# These sport harvest numbers match those reported in Erickson et al. 2017, except that Erickson et al. has a typo
# for 1990.  I verified that the 1990 sport harvest calculated here is correct based on the original source (Mills 1991,
# p. 85).

# Remove Totals rows from the SWHS data (to avoid accidentally double counting)
sportFreshSWHS <- sportFreshSWHS %>%
  filter(!str_detect(Area.Fished, "Total")) %>%
  filter(!str_detect(Area.Fished, "TOTAL")) %>%
  # remove rows with 0 Chinook harvest
  filter(Chinook > 0)

# Compile the sport harvest data for the Kenai and Kasilof broken down by early/late runs (from Begich et al. 2017)
sportKenai <- kenai %>%
  mutate(Chinook = rowSums(cbind(SportHarvestBelowSonar, SportHarvestAboveSonar), 
                                na.rm = T)) %>%
  select(Population, Year, Chinook) %>%
  filter(Year < 2016) %>% # remove 0 for 2016 (we actually have no data)
  mutate(ReportingGroup_ESSN = ifelse(Population == "Kenai.Early", "Kenai River tributaries", "Kenai River mainstem"),
         ReportingGroup_NSN = "Kenai Peninsula",
         ReportingGroup_MS = "Kenai")

sportKasilofLate <- kasilofLate %>%
  rename(Chinook = Harvest_InriverSport) %>%
  select(Population, Year, Chinook) %>%
  mutate(ReportingGroup_ESSN = "Kasilof River mainstem",
         ReportingGroup_NSN = "Kenai Peninsula",
         ReportingGroup_MS = "Kenai")

sportCrooked <- crooked %>%
  rename(Chinook = Harvest_total) %>%
  select(Population, Year, Chinook) %>%
  filter(Year < 2016) %>% # remove 0 for 2016 (we actually have no data)
  mutate(ReportingGroup_ESSN = "Cook Inlet other_ESSN",
         ReportingGroup_NSN = "Kenai Peninsula",
         ReportingGroup_MS = "Kenai")

# Can we estimate the sport harvest of natural-origin Chinook in the Crooked Creek and Ninilchik populations?
# 1: Crooked
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

# 2: Ninilchik
# Are there real data breaking down sport harvest by
# hatchery vs. natural origin?  The spreadsheet from Mike Booz just divides them 50/50
# for some years and 75/25 for other years.  If they don't have data on the breakdown
# can I use the Crooked Creek model based on how many days the fishery was open to 
# retention of natural vs hatchery fish?
# # Expansion factor for Ninilchik River (this proportion of spawning occurs
# # above the weir, so divide escapement by this factor to account for spawning below weir).
# # Source: Mike Booz pers. comm.
# ninExp <- 0.65 

# Build the best available dataset of freshwater sport harvest.  This includes all FW sport harvest 
# in Cook Inlet basin from SWHS, except that more detailed data from Begich et al. 2017
# are used to break down Kenai and Kasilof harvest by early vs late runs (for most
# years)

sportFresh <- sportFreshSWHS %>%
  # For the years in which we have sport harvest broken down by early / late runs,
  # remove the ambiguous / redundant Kenai and Kasilof data from the SWHS.
  filter(!(Population == "Kenai River undetermined" & Year > 1985)) %>%
  filter(!(Population == "Kasilof River undetermined" & Year > 1995)) %>%
  # Add the population-specific Kenai and Kasilof data from Begich et al. 2017
  bind_rows(sportKenai, sportKasilofLate, sportCrooked)

# Check that we haven't lost or gained any total sport harvest
fwSportHarvestByYear2 <- sportFresh %>%
  group_by(Year) %>%
  summarize(SportFW2 = sum(Chinook, na.rm = T)) %>%
  left_join(fwSportHarvestByYear, by = "Year") %>%
  mutate(diff = SportFW2 - SportFW)

# TODO: Generate dataframes of sport harvest in each reporting group, by year
sportByRG_ESSN <- sportFresh %>%
  group_by(Year, ReportingGroup_ESSN) %>%
  summarize(fwSportHarvest = sum(Chinook))
# The Kenai mainstem data isn't going in here for some reason.  

sportByRG_NSN

sportByRG_MS

# Filter out just our study streams and summarize multiple sites / population
sportSimple <- sportFresh %>%
  filter(!is.na(Population)) %>%
  distinct() %>%
  group_by(Population, Year) %>%
  summarize(SportHarvest = sum(Chinook, na.rm = T))
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
# Also exclude Crooked, Ninilchik, and Willow because we don't know the breakdown of hatchery vs natural origin
# fish in the sport harvest
sport <- sportWideCorr %>%
  gather(key = "Population", value = "SportHarvest", Alexander:Willow) %>%
  filter(Population != "Chulitna" & Population != "Lewis" & 
           Population != "Crooked/Kasilof" & Population != "Crooked" & Population != "Ninilchik" & 
           Population != "Willow")

# Clean up
rm(sportFresh, sportKenai, sportDeshkaFull, sportDeshka, sportSimple, sportWide, sportWideCorr)

# Freshwater subsistence, personal use, and educational---------------

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

# Generate a time series of total king salmon personal use harvest in fresh waters of Cook Inlet
puHarvestByYear <- pu %>%
  group_by(Year) %>%
  summarize(PersonalUse = sum(Harvest, na.rm = T))

fwSubPUEdHarvestByYear <- kenai %>%
  left_join(puHarvestByYear, by = "Year") %>%
  mutate(SubPUEd = rowSums(cbind(SubsistenceHarvest, EducationalHarvest, PersonalUse), na.rm = T)) %>%
  group_by(Year) %>%
  summarize(SubPUEdFW = sum(SubPUEd, na.rm = T)) 

# Commercial------------------
comm <- commRaw %>%
  select(-Reference, -Page) %>%
  gather(key = "Fishery", value = "Harvest", Drift:NSN) %>%
  mutate(Area_Season = "All") %>%
  select(Fishery, Area_Season, Year, Harvest)

# Generate a time series of total king salmon commercial harvest in Cook Inlet
commHarvestByYear <- comm %>%
  group_by(Year) %>%
  summarize(Commercial = sum(Harvest))

# Marine Sport---------------
marineSport <- marineSportRaw %>%
  select(Year:CentralCI_LateSummer) %>%
  gather(key = "Area_Season", value = "Harvest", CentralCI_EarlySummer:CentralCI_LateSummer) %>%
  mutate(Fishery = "MarineSport" )%>%
  select(Fishery, Area_Season, Year, Harvest)

marineSportHarvestByYear <- marineSport %>%
  group_by(Year) %>%
  summarize(SportMarine = sum(Harvest))
  
# Marine subsistence------------

# Compile subsistence harvest data
# Generate a time series of total king salmon subsistence harvest in marine waters of Cook Inlet
marineSubHarvestByYear <- sub %>%
  # Exclude terminal subsistence fisheries: Upper Yentna and federal Kenai/Kasilof.  Also "Northern/Central
  # Districts personal-use / subsistence fishery" since it is unclear how these data overlap with
  # harvest reported in other sources
  filter(Fishery != "FederalKenaiKasilof" & Fishery != "NorthernCentral") %>%
  group_by(Year) %>%
  summarize(SubsistenceMarine = sum(Chinook))

tyonek <- sub %>%
  filter(Fishery == "Tyonek") %>%
  select(Fishery, Year, Harvest = Chinook) %>%
  mutate(Area_Season = "All") %>%
  select(Fishery, Area_Season, Year, Harvest)

# Mixed stock analysis----------------------------
# First, assign harvest from fisheries (or areas and seasons for marine sport fishery) to reporting groups.
# Compile harvest from all mixed stock fisheries (commercial, sport, and subsistence) with MSA data available
mixedStockHarvest <- comm %>%
  filter(Fishery == "ESSN" | Fishery == "NSN") %>%
  bind_rows(tyonek, marineSport) %>%
  filter(Year > 1979 & Year < 2016)

# Calculate the mean stock composition across all years with available data
mixedStockMean <- mixedStock %>%
  group_by(Fishery, Area_Season, ReportingGroup) %>%
  summarize(meanStockComp = mean(StockCompMean))

# Join mixed stock analysis data to harvest data
mixedStockHarvest <- mixedStockHarvest %>%
  rename(Harvest_Area_Season = Harvest) %>%
  left_join(mixedStockMean, by = c("Fishery", "Area_Season")) %>%
  left_join(mixedStock, by = c("Fishery", "Area_Season", "Year", "ReportingGroup")) %>%
  select(Fishery:StockCompMean) %>%
  rename(yearlyStockComp = StockCompMean) %>%
  mutate(estStockComp = ifelse(!is.na(yearlyStockComp), yearlyStockComp, meanStockComp),
         Harvest_ReportingGroup = Harvest_Area_Season * estStockComp)

# Filter out the reporting groups that don't contain any of our study populations

# TODO: Second, assign harvest from reporting groups to individual populations
# 1) calculate the proportion of the aggregate in-river runs (escapement + sport harvest) to each reporting group
# made up by each population

# 2) assign the harvest from reporting groups to individual populations

# 3) add up the harvest and make sure it matches the total harvest by fishery that we started with


# Clean up
rm(commRaw, puNew, puNew.corr, puNew.long, puOld, sportAreas, sportRaw)

# Compile all harvest data and calculate values for manuscript text--------------
harvestByYearWide <- commHarvestByYear %>%
  left_join(fwSportHarvestByYear, by = "Year") %>%
  left_join(marineSportHarvestByYear, by = "Year") %>%
  left_join(fwSubPUEdHarvestByYear, by = "Year") %>%
  left_join(marineSubHarvestByYear, by = "Year") %>%
  filter(Year > 1979)  %>%
  mutate(PercentFW = rowSums(cbind(SportFW, SubPUEdFW), na.rm = T) / 
           rowSums(cbind(Commercial, SportFW, SportMarine, SubPUEdFW, SubsistenceMarine), na.rm = T))

# What % of the Chinook harvest in Cook Inlet takes place in freshwater?
meanPercentFW <- mean(harvestByYearWide$PercentFW)
minPercentFW <- min(harvestByYearWide$PercentFW)
maxPercentFW <- max(harvestByYearWide$PercentFW)

# What % of Chinook harvested in the marine sport fishery during 2014-15 were from Cook Inlet stocks?
marineSportCIStocks <- marineSport %>%
  select(Year:LowerCI_Winter) %>%
  gather(key = "Area_Season", value = "Harvest", 2:5) %>%
  filter(Year == 2014 | Year == 2015) %>%
  left_join(mixedStock, by = c("Year", "Area_Season")) %>%
  filter(ReportingGroup == "Outside Cook Inlet" | is.na(ReportingGroup)) %>%
  # Use MSA data from Barclay et al. 2016 for 3 of the 4 space/time strata.  Assume the 4th stratum
  # with no data (Central Cook Inlet_Late Summer) has the same stock composition as Central Cook Inlet_
  # Early Summer (mean estimate), with a minimum estimate of 0% and a maximum estimate of 100%.
  mutate(StockCompMean = ifelse(Area_Season == "CentralCI_LateSummer", 
                                ifelse(Year == 2014, 75.3, 80.4),
                                 StockCompMean),
         StockComp5Percentile = ifelse(Area_Season == "CentralCI_LateSummer", 
                                       0, StockComp5Percentile),
         StockComp95Percentile = ifelse(Area_Season == "CentralCI_LateSummer", 
                                        100, StockComp95Percentile),
         HarvestCIStocksMean = Harvest * (100-StockCompMean)/100,
         HarvestCIStocksMin = Harvest * (100-StockComp95Percentile)/100,
         HarvestCIStocksMax = Harvest * (100-StockComp5Percentile)/100) %>%
  group_by(Year) %>%
  summarize(Harvest = sum(Harvest),
            HarvestCIStocksMean = sum(HarvestCIStocksMean),
            HarvestCIStocksMin = sum(HarvestCIStocksMin),
            HarvestCIStocksMax = sum(HarvestCIStocksMax)) %>%
  # Estimate the fraction of the marine sport harvest from Cook Inlet stocks (min, mean, max)
  mutate(PercCIMean = HarvestCIStocksMean/Harvest,
         PercCIMin = HarvestCIStocksMin/Harvest,
         PercCIMax = HarvestCIStocksMax/Harvest)
         
harvestByYear <- harvestByYearWide %>%
  gather(key = "Fishery", value = "Harvest", 2:6) %>%
  mutate(Fishery = factor(as.factor(Fishery), labels = c("Commercial",
                                                         "Sport (Freshwater)", 
                                                         "Sport (Marine)", 
                                                         "Sub/PU/Ed (Freshwater)",
                                                         "Subsistence (Marine)")),
         FWorMarine = ifelse(Fishery == "Sport (Freshwater)" , "Freshwater", 
                             ifelse(Fishery == "Sub/PU/Ed (Freshwater)", "Freshwater", "Marine")))

# Plots--------------
# Plot the harvest trends
harvest.plot <- ggplot(data = harvestByYear, aes(x = Year, y = Harvest, color = Fishery)) +
  geom_line() +
  scale_x_continuous(name = "Year", breaks = seq(1980, 2015, by = 10), limits = c(1980, 2015)) +
  scale_y_continuous(name = "Harvest", labels = scales::comma) 
harvest.plot
ggsave("./figs/Harvest by fishery.png")

# Pairwise correlations btw harvest in each fishing sector
harvest.pairs.plot <- ggpairs(data = harvestByYearWide, columns = 2:6)
harvest.pairs.plot
