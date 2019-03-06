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

theme_set(theme_bw(12))
setwd("~/Desktop/Cook Inlet Chinook/Analysis")

# Read in data
commRaw <- read_csv("./data/CommHarvest.csv")
sportRaw <- read_csv("./data/SportHarvest_SWHS.csv")  %>%
  # Calculate Chinook harvest as "KS" = king salmon + "KI" = immature king salmon (used from 1981-1992 only)
  mutate(Chinook = rowSums(cbind(KS, KI), na.rm = T)) %>%
  filter(Value == "Estimates") %>%
  rename(Region.SWHS = Region)
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
escape <- read_csv("./data/Escapement.csv")
sites <- read_csv("./data/Sites.csv")

# Munge and clean up data--------------------

# Freshwater Sport---------------

# Make a table of all Area.Fished values listed in the SWHS for Cook Inlet (freshwater only, with Chinook harvest > 0)
# sportHarvestAreasAll <- sportRaw %>%
#   filter(Type == "Freshwater") %>%
#   filter(Chinook > 0) %>%
#   # Remove regional totals
#   filter(!str_detect(Area.Fished, "Total")) %>%
#   filter(!str_detect(Area.Fished, "TOTAL")) %>%
#   select(Area.Fished, Region.SWHS, Chinook) %>%
#   group_by(Area.Fished, Region.SWHS) %>%
#   summarize(ChinookHarvestTotal = sum(Chinook)) %>%
#   arrange(Region.SWHS, Area.Fished) %>%
#   distinct() %>%
#   write_csv("./data/SportHarvestAreasAll.csv")
# Study populations and MSA reporting groups added to this table manually to create the sportHarvestAreasAll.csv file

# Select the Chinook data and join with population table
sportFresh <- sportRaw %>%
  filter(Type == "Freshwater") %>%
  select(Year, Region.SWHS, Area.Fished, Chinook) %>%
  # Remove marginal sums (regional totals)
  filter(!str_detect(Area.Fished, "Total")) %>%
  filter(!str_detect(Area.Fished, "TOTAL")) %>%
  # Remove rows with 0 Chinook harvest
  filter(Chinook > 0) %>%
  distinct() %>%
  left_join(sportAreas, by = c("Region.SWHS", "Area.Fished"))

# Generate a time series of total king salmon sport harvest in fresh waters of Cook Inlet
fwSportHarvestByYear <- sportFresh %>%
  group_by(Year) %>%
  summarize(SportFW = sum(Chinook, na.rm = T))

# These annual sums match the regional totals I downloaded
# separately from the ADFG SWHS website for 1996-2015.

# Show the Deshka River only to verify against sport harvest numbers in Erickson et al. 2017
sportDeshka <- sportFresh %>%
  filter(Population == "Deshka") %>%
  group_by(Year) %>%
  summarize(Chinook = sum(Chinook))
# These sport harvest numbers match those reported in Erickson et al. 2017, except that Erickson et al. has a typo
# for 1990.  I verified that the 1990 sport harvest calculated here is correct based on the original source (Mills 1991,
# p. 85).

# Deal with hatchery-enhanced populations (commented out) ----------
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

# Build the best available dataset of freshwater sport harvest (commented out)----------
# This includes all FW sport harvest in Cook Inlet basin from SWHS,
# except that more detailed data from Begich et al. 2017
# are used to break down Kenai and Kasilof harvest by early vs late runs (for most years)
# Compile the sport harvest data for the Kenai and Kasilof broken down by early/late runs (from Begich et al. 2017)
# sportKenai <- kenai %>%
#   mutate(Chinook = rowSums(cbind(SportHarvestBelowSonar, SportHarvestAboveSonar), 
#                            na.rm = T)) %>%
#   select(Population, Year, Chinook) %>%
#   filter(Year < 2016) %>% # remove 0 for 2016 (we actually have no data)
#   mutate(Region.SWHS = "Kenai Peninsula",
#          Area.Fished = "Kenai River",
#          ReportingGroup_ESSN = ifelse(Population == "Kenai.Early", "Kenai River tributaries", "Kenai River mainstem"),
#          ReportingGroup_NSN = "Kenai Peninsula",
#          ReportingGroup_MS = "Kenai")
# 
# sportKasilofLate <- kasilofLate %>%
#   rename(Chinook = Harvest_InriverSport) %>%
#   select(Population, Year, Chinook) %>%
#   mutate(Region.SWHS = "Kenai Peninsula",
#          Area.Fished = "Kasilof River",
#          ReportingGroup_ESSN = "Kasilof River mainstem",
#          ReportingGroup_NSN = "Kenai Peninsula",
#          ReportingGroup_MS = "Kenai")
# 
# sportCrooked <- crooked %>%
#   rename(Chinook = Harvest_total) %>%
#   select(Population, Year, Chinook) %>%
#   filter(Year < 2016) %>% # remove 0 for 2016 (we actually have no data)
#   mutate(Region.SWHS = "Kenai Peninsula",
#          Area.Fished = "Kasilof River",
#          ReportingGroup_ESSN = "Cook Inlet other_ESSN",
#          ReportingGroup_NSN = "Kenai Peninsula",
#          ReportingGroup_MS = "Kenai")
# 
# # Combine Kenai and Kasilof early / late run data
# sportKenaiKasilof <- bind_rows(sportKenai, sportKasilofLate, sportCrooked)
# 
# # # Calculate proportional contributions of early and late runs to the Kenai and Kasilof River 
# # # sport harvest 
# # sportKenaiKasilof_meanEarlyLate <- sportKenaiKasilof %>%
# #   select(Area.Fished, Year, Population, Chinook) %>%
# #   spread(key = Population, value = Chinook) %>%
# #   mutate(propKenaiEarly = Kenai.Early / (Kenai.Early + Kenai.Late),
# #          propCrooked = Crooked / (Crooked + Kasilof.Late))
# # 
# # # The Kenai early run made up ~45% of the total Kenai sport harvest from 1986-1990, but then
# # # declined over time.  Use the 5-yr average from 1986-1990 to estimate the proportion during
# # # previous years
# 
# # Replace the non
# # TODO: This code chunk isn't working.  I think the filters are inadvertently dropping a bunch 
# # of NAs
# sportFresh <- sportFresh %>%
#   select(Population, Year, Chinook, Region.SWHS, Area.Fished, ReportingGroup_ESSN, 
#          ReportingGroup_NSN, ReportingGroup_MS) %>%
#   # For the years in which we have Kenai and Kasilof sport harvest broken down by early / late 
#   # runs from Begich, remove the ambiguous / redundant Kenai/Kasilof harvest data from the SWHS.
#   filter(!(ReportingGroup_ESSN == "Kenai River undetermined" & Year > 1985)) %>%
#   filter(!(ReportingGroup_ESSN == "Kenai River tributaries" & Year > 1985)) %>%
#   filter(!(ReportingGroup_ESSN == "Kenai River mainstem" & Year > 1985)) %>%
#   filter(!(ReportingGroup_ESSN == "Kasilof River undetermined" & Year > 1995)) %>%
#   filter(!(ReportingGroup_ESSN == "Kasilof River mainstem" & Year > 1995)) %>%
#   # Add the population-specific Kenai and Kasilof data from Begich et al. 2017
#   bind_rows(sportKenaiKasilof) %>%
#   # Remove 2016 (because Begich et al. data only go through 2015)
#   filter(Year < 2016)
# 
# levels(factor(sportFresh$ReportingGroup_ESSN))
# # Calculate Kenai River sport harvest by year and run
# sportKenaiSummary <- sportFresh %>%
#   filter(Population == "Kenai.Late") %>%
#   group_by(Year) %>%
#   summarize(SportHarvest = sum(Chinook))
# 
# # Check that we haven't lost or gained any total sport harvest
# totalChk_SWHS <- sum(sportFresh$Chinook)
# totalChk_sportFresh <- sum(sportFresh$Chinook)
# totalChk_SWHS - totalChk_sportFresh
# 
# totalChk_sportFresh <- sum(sportFresh$Chinook)
# totalChk_fwSportHarvestByYear <- sum(fwSportHarvestByYear$SportFW)
# # why are these different?
# 
# totalChk_check <- sportFresh %>%
#   group_by(Year) %>%
#   summarize(harvest_sportFresh = sum(Chinook, na.rm = T)) %>%
#   left_join(fwSportHarvestByYear, by = "Year") %>%
#   mutate(diff = harvest_sportFresh - SportFW)
# # TODO: These columns don't match: the new sportFresh dataframe is off by varying amounts
# # starting in 1986
# 
# # Make the same comparison, but with Kenai/Kasilof harvest only
# totalChkKK_check <- sportFresh %>%
#   filter(str_detect(ReportingGroup_ESSN, "Kenai") | str_detect(ReportingGroup_ESSN, "Kasilof"))
#   group_by(Year) %>%
#   summarize(harvest_sportFresh = sum(Chinook, na.rm = T)) %>%
#   left_join(fwSportHarvestByYear, by = "Year") %>%
#   mutate(diff = harvest_sportFresh - SportFW)

# Generate dataframes of sport harvest in each NSN / Tyonek reporting group, by year, and
# in each study population (within reporting groups) by year.
# ESSN is not needed because ESSN reporting groups map directly to our study populations 
# or to non-study populations (and "Cook Inlet other" RG is only around 1% of the harvest).

sportByRG_NSN <- sportFresh %>%
  group_by(Year, ReportingGroup_NSN) %>%
  summarize(SportHarvest_RG = sum(Chinook))

sportByPopByRG_NSN <- sportFresh %>%
  mutate(Population = ifelse(is.na(Population), "Other", Population)) %>%
  group_by(Year, ReportingGroup_NSN, Population) %>%
  summarize(SportHarvest_Pop = sum(Chinook)) %>%
  left_join(sportByRG_NSN, by = c("Year", "ReportingGroup_NSN"))

# Check sums
sum(sportByRG_NSN$SportHarvest_RG) - sum(sportByPopByRG_NSN$SportHarvest_Pop)

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

sportWide <- spread(sportSimple, key = Population, value = SportHarvest)

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
rm(sportFresh, sportDeshka, sportRaw, sportSimple, sportWide, sportWideCorr)

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
# Compile marine sport harvest data
# Exclude Lower Cook Inlet area because 98-100% of the Chinook harvested there are from
# populations outside CI, based on mixed-stock analysis of the 2014-15 harvest 
# (Barclay et al. 2016)
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
# Compile harvest from all mixed stock fisheries (commercial, sport, and subsistence) that
# have MSA data available
mixedStockHarvest <- comm %>%
  filter(Fishery == "ESSN" | Fishery == "NSN") %>%
  bind_rows(tyonek, marineSport) %>%
  filter(Year > 1979 & Year < 2016) %>%
  filter(Area_Season != "CentralCI_LateSummer")

# Convert mixed stock analysis results from percent to proportions (0-1) when necessary
mixedStock <- mixedStock %>%
  mutate(StockCompMean = ifelse(Fishery == "ESSN", StockCompMean, StockCompMean / 100),
         StockComp95Percentile = ifelse(Fishery == "ESSN", StockComp95Percentile, 
                                        StockComp95Percentile / 100),
         StockComp5Percentile = ifelse(Fishery == "ESSN", StockComp5Percentile, 
                                       StockComp5Percentile / 100))

# Calculate the mean stock composition across all years with available data
mixedStockMean <- mixedStock %>%
  group_by(Fishery, Area_Season, ReportingGroup) %>%
  # group_by(Fishery, Area_Season, ReportingGroup_ESSN, ReportingGroup_NSN, ReportingGroup_MS) %>%
  summarize(meanStockComp = mean(StockCompMean))

# Join mixed stock analysis data to harvest data
mixedStockHarvestByRG <- mixedStockHarvest %>%
  rename(Harvest_Area_Season = Harvest) %>%
  left_join(mixedStockMean, by = c("Fishery", "Area_Season")) %>%
  left_join(mixedStock, by = c("Fishery", "Area_Season", "Year", "ReportingGroup")) %>%
  select(Fishery:StockCompMean) %>%
  rename(yearlyStockComp = StockCompMean) %>%
  mutate(estStockComp = ifelse(!is.na(yearlyStockComp), yearlyStockComp, meanStockComp),
         Harvest_RG = Harvest_Area_Season * estStockComp)

# check sums
sum(mixedStockHarvestByRG$Harvest_RG) - sum(mixedStockHarvest$Harvest)

checkMSH <- mixedStockHarvestByRG %>%
  group_by(Fishery, Area_Season, Year) %>%
  summarize(totalHarvestAllRGs = round(sum(Harvest_RG))) %>%
  left_join(mixedStockHarvest, by = c("Fishery", "Area_Season", "Year")) %>%
  mutate(diff = totalHarvestAllRGs - Harvest)
# These are pretty close, just off by rounding errors of < 10 fish

# Second, assign harvest from reporting groups to individual populations--------

# For ESSN, simply assign all "Kenai River mainstem" harvest to Kenai.Late population--------
mixedStockHarvestByPop <- mixedStockHarvestByRG %>%
  filter(ReportingGroup == "Kenai River mainstem") %>%
  mutate(Population = "Kenai.Late") %>%
  select(-Area_Season) %>%
  rename(TotalHarvest = Harvest_Area_Season) %>%
  mutate(MixedStockHarvest = round(Harvest_RG))

# # TODO: For NSN and Tyonek... (commented out)-----------
# Break down commercial harvest by study population, assuming that each study population
# makes up the same proportion of its respective reporting group in the 
# commercial harvest as it does in the escapement + sport harvest combined.
# # Generate dataframes of escapement in each NSN / Tyonek reporting group, by year, and
# # in each study population (within reporting groups) by year.
# # ESSN is not needed because 3 main ESSN reporting groups map directly to 1 study population 
# # and 2 non-study populations (and "Cook Inlet other" RG is only around 1% of the harvest).
# 
# escByRG_NSN <- escape %>%
#   rename(Year = ReturnYear) %>%
#   filter(Year > 1979 & Year < 2016) %>%
#   left_join(sites, by = c("Population" = "Group")) %>%
#   filter(GroupType == "Population") %>%
#   group_by(Year, ReportingGroup_NSN) %>%
#   summarize(Escapement_RG = sum(Escapement, na.rm = T),
#             Pops_esc_RG = n())
# 
# escByPopByRG_NSN <- escape %>%
#   rename(Year = ReturnYear) %>%
#   filter(Year > 1979 & Year < 2016) %>%
#   left_join(sites, by = c("Population" = "Group")) %>%
#   filter(GroupType == "Population") %>%
#   group_by(Year, ReportingGroup_NSN, Population) %>%
#   summarize(Escapement_Pop = sum(Escapement, na.rm = T))
# 
# # Check sums
# sum(escByRG_NSN$Escapement_RG) - sum(escByPopByRG_NSN$Escapement_Pop)
# 
# inriverPopByRG <- full_join(escByPopByRG_NSN, sportByPopByRG_NSN, 
#                             by = c("Year", "ReportingGroup_NSN", "Population"))
# 
# # Join commercial harvest with escapement and sport harvest by proportions 
# 
# mixedStockHarvestByPop <- mixedStockHarvestByRG %>%
#   # Remove marine sport harvest (for now)
#   filter(Fishery != "MarineSport") %>%
#   # Remove reporting groups that do not include any study populations
#   filter(ReportingGroup != "Kenai River tributaries" & 
#            ReportingGroup != "Kasilof River mainstem") %>%
#   # Remove reporting groups that made up < 2% of the harvest (for now)
#   filter(meanStockComp > 0.02) %>%
#   # Join the escapement by population (within reporting groups) dataframe
#   left_join(escByPopByRG_NSN, by = c("ReportingGroup" = "ReportingGroup_NSN"))

# 1) calculate the proportion of the aggregate in-river runs (escapement + sport harvest) to each reporting group
# made up by each population

# 2) assign the harvest from reporting groups to individual populations

# 3) add up the harvest and make sure it matches the total harvest by fishery that we started with


# Clean up
rm(commRaw, puNew, puNew.corr, puNew.long, puOld, sportAreas, sportRaw)

# Compile all harvest data and calculate values for manuscript text--------------
#TODO:
# What % of the Chinook harvest in Cook Inlet is accounted for in the stock-recruit analysis?
# Exclude marine sport harvest from this estimate because most of those fish are not from
# Cook Inlet stocks

harvestAccountedFor <- comm %>%
  mutate(AccountedFor = ifelse(Fishery == "ESSN", T, F)) %>%
  group_by(Year, AccountedFor) %>%
  summarize(Harvest = sum(Harvest, na.rm = T)) %>%
  filter(Year < 2016 & Year > 1979) %>%
  spread(key = "AccountedFor", value = "Harvest") %>%
  rename(CommAccountedFor = `TRUE`, CommNotAccountedFor = `FALSE`) %>%
  left_join(fwSportHarvestByYear, by = "Year") %>%
  left_join(marineSportHarvestByYear, by = "Year") %>%
  left_join(fwSubPUEdHarvestByYear, by = "Year") %>%
  left_join(marineSubHarvestByYear, by = "Year") %>%
  mutate(HarvestAccountedFor = rowSums(cbind(SportFW, SubPUEdFW, CommAccountedFor), na.rm = T),
         HarvestNotAccountedFor = rowSums(cbind(CommNotAccountedFor, 
                                                SubsistenceMarine), na.rm = T),
         PercentAccountedFor = HarvestAccountedFor / (HarvestAccountedFor +
                                                        HarvestNotAccountedFor))

meanPercentAccountedFor <- mean(harvestAccountedFor$PercentAccountedFor)
minPercentAccountedFor <- min(harvestAccountedFor$PercentAccountedFor)
maxPercentAccountedFor <- max(harvestAccountedFor$PercentAccountedFor)

# How well does the harvest that was not accounted for correlate with the harvest that was
# accounted for?
# reshape the data long for plotting
harvestAccountedFor.long <- harvestAccountedFor %>%
  select(Year, HarvestAccountedFor, HarvestNotAccountedFor) %>%
  gather(key = "Fishery", value = "Harvest", -Year)
harvestAccountedFor.plot <- ggplot(data = harvestAccountedFor, aes(x = HarvestAccountedFor, 
                                                                   y = HarvestNotAccountedFor,
                                                                   label = Year)) +
  geom_text()
harvestAccountedFor.plot

harvestAccountedFor.lm <- lm(HarvestNotAccountedFor ~ HarvestAccountedFor, 
                             data = harvestAccountedFor)
summary(harvestAccountedFor.lm)

harvestByYearWide <- commHarvestByYear %>%
  left_join(fwSportHarvestByYear, by = "Year") %>%
  left_join(marineSportHarvestByYear, by = "Year") %>%
  left_join(fwSubPUEdHarvestByYear, by = "Year") %>%
  left_join(marineSubHarvestByYear, by = "Year") %>%
  filter(Year > 1979)  %>%
  mutate(PercentFW = rowSums(cbind(SportFW, SubPUEdFW), na.rm = T) / 
           rowSums(cbind(Commercial, SportFW, SubPUEdFW, SportMarine, 
                         SubsistenceMarine), na.rm = T),
         TotalHarvest = rowSums(cbind(SportFW, SubPUEdFW, Commercial, SportMarine, 
                                      SubsistenceMarine), na.rm = T))

# What % of the Chinook harvest in Cook Inlet takes place in freshwater?
meanPercentFW <- mean(harvestByYearWide$PercentFW)
minPercentFW <- min(harvestByYearWide$PercentFW)
maxPercentFW <- max(harvestByYearWide$PercentFW)

# What was the mean total harvest 

# TODO: Double-check how I accounted for the UCI-Late Summer stratum, which has no MSA data.
# Do I need to revise the methods text to clarify this?
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
  mutate(Fishery = factor(as.factor(Fishery), labels = c("Commercial (SW)",
                                                         "Sport (FW)", 
                                                         "Sport (SW)", 
                                                         "Sub/PU/Ed (FW)",
                                                         "Subsistence (SW)")),
         FWorMarine = ifelse(Fishery == "Sport (Freshwater)" , "Freshwater", 
                             ifelse(Fishery == "Sub/PU/Ed (Freshwater)", "Freshwater", "Marine")))

# Plots--------------
# Plot the harvest trends
harvest.plot <- ggplot(data = harvestByYear, aes(x = Year, y = Harvest/1000, color = Fishery)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Return Year", breaks = seq(1980, 2015, by = 10), limits = c(1980, 2015)) +
  scale_y_continuous(name = "Harvest (1,000s)") +
  theme(legend.justification=c(1,1), legend.position=c(.98,.98), legend.background = element_rect()) 
harvest.plot
ggsave("./figs/Harvest by fishery.png", width = 7, height = 5)

# Pairwise correlations btw harvest in each fishing sector
harvest.pairs.plot <- ggpairs(data = harvestByYearWide, columns = 2:6)
harvest.pairs.plot
