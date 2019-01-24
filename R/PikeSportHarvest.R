# PikeSportHarvest.R
# Erik Schoen
# eschoen@alaska.edu
# 1-2019

# Summarize sport harvest data for northern pike in Cook Inlet drainage

# Load packages and read in data-------------
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)
library(stringr)

theme_set(theme_bw(14))
setwd("~/Desktop/Cook Inlet Chinook/Analysis")

# Read in data
sportRaw <- read_csv("./data/SportHarvest_SWHS.csv")  %>%
  # Calculate Chinook harvest as "KS" = king salmon + "KI" = immature king salmon (used from 1981-1992 only)
  mutate(Chinook = rowSums(cbind(KS, KI), na.rm = T)) %>%
  filter(Value == "Estimates") %>%
  rename(Region.SWHS = Region)
sportAreas <- read_csv("./data/sportHarvestAreas.csv") %>%
  select(-ChinookHarvestTotal)
sites <- read_csv("./data/Sites.csv")

# Munge and clean up data--------------------

# Select the pike data and join with table of study populations
pikeSportAllAreas <- sportRaw %>%
  select(Year, Region.SWHS, Area.Fished, NP, Responses.Used, Anglers, Trips, Days.Fished) %>%
  # Remove marginal sums (regional totals)
  filter(!str_detect(Area.Fished, "Total")) %>%
  filter(!str_detect(Area.Fished, "TOTAL")) %>%
  # Remove rows with 0 Chinook harvest
  filter(NP > 0) %>%
  distinct() %>%
  left_join(sportAreas, by = c("Region.SWHS", "Area.Fished"))

# Filter down to only our study populations and summarize by year and population
pikeSport <- pikeSportAllAreas %>%
  filter(!is.na(Population)) %>%
  group_by(Year, Population) %>%
  summarize(PikeHarvest = sum(NP)) %>%
            # , Responses.Used = sum(Responses.Used), Anglers = sum(Anglers),
            # Days.Fished = sum(Days.Fished))
  mutate(Population = ifelse(Population == "Kenai River undetermined", "Kenai",
                             Population))
              
# Plot the harvest trends
pikeHarvest.plot <- ggplot(data = pikeSport, aes(x = Year, y = PikeHarvest, color = Population)) +
  geom_line() +
  geom_point(shape = 1) +
  scale_x_continuous(name = "Year", breaks = seq(1980, 2015, by = 5), limits = c(1980, 2015)) +
  scale_y_continuous(name = "Sport Harvest of Northern Pike") 
pikeHarvest.plot
ggsave("./figs/PikeSportHarvest.png", width = 8, height = 6)
