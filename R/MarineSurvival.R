# MarineSurvival.R
# Erik Schoen
# eschoen@alaska.edu
# 8-2018

# Calculate marine (smolt-to-adult) survival rates for hatchery-origin Chinook salmon stocked into
# Crooked Creek and the Ninilchik River

# Load packages, source scripts, and read in data-------------
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)
library(stringr)

source("./R/SpawnersRecruits.R")

smolt <- read_csv("./data/HatcheryReleases.csv")

# 
hatcheryOriginRecruits <- recruitsByBrood %>%
  filter(Population == "Crooked.Hatchery" | Population == "Ninilchik.Hatchery") %>%
  mutate(ReleaseYear == BroodYear + 1) %>%