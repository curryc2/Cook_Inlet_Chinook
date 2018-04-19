# AgeComp_estimate.R
# Erik Schoen
# eschoen@alaska.edu
# 2-2018

# Cook Inlet Chinook project
# Estimate age comps for stocks / brood years with missing data

# Load packages and read in data-------------
library(dplyr)
library(tidyr)
library(readr)

setwd("~/Desktop/Cook Inlet Chinook/Analysis/data")
# Read in data from AgeComp tab
ageSimple <- read_csv("AgeSimple.csv")

# Method 1: Simple unweighted mean age composition by return year across all populations--------
meanAgeComp <- ageSimple %>%
  # Exclude hatchery-origin fish
  filter(Population != "Crooked.Hatchery" & Population != "Ninilchik.Hatchery") %>%
  group_by(ReturnYear, Age) %>%
  summarize(PropMean = mean(Prop))

# Method 2: Fit a multinomial mixed-effects model ----------
# to the age composition data to test for effects of group (random), gear, and year.  


