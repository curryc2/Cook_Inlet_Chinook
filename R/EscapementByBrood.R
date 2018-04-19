# EscapementByBrood.R
# Erik Schoen
# eschoen@alaska.edu
# 4-2018

# Cook Inlet Chinook project
# Convert escapement by return year to returns by brood year

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

# Calculate returns by brood year: 1) Escapement ------------------

# Join the escapement data to the age comp data and the mean age comps (for filling in NAs)
escapeByReturnXBrood <- escapeFull %>%
  select(Population, ReturnYear, Escapement, EscapementMethod) %>%
  # filter(!is.na(Escapement)) %>%
  # Make 5 rows for each population X return year (1 for each age)
  mutate(Age1.1 = NA, Age1.2 = NA, Age1.3 = NA, Age1.4 = NA, Age1.5 = NA) %>%
  gather(key = "Age", value = "Prop", 5:9) %>%
  select(-Prop) %>%
  # Join the age data and the global mean age composition
  full_join(ageSimple, by = c("Population", "ReturnYear", "Age")) %>%
  full_join(meanAgeComp, by = c("ReturnYear", "Age")) %>%
  # Remove the marine fisheries (no escapement data)
  filter(Population != "NSN" & Population != "ESSN" & Population != "WSSN" & 
           Population != "Tyonek") %>%
  # Now make a bunch of calculations...
  mutate(
          # Use the population-specific age composition data if available, 
          # or the average age comp for that return year if not
         PropEst = ifelse(is.na(Prop), PropMean, Prop),
         # Calculate total ages in years
         AgeFW = as.numeric(str_sub(Age, start = 4, end = 4)),
         AgeSW = as.numeric(str_sub(Age, start = 6, end = 6)),
         AgeYr = AgeFW + AgeSW + 1,
         # Calculate Escapement for each combination of Population, ReturnYear, and BroodYear
         BroodYear = ReturnYear - AgeYr,
         EscapeByReturnXBrood = Escapement * PropEst
  )

# Calculate escapement by brood year
escapeByBrood <- escapeByReturnXBrood %>%
  group_by(Population, BroodYear) %>%
  summarize(Escapement = sum(EscapeByReturnXBrood, na.rm = T),
            NumAgeClasses = sum(!is.na(EscapeByReturnXBrood)),
            CompleteBrood = NumAgeClasses == 5)

# Double-check that total escapement by return year = total returns by brood year
totalEscapementRY <- escapeFull %>%
  filter(ReturnYear > 1978) %>%
  summarize(totalEscapement = sum(Escapement, na.rm = T))
totalEscapementBY <- sum(escapeByBrood$Escapement, na.rm = T)
totalEscapementRY[1,1] == totalEscapementBY
# Yes, they're equal, so no escapement data was lost in the conversion

# Trim years and populations outside the study
escapeByBrood <- escapeByBrood %>%
  filter(BroodYear > 1979 & BroodYear < 2009) %>%
  filter(Population != "Funny" & Population != "Slikok" & Population != "Quartz" & 
           Population != "Crooked.Hatchery" & Population != "Ninilchik.Hatchery") %>%
  # Make a new variable EscapementCompleteBrood, listing incomplete broods as NA so they don't
  # get plotted
  mutate(EscapementCompleteBrood = ifelse(CompleteBrood, Escapement, NA))

# Plot the results -----------------

# Set escapement to NA for incomplete broods, so ggplot doesn't interpoloate across missing values
escapementByBrood.plot <- ggplot(data = escapeByBrood, aes(x = BroodYear, 
                                                        y = EscapementCompleteBrood, 
                                                        color = Population)) +
  geom_line() +
  scale_x_continuous(name = "Brood Year", breaks = seq(1980, 2010, by = 10), limits = c(1980, 2010)) +
  scale_y_continuous(name = "Chinook salmon returns (escapement only)", labels = scales::comma) 
escapementByBrood.plot
ggsave("./figs/Escapement by brood year.png")

escapementByBroodLog.plot <- escapementByBrood.plot +
  scale_y_log10(name = "Chinook salmon returns (escapement only)", labels = scales::comma,
                breaks = c(10, 100, 1000, 10000, 100000)) 
escapementByBroodLog.plot
ggsave("./figs/Escapement by brood year_log scale.png")

