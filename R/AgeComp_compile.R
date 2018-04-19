# AgeComp_compile.R
# Erik Schoen
# eschoen@alaska.edu
# 2-2018

# Cook Inlet Chinook project
# Compile, QA, and summarize raw age composition data from ADFG

# Load packages and read in data-------------
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)
library(stringr)

setwd("~/Desktop/Cook Inlet Chinook/Analysis/data")

# Read in data from AgeComp tab
ageRaw <- read_csv("AgeComp_Raw.csv")

theme_set(theme_bw() + theme(panel.grid.minor = element_blank()))

# Compile and QA data--------------

# Reshape into long format
ageRawLong <- ageRaw %>%
  select(-SumProportions) %>%
  gather(key = "Age", value = "Prop", 9:24)

# Decide which rare ages to exclude from the analysis
# 1) Calculate how many total fish were aged (for years/sites where sample sizes
# and FW ages were reported.  This also excludes hatchery-origin populations)
ageRawLongClean <- ageRawLong %>%
  filter(!is.na(n)) %>%
  filter(is.na(FWAgeAssumed)) %>%
  mutate(nAtAge = n * Prop)

ageCompN <- sum(ageRawLongClean$nAtAge, na.rm = T)
ageCompN

# 2) Calculate overall age composition of all aged fish (for years/sites where sample sizes
# were reported, and excluding hatchery-origin populations)
ageCompOverall <- ageRawLongClean %>%
  mutate(Number = Prop * n) %>%
  group_by(Age) %>%
  summarize(NAtAgeOverall = sum(Number)) %>%
  mutate(Prop = NAtAgeOverall / ageCompN)

# double check that proportions add to 1
sum(ageCompOverall$Prop)

# Each of the 0-freshwater and 2-freshwater age classes made up < 1% of the overall age comp
# Calculate % of 0-fresh and 2-fresh
Prop0FW <- sum(ageCompOverall[1:5,3])
Prop0FW
Prop1FW <- sum(ageCompOverall[6:11,3])
Prop1FW
Prop2FW <- sum(ageCompOverall[12:16,3])
Prop2FW

PropPrimaryAges <- sum(ageCompOverall[6:10,3])
PropPrimaryAges
# Result: Ages 1.1 through 1.5 made up 99.4% of the overall age comp, so we excluded the other age
# classes for simplicity

#### Remove the rare age classes from the dataset and recalculate age composition #######
# Remove rare age classes
ageRawSimple <- ageRaw %>%
  select(-Age0.1, -Age0.2, -Age0.3, -Age0.4, -Age0.5, -Age1.6, -Age2.1, -Age2.2, -Age2.3, 
         -Age2.4, -Age2.5, -SumProportions)

# Recalculate age composition so proportions add to 1
# (Note: These errors are due to inclusion of rare age classes and rounding or other errors in the
# original source.  I QC'd the biggest offenders and found no data entry errors)
ageRawSimple <- ageRawSimple %>%
  # Make new SumProportions1 column calculated in R 
  mutate(SumProportions1 = Age1.1 + Age1.2 + Age1.3 + Age1.4 + Age1.5,
         
         #Divide each proportion by the SumProportions for that row.  New values should add to 1
         Age1.1 = Age1.1/SumProportions1,
         Age1.2 = Age1.2/SumProportions1,
         Age1.3 = Age1.3/SumProportions1,
         Age1.4 = Age1.4/SumProportions1,
         Age1.5 = Age1.5/SumProportions1,
         # Verify that the corrected values add to 1
         SumProportions2 = Age1.1 + Age1.2 + Age1.3 + Age1.4 + Age1.5)

# Summarize the age composition data for commercial fisheries where more than one area was
# sampled per year.  Resulting table has 1 row per population (or fishery) per gear (and mesh size)
# per year

age <- ageRawSimple %>%
  # Add a Group column that includes Population (for esc. sampling) and Fishery (for comm fish sampling)
  mutate(Group = if_else(is.na(Population), Fishery, Population)) %>%
  group_by(Group, Year, Source, Gear, Mesh) %>%
  summarize_at(vars(Age1.1:Age1.5), mean) %>%
  mutate(SumProportions = Age1.1 + Age1.2 + Age1.3 + Age1.4 + Age1.5)

# Calculate sample sizes for each new group 
# Assume that when multiple areas were sampled per group, that they were sampled in proportion to
# their abundance (so that n_total = sum of all n_areas)
age.n <- ageRaw %>%
  mutate(Group = if_else(is.na(Population), Fishery, Population)) %>%
  group_by(Group, Year, Source, Gear, Mesh) %>%
  summarize(n = sum(n)) 

# Join sample sizes to the age comp dataframe and reshape into long format
age <- age %>%
  left_join(age.n) %>%
  select(-SumProportions) %>%
  gather(key = "Age", value = "Prop", 6:10)

# Save the full age comp dataframe to csv (includes data from mixed-stock fisheries as
# well as multiple estimates for some groups/years for different gear types).
# This will be used for fitting a model to age comp data
write_csv(age, "AgeFull.csv")

# Generate a simplified version of the age comp data with one row for each population X year
# (population-specific age data only, no data from mixed-stock fisheries)
ageSimple <- age %>%
  # filter(Source != "Commercial Harvest") %>%
  group_by(Group, Year, Age) %>%
  summarize(Prop = mean(Prop)) %>%
  rename(Population = Group, ReturnYear = Year)

write_csv(ageSimple, "AgeSimple.csv")
# Plot the data------------

# Filter out the hatchery fish, and make a new variable GroupGear 
# so we can plot each group separately by gear type
agePlot <- age %>%
  filter(Group != "Ninilchik.Hatchery" & Group != "Crooked.Hatchery") %>%
  mutate(GroupGear = ifelse(is.na(Mesh), str_c(Group, Gear, sep = " "), 
                            str_c(Group, Mesh, Gear, sep = " ")))

ageByGear.plot <- ggplot(data = agePlot, aes(x = Year, y = Prop, group = GroupGear, 
                                                       color = Gear)) +
  facet_grid(Age ~ .) +
  geom_line() +
  labs(x = "Return year", y = "Age Prop") +
  scale_x_continuous(breaks = seq(1980, 2015, 5))
ageByGear.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/figs/Age comp by gear type.png")

ageByGroup.plot <- ggplot(data = agePlot, aes(x = Year, y = Prop, group = GroupGear, 
                                                        color = Group)) +
  facet_grid(Age ~ .) +
  geom_line() +
  labs(x = "Return year", y = "Age Prop") +
  scale_color_discrete(name = "River or fishery") +
  scale_x_continuous(breaks = seq(1980, 2015, 5))
ageByGroup.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/figs/Age comp by river or fishery.png")

# Plot the age comp for Kenai and ESSN only to compare gear types
agePlotKenai <- agePlot %>%
  filter(Group == "ESSN" | Group == "Kenai.Late") 

ageByGearKenai.plot <- ggplot(data = agePlotKenai, 
                                 aes(x = Year, y = Prop, color = GroupGear)) +
  facet_grid(Age ~ .) +
  geom_line() +
  # geom_point(shape = 1, fill = NA) +
  labs(x = "Return year", y = "Age Prop") +
  scale_color_discrete(name = "Gear type") +
  scale_x_continuous(breaks = seq(1980, 2015, 5))
ageByGearKenai.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/figs/Kenai late run age comp by gear type.png")

# Plot the age comp for Deshka and NSN only to compare gear types
agePlotDeshka <- agePlot %>%
  filter(Group == "NSN" | Group == "Deshka") 

ageByGearDeshka.plot <- ggplot(data = agePlotDeshka, 
                                  aes(x = Year, y = Prop, color = GroupGear)) +
  facet_grid(Age ~ .) +
  geom_line() +
  geom_point(shape = 1, fill = NA) +
  labs(x = "Return year", y = "Age Prop") +
  scale_color_discrete(name = "Gear type") +
  scale_x_continuous(breaks = seq(1980, 2015, 5))
ageByGearDeshka.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/figs/Deshka age comp by gear type.png")

