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
age.raw <- read_csv("AgeComp_Raw.csv")

theme_set(theme_bw() + theme(panel.grid.minor = element_blank()))

# Compile and QA data--------------

# Correct age comp proportions that don't add to 1
# (ES: These errors are due to rounding or other errors in the original source.
# I QC'd the biggest offenders and found no data entry errors)
age.raw <- age.raw %>%
  # Write over the SumProportions column from the spreadsheet with a new sum calculated in R
  # (to eliminate change of rounding errors)
  mutate(SumProportions1 = Age0.1 + Age0.2 + Age1.1 + Age0.3 + Age1.2 + Age2.1 + Age0.4 +
           Age1.3 + Age2.2 + Age0.5 + Age1.4 + Age2.3 + Age1.5 + Age2.4 + Age2.5 + Age1.6,
         
         #Divide each proportion by the SumProportions for that row.  New values should add to 1
         Age0.1 = Age0.1/SumProportions1,
         Age0.2 = Age0.2/SumProportions1,
         Age1.1 = Age1.1/SumProportions1,
         Age0.3 = Age0.3/SumProportions1,
         Age1.2 = Age1.2/SumProportions1,
         Age2.1 = Age2.1/SumProportions1,
         Age0.4 = Age0.4/SumProportions1,
         Age1.3 = Age1.3/SumProportions1,
         Age2.2 = Age2.2/SumProportions1,
         Age0.5 = Age0.5/SumProportions1,
         Age1.4 = Age1.4/SumProportions1,
         Age2.3 = Age2.3/SumProportions1,
         Age1.5 = Age1.5/SumProportions1,
         Age2.4 = Age2.4/SumProportions1,
         Age2.5 = Age2.5/SumProportions1,
         Age1.6 = Age1.6/SumProportions1,
         # Verify that the corrected values add to 1
         SumProportions2 = Age0.1 + Age0.2 + Age1.1 + Age0.3 + Age1.2 + Age2.1 + Age0.4 +
           Age1.3 + Age2.2 + Age0.5 + Age1.4 + Age2.3 + Age1.5 + Age2.4 + Age2.5 + Age1.6)

# Summarize the age composition data for commercial fisheries where more than one area was
# sampled per year.  Resulting table has 1 row per population (or fishery) per gear (and mesh size)
# per year

age <- age.raw %>%
  # Add a Group column that includes Population (for esc. sampling) and Fishery (for comm fish sampling)
  mutate(Group = if_else(is.na(Population), Fishery, Population)) %>%
  group_by(Group, Year, Source, Gear, Mesh) %>%
  summarize_at(vars(Age0.1:Age1.6), mean) %>%
  mutate(SumProportions = Age0.1 + Age0.2 + Age1.1 + Age0.3 + Age1.2 + Age2.1 + Age0.4 +
           Age1.3 + Age2.2 + Age0.5 + Age1.4 + Age2.3 + Age1.5 + Age2.4 + Age2.5 + Age1.6)

# Calculate sample sizes for each new group 
# Assume that when multiple areas were sampled per group, that they were sampled in proportion to
# their abundance (so that n_total = sum of all n_areas)
age.n <- age.raw %>%
  mutate(Group = if_else(is.na(Population), Fishery, Population)) %>%
  group_by(Group, Year, Source, Gear, Mesh) %>%
  summarize(n = sum(n)) 

# Join sample sizes to the age comp dataframe
age <- left_join(age, age.n)

# Save the age comp dataframe to csv
write_csv(age, "/Users/ErikSchoen1/Desktop/Cook Inlet Chinook/Analysis/data/AgeComp.csv")

# Plot the data------------

# Reshape data into long form
age.long <- age %>%
  gather("Age", "Proportion", 6:21) 

# Filter out the uncommon ages and hatchery fish, and make a new variable GroupGear 
# so we can plot each group separately by gear type
age.long.common <- age.long %>%
  filter(Age == "Age1.1" | Age == "Age1.2" | Age == "Age1.3" | Age == "Age1.4" | Age == "Age1.5") %>%
  filter(Group != "Ninilchik.Hatchery" & Group != "Crooked.Hatchery") %>%
  mutate(GroupGear = ifelse(is.na(Mesh), str_c(Group, Gear, sep = " "), 
                            str_c(Group, Mesh, Gear, sep = " ")))

age.by.gear.plot <- ggplot(data = age.long.common, aes(x = Year, y = Proportion, group = GroupGear, 
                                                       color = Gear)) +
  facet_grid(Age ~ .) +
  geom_line() +
  labs(x = "Return year", y = "Age proportion") +
  scale_x_continuous(breaks = seq(1980, 2015, 5))
age.by.gear.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/output/Age comp by gear type.png")

age.by.group.plot <- ggplot(data = age.long.common, aes(x = Year, y = Proportion, group = GroupGear, 
                                                        color = Group)) +
  facet_grid(Age ~ .) +
  geom_line() +
  labs(x = "Return year", y = "Age proportion") +
  scale_color_discrete(name = "River or fishery") +
  scale_x_continuous(breaks = seq(1980, 2015, 5))
age.by.group.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/output/Age comp by river or fishery.png")

# Plot the age comp for Kenai and ESSN only to compare gear types
age.long.kenai <- age.long.common %>%
  filter(Group == "ESSN" | Group == "Kenai.Late") 

age.by.gear.kenai.plot <- ggplot(data = age.long.kenai, 
                                 aes(x = Year, y = Proportion, color = GroupGear)) +
  facet_grid(Age ~ .) +
  geom_line() +
  # geom_point(shape = 1, fill = NA) +
  labs(x = "Return year", y = "Age proportion") +
  scale_color_discrete(name = "Gear type") +
  scale_x_continuous(breaks = seq(1980, 2015, 5))
age.by.gear.kenai.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/output/Kenai late run age comp by gear type.png")

# Plot the age comp for Deshka and NSN only to compare gear types
age.long.deshka <- age.long.common %>%
  filter(Group == "NSN" | Group == "Deshka") 

age.by.gear.deshka.plot <- ggplot(data = age.long.deshka, 
                                  aes(x = Year, y = Proportion, color = GroupGear)) +
  facet_grid(Age ~ .) +
  geom_line() +
  geom_point(shape = 1, fill = NA) +
  labs(x = "Return year", y = "Age proportion") +
  scale_color_discrete(name = "Gear type") +
  scale_x_continuous(breaks = seq(1980, 2015, 5))
age.by.gear.deshka.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/output/Deshka age comp by gear type.png")

