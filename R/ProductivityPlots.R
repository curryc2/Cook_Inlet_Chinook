# ProductivityPlots.R
# Erik Schoen
# eschoen@alaska.edu
# 4-2018

# Cook Inlet Chinook project

# Plot brood-year productivity over time and against a few key predictors (flooding and high temps)

# # Load packages and read in data-------------

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(ggplot2)

covars <- read_csv("/Volumes/GoogleDrive/My Drive/Cook Inlet Chinook project_salmon data/Stream Temperatures/V6/covars.csv")
setwd("~/Desktop/Cook Inlet Chinook/Analysis")
spawnersRecruits <- read_csv("./data/SpawnersRecruits.csv")
theme_set(theme_bw(14))

# Munge data---------
# Standardize productivity indices: ln(Recruits/Spawner) and ln(Recruits/Spawner) core ages only
prod <- spawnersRecruits %>%
  group_by(Population) %>%
  mutate(prodTotal = scale(log(RecruitsPerSpawner)),
         prodCore = scale(log(CoreRecruitsPerSpawner))) %>%
  select(Population, BroodYear, prodTotal, prodCore)

# Double-check that productivity indices standardized properly (mean of each population should 
# be 0)
meanProd <- prod %>%
  group_by(Population) %>%
  summarize(meanProdTotal = mean(prodTotal, na.rm = T),
            meanProdCore = mean(prodCore, na.rm = T))
# Yes, good to go

# In what year was the lowest productivity value for each population?
lowestProd <- prod %>%
  group_by(Population) %>%
  summarize(minProdCore = min(prodCore, na.rm = T))

yearOfLowestProd <- lowestProd %>%
  left_join(prod, by = c("Population", "minProdCore" = "prodCore"))

# Join the environmental covariates
envProd <- prod %>%
  left_join(covars, by = c("Population", "BroodYear" = "Year")) %>%
  select(-X1, -Site)

# Standardize environmental covariates of interest
envProd <- envProd %>%
  group_by(Population) %>%
  mutate(zASON_max = scale(ASON_max),
         zMJJA_avg = scale(MJJA_avg),
         # lag this predictor by 1 year (since it applies to brood-year + 1)
         zMJJA_avg.lag1 = lead(zMJJA_avg),
         zmaxWkJA = scale(maxWkJA),
         meanWkJJA.lag1 = lead(meanWkJJA),
         zmeanWkJJA = scale(meanWkJJA),
         # lag this predictor by 1 year (since it applies to brood-year + 1)
         zmeanWkJJA.lag1 = lead(zmeanWkJJA))

# Plot the data----------
# First, make a plain plot of productivity over time (faceted by population)
prodCore.time <- ggplot(data = envProd, aes(x = BroodYear, y = prodCore)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, lty = "dotted") +
  facet_wrap(.~Population, scales = "free_y") +
  scale_x_continuous(name = "Brood Year", breaks = seq(1980, 2010, by = 10)) +
  scale_y_continuous(name = "Productivity (ln(Recruits/Spawner))")
prodCore.time
ggsave("./figs/Productivity timeseries.png", width = 8, height = 6)

# Next show max weekly temp during spawning as dot fill color
prodCore.time.temp <- prodCore.time +
  geom_point(aes(fill = maxWkJA), shape = 21, color = "black", size = 2) +
  # scale_fill_gradient(low = "blue", high = "red")
  # scale_fill_gradientn(colors = rainbow(5), trans = "reverse")
  scale_fill_gradient2(name = "Max. weekly\ntemperature\nduring\nspawning (C)", low="navy", mid="white", high="red", 
                       midpoint=13, limits=range(envProd$maxWkJA)) 
  # # put bottom-right corner of legend in bottom-right corner of plot
  # theme(legend.justification=c(1,0), legend.position=c(1,-0.15)) +
  # # shrink the font size of the legend title
  # theme(legend.title = element_text(size=12))
prodCore.time.temp
ggsave("./figs/Productivity timeseries_maxT_spawn.png", width = 10, height = 6)

# Then show max precip during spawning as dot fill color
prodCore.time.precip <- prodCore.time +
  geom_point(aes(fill = zASON_max), shape = 21, color = "black", size = 2) +
  # scale_fill_distiller(palette = "Blues", trans = "reverse")
  scale_fill_gradient2(name = "Maximum\nprecipitation\nduring\nspawning &\nincubation\n(SD)", 
                       low="navy", mid="white", high="red", 
                       midpoint=0, limits=range(envProd$zASON_max))
prodCore.time.precip
ggsave("./figs/Productivity timeseries_maxP_spawn.png", width = 10, height = 6)

# Now plot productivity vs the environmental covariates directly (not a time series)
prodCore.maxT_spawn <- ggplot(data = envProd, aes(x = maxWkJA, y = prodCore)) +
  geom_point(shape = 1) + 
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(name = "Max. weekly temp. during spawning (C)") +
  scale_y_continuous(name = "Productivity (ln(Recruits/Spawner))", limits=range(envProd$prodCore))
prodCore.maxT_spawn
ggsave("./figs/Productivity_maxT_spawn.png", width = 10, height = 6)

prodCore.maxT_spawn.popn <- ggplot(data = envProd, aes(x = maxWkJA, y = prodCore)) +
  geom_point(shape = 1) + 
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(.~Population) +
  scale_x_continuous(name = "Max. weekly temp. during spawning (C)") +
  scale_y_continuous(name = "Productivity (ln(Recruits/Spawner))", limits=range(envProd$prodCore))
prodCore.maxT_spawn.popn
ggsave("./figs/Productivity_maxT_spawn_by_popn.png", width = 10, height = 6)

prodCore.avgT_grow <- ggplot(data = envProd, aes(x = meanWkJJA.lag1, y = prodCore)) +
  geom_point(shape = 1) + 
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(name = "Mean weekly temp. during juvenile rearing (C)") +
  scale_y_continuous(name = "Productivity (ln(Recruits/Spawner))", limits=range(envProd$prodCore))
prodCore.avgT_grow
ggsave("./figs/Productivity_avgT_grow.png", width = 10, height = 6)

prodCore.avgT_grow.popn <- ggplot(data = envProd, aes(x = meanWkJJA.lag1, y = prodCore)) +
  geom_point(shape = 1) + 
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(.~Population) +
  scale_x_continuous(name = "Mean weekly temp. during juvenile rearing (C)") +
  scale_y_continuous(name = "Productivity (ln(Recruits/Spawner))", limits=range(envProd$prodCore))
prodCore.avgT_grow.popn
ggsave("./figs/Productivity_avgT_grow_by_popn.png", width = 10, height = 6)

prodCore.maxP_spawn <- ggplot(data = envProd, aes(x = zASON_max, y = prodCore)) +
  geom_point(shape = 1) + 
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(name = "Max. precip. during spawning & incubation (SD)") +
  scale_y_continuous(name = "Productivity (ln(Recruits/Spawner))", limits=range(envProd$prodCore))
prodCore.maxP_spawn
ggsave("./figs/Productivity_maxP_spawn.png", width = 10, height = 6)

prodCore.maxP_spawn.popn <- ggplot(data = envProd, aes(x = zASON_max, y = prodCore)) +
  geom_point(shape = 1) + 
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(.~Population) +
  scale_x_continuous(name = "Maximum monthly precipitation during spawning & incubation (SD)") +
  scale_y_continuous(name = "Productivity (ln(Recruits/Spawner))", limits=range(envProd$prodCore))
prodCore.maxP_spawn.popn
ggsave("./figs/Productivity_maxP_spawn_by_popn.png", width = 10, height = 6)

prodCore.avgP_grow <- ggplot(data = envProd, aes(x = zMJJA_avg.lag1, y = prodCore)) +
  geom_point(shape = 1) + 
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(name = "Mean precipitation during juvenile rearing (SD)") +
  scale_y_continuous(name = "Productivity (ln(Recruits/Spawner))", limits=range(envProd$prodCore))
prodCore.avgP_grow
ggsave("./figs/Productivity_avgP_grow.png", width = 10, height = 6)

prodCore.avgP_grow.popn <- ggplot(data = envProd, aes(x = zMJJA_avg.lag1, y = prodCore)) +
  geom_point(shape = 1) + 
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(.~Population) +
  scale_x_continuous(name = "Mean precipitation during juvenile rearing (SD)") +
  scale_y_continuous(name = "Productivity (ln(Recruits/Spawner))", limits=range(envProd$prodCore))
prodCore.avgP_grow.popn
ggsave("./figs/Productivity_avgP_grow_by_popn.png", width = 10, height = 6)

# Try showing precip on x and temp on color axis
prodCore.precip.temp <- prodCore.precip +
  geom_point(aes(fill = maxWkJA), shape = 21, color = "black", size = 2) +
  scale_fill_gradient2(name = "Max. weekly\ntemperature\nduring\nspawning (C)", low="navy", mid="white", high="red", 
                       midpoint=13, limits=range(envProd$maxWkJA)) 
prodCore.precip.temp
ggsave("./figs/Productivity_maxP_spawn_maxT_spawn.png", width = 10, height = 6)

prodCore.precip.temp.popn <- prodCore.precip.popn +
  geom_point(aes(fill = maxWkJA), shape = 21, color = "black", size = 2) +
  scale_fill_gradient2(name = "Max. weekly\ntemperature\nduring\nspawning (C)", low="navy", mid="white", high="red", 
                       midpoint=13, limits=range(envProd$maxWkJA)) 
prodCore.precip.temp.popn
ggsave("./figs/Productivity timeseries_maxP_spawn_maxT_spawn_by_popn.png", width = 10, height = 6)

