# ProductivityPlots.R
# Erik Schoen
# eschoen@alaska.edu
# 4-2018

# Cook Inlet Chinook project

# Post-hoc Exploratory Data Analysis
# Plot brood-year productivity over time and against environmental drivers to determine
# the shapes of the associations and illustrate how changes in productivity over time were
# associated with drivers

# # Load packages and read in data-------------

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(ggplot2)
library(cowplot)

setwd("~/Desktop/Cook Inlet Chinook/Analysis")
# source("./JAGS/Hierarchical-JAGS-Final-LinearRicker.R")
covarsFW <- read_csv("./data/covars.csv")
covarsSW <- read_csv("./JAGS/Data/10.25.18_update/covars.list.csv")
spawnersRecruits <- read_csv("./data/SpawnersRecruits.csv")
theme_set(theme_bw(12))

# Munge data---------

# Define site names for pretty labels
siteNames <- c("Alexander", "Anchor", "Campbell", "Chuitna", "Chulitna", "Crooked", "Deep",
               "Deshka", "Kenai late run", "Little Susitna", "Little Willow", "Montana", "Ninilchik",
               "Theodore", "Willow")

# Calculate basic productivity indices: ln(Recruits/Spawner) and ln(Recruits/Spawner) core ages
# only. Then standardize the productivity indices
prod <- spawnersRecruits %>%
  mutate(Population = factor(Population, labels = siteNames)) %>%
  group_by(Population) %>%
  mutate(Spawners = Spawners2.nom,
         zSpawners = scale(Spawners),
         ln.rpsTotal = log(RecruitsPerSpawner),
         ln.rpsCore = log(CoreRecruitsPerSpawner),
         prodTotal = scale(ln.rpsTotal),
         prodCore = scale(ln.rpsCore)) %>%
  select(Population, BroodYear, Spawners, zSpawners, ln.rpsTotal, ln.rpsCore, prodTotal, prodCore)

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

# Convert Population to a factor and set label names to match the prod dataframe
covarsFW <- covarsFW %>%
  mutate(Population = factor(Population, labels = siteNames))

# Join the FW environmental covariates
envProd <- prod %>%
  left_join(covarsFW, by = c("Population", "BroodYear" = "Year")) %>%
  select(-X1, -Site)

# Standardize and lag FW environmental covariates of interest
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
         zmeanWkJJA.lag1 = lead(zmeanWkJJA),
         # calculate a compound indicator of hot years with high precip (maxP_spawn + maxT_spawn)
         maxPT_spawn = zASON_max + zmaxWkJA
         )

# Filter down, standardize, and lag NPGO
npgo <- covarsSW %>%
  filter(Covar == "NPGO",
         Year > 1979,
         Year < 2012) %>%
  mutate(NPGO = value,
         zNPGO = scale(value),
         NPGO.lag2 = lead(NPGO, 2),
         zNPGO.lag2 = lead(zNPGO, 2)) %>%
  select(Year, NPGO, zNPGO, NPGO.lag2, zNPGO.lag2)

# Plot NPGO trend over time for brief description in the results
npgo.lag2.trend <- ggplot(data = npgo, aes(x = Year, y = zNPGO.lag2)) + 
  geom_col() +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Brood year") +
  scale_y_continuous(name = "NPGO (std) during\nyear of ocean entry")
npgo.lag2.trend

# Join NPGO data to envProd data frame
envProd <- left_join(envProd, npgo, by = c("BroodYear" = "Year"))

# Calculate Ricker residuals to plot against covariates
# First plot ln(R/S) vs spawners by population to see where the residuals come from
ricker.plot <- ggplot(data = envProd, aes(x = Spawners/1000, y = ln.rpsCore)) +
  # geom_point() +
  geom_text(aes(label = BroodYear), size = 3) +
  geom_smooth(method = "lm") +
  facet_wrap(.~Population, ncol = 3, scales = "free") +
  scale_x_continuous(name = "Spawners (1,000s)") +
  scale_y_continuous(name = "Productivity (ln[Recruits/Spawner])")
ricker.plot
ggsave("./figs/Ricker residuals.png", width = 12, height = 10)

# Fit linearized simple Ricker models (no environmental covariates) for each population
envProd.no.nas <- drop_na(envProd, prodCore)
rickers <- lm(ln.rpsCore ~ Population + Population:Spawners, data = envProd.no.nas)
envProd.no.nas$ln.rpsCore.pred <- predict(rickers)
envProd.no.nas$resid <- envProd.no.nas$ln.rpsCore - envProd.no.nas$ln.rpsCore.pred
# Join residuals to envProd dataframe with NAs intact (NAs are needed to avoid interpolating
# across missing data years in the timeseries plots)
envProd.no.nas <- select(envProd.no.nas, Population, BroodYear, resid)
envProd <- left_join(envProd, envProd.no.nas, by = c("Population", "BroodYear"))
                     
# Plot the data----------
# 1A) Productivity (ln R/S) time-series plots-----------
# First, make a plain plot of productivity over time (faceted by population)
prodCore.time <- ggplot(data = envProd, aes(x = BroodYear, y = prodCore)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, lty = "dotted") +
  facet_wrap(.~Population, ncol = 3) +
  # facet_wrap(.~Population, ncol = 3, scales = "free_y") + # facets with free y-axis scales
  scale_x_continuous(name = "Brood Year", breaks = seq(1980, 2010, by = 10)) +
  expand_limits(x = c(1979,2011)) +
  scale_y_continuous(name = "Productivity index (SD)")
prodCore.time
ggsave("./figs/Productivity timeseries.png", width = 6, height = 6)

# Same plot with 2003-2007 highlighted
prodCore.time.2003.2007 <- prodCore.time +
  geom_rect(xmin = 2003, xmax = 2007, ymin = -3, ymax = 3, fill = "grey75", alpha = 0.2)+
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, lty = "dotted") 
prodCore.time.2003.2007
ggsave("./figs/Productivity timeseries_2003-2007.png", width = 6, height = 6)

# Next show max precip during spawning as dot fill color
prodCore.time.precip <- prodCore.time.2003.2007 +
  geom_point(aes(fill = zASON_max), shape = 21, color = "black", size = 2) +
  # scale_fill_distiller(palette = "Blues", trans = "reverse")
  scale_fill_gradient2(name = "Maximum\nprecipitation\nduring\nspawning &\nincubation\n(SD)", 
                       low="yellow", mid="white", high="navy", 
                       midpoint=0, limits=range(envProd$zASON_max))
prodCore.time.precip
ggsave("./figs/Productivity timeseries_maxP_spawn.png", width = 8, height = 6)

# # Then show max weekly temp during spawning as dot fill color (degrees C units)
# prodCore.time.temp <- prodCore.time.2003.2007 +
#   geom_point(aes(fill = maxWkJA), shape = 21, color = "black", size = 2) +
#   # scale_fill_gradient(low = "blue", high = "red")
#   # scale_fill_gradientn(colors = rainbow(5), trans = "reverse")
#   scale_fill_gradient2(name = "Maximum\nweekly\ntemperature\nduring\nspawning\n(˚C)", low="navy", mid="white", high="red", 
#                        midpoint=13, limits=range(envProd$maxWkJA)) 
#   # # shrink the font size of the legend title
#   # theme(legend.title = element_text(size=12))
# prodCore.time.temp
# ggsave("./figs/Productivity timeseries_maxT_spawn.png", width = 8, height = 6)
# # This is hard to interpret because most dots are close to white

# Then show max weekly temp during spawning as dot fill color (SD units)
prodCore.time.temp.SD <- prodCore.time.2003.2007 +
  geom_point(aes(fill = zmaxWkJA), shape = 21, color = "black", size = 2) +
  # scale_fill_gradient(low = "blue", high = "red")
  # scale_fill_gradientn(colors = rainbow(5), trans = "reverse")
  scale_fill_gradient2(name = "Maximum\nweekly\ntemperature\nduring\nspawning\n(maxT_spawn;\nSD)", 
                       low="navy", mid="white", high="red", 
                       midpoint=0, limits=range(envProd$zmaxWkJA)) 
prodCore.time.temp.SD
ggsave("./figs/Productivity timeseries_maxT_spawn_SD.png", width = 8, height = 6)
# This is easier to interpret than the one in degrees C units

# Then show max weekly temp during spawning as dot fill color (SD units) 
# AND spawning abundance as dot size (SD units)
prodCore.time.temp.esc.SD <- prodCore.time.2003.2007 +
  geom_point(aes(fill = zmaxWkJA, size = zSpawners), shape = 21, color = "black") +
  scale_fill_gradient2(name = "Maximum\nweekly\ntemperature\nduring\nspawning\n(maxT_spawn;\nSD)", 
                       low="navy", mid="white", high="red", 
                       midpoint=0, limits=range(envProd$zmaxWkJA)) +
  scale_size_continuous(name = "Escapement\n(SD)")
prodCore.time.temp.esc.SD
ggsave("./figs/Productivity timeseries_maxT_spawn_escape_SD.png", 
       width = 8, height = 6)

# Next show NPGO as dot fill color
prodCore.time.npgo <- prodCore.time.2003.2007 +
  geom_point(aes(fill = zNPGO.lag2), shape = 21, color = "black", size = 2) +
  # scale_fill_distiller(palette = "Blues", trans = "reverse")
  scale_fill_gradient2(name = "North Pacific\nGyre Oscillation\n(NPGO; SD)", 
                       low="navy", mid="white", high="red", 
                       midpoint=0, limits=range(envProd$zASON_max))
prodCore.time.npgo
ggsave("./figs/Productivity timeseries_npgo.png", width = 8, height = 6)

# # Finally, show max weekly temp during spawning as dot fill color and maxP_spawn as dot size
# prodCore.time.maxT_spawn_maxP_spawn <- prodCore.time.2003.2007 +
#   geom_point(aes(fill = zmaxWkJA, size = zASON_max), shape = 21, color = "black") +
#   scale_fill_gradient2(name = "Maximum\nweekly\ntemperature\nduring\nspawning\n(maxT_spawn;\nSD)", 
#                        low="navy", mid="white", high="red", 
#                        midpoint=0, limits=range(envProd$zmaxWkJA)) +
#   scale_size_continuous(name = "Maximum\nprecipitation\nduring\nspawning &\nincubation\n(maxP_spawn;\nSD)",
#                         range = c(0.5, 4))
# prodCore.time.maxT_spawn_maxP_spawn
# ggsave("./figs/Productivity timeseries_maxT_spawn_maxP_spawn.png", width = 8, height = 6)
# This is way too complicated to interpret

# Plot maxP_spawn + maxT_spawn on color axis (SD units)
prodCore.time.maxPT_spawn <- prodCore.time.2003.2007 +
  geom_point(aes(fill = maxPT_spawn), shape = 21, color = "black", size = 2) +
  # scale_fill_gradient(low = "blue", high = "red")
  # scale_fill_gradientn(colors = rainbow(5), trans = "reverse")
  scale_fill_gradient2(name = "maxP_spawn +\nmaxT_spawn\n(SD)", 
                       low="navy", mid="white", high="red", 
                       midpoint=0, limits=range(envProd$maxPT_spawn)) 
prodCore.time.maxPT_spawn
ggsave("./figs/Productivity timeseries_maxPT_spawn.png", width = 8, height = 6)

# Productivity timeseries plots for Kenai late run population only (for use in ppt)
prodCore.time.Kenai <- ggplot(data = filter(envProd, Population == "Kenai late run"),
                               aes(x = BroodYear, y = prodCore)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, lty = "dotted") +
  scale_x_continuous(name = "Brood Year", breaks = seq(1980, 2010, by = 10)) +
  expand_limits(x = 2011) +
  scale_y_continuous(name = "Productivity index (SD)") 
prodCore.time.Kenai
ggsave("./figs/Productivity timeseries Kenai.png", width = 4, height = 3)

prodCore.time.Kenai.2003.2007 <- prodCore.time.Kenai +
  geom_rect(xmin = 2003, xmax = 2007, ymin = -2.5, ymax = 2.5, fill = "grey75", alpha = 0.2)+
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, lty = "dotted") 
prodCore.time.Kenai.2003.2007
ggsave("./figs/Productivity timeseries Kenai_2003-2007.png", width = 4, height = 3)

prodCore.time.Kenai.maxP_spawn <- prodCore.time.Kenai.2003.2007 +
geom_point(aes(fill = zASON_max), shape = 21, color = "black", size = 2) +
  scale_fill_gradient2(name = "Maximum\nprecipitation\nduring\nspawning &\nincubation\n(maxP_spawn; SD)", 
                       low="yellow", mid="white", high="navy", 
                       midpoint=0, limits=range(envProd$zASON_max))
prodCore.time.Kenai.maxP_spawn
ggsave("./figs/Productivity timeseries Kenai_maxP_spawn.png", width = 6, height = 3)

prodCore.time.Kenai.maxT_spawn <- prodCore.time.Kenai.2003.2007 +
  geom_point(aes(fill = zmaxWkJA), shape = 21, color = "black", size = 2) +
  scale_fill_gradient2(name = "Maximum\nweekly\ntemperature\nduring\nspawning\n(SD)", low="navy", mid="white", high="red", 
                       midpoint=0) 
prodCore.time.Kenai.maxT_spawn
ggsave("./figs/Productivity timeseries Kenai_maxT_spawn.png", width = 6, height = 3)

prodCore.time.Kenai.maxT_spawn_esc <- prodCore.time.Kenai.2003.2007 +
  geom_point(aes(fill = zmaxWkJA, size = zSpawners), shape = 21, color = "black") +
  scale_fill_gradient2(name = "Maximum\nweekly\ntemperature\nduring\nspawning\n(SD)", low="navy", mid="white", high="red", 
                       midpoint=0) +
  scale_size_continuous(name = "Escapement\n(SD)")
prodCore.time.Kenai.maxT_spawn_esc
ggsave("./figs/Productivity timeseries Kenai_maxT_spawn_esc.png", width = 8, height = 5)

prodCore.time.Kenai.maxPT_spawn <- prodCore.time.Kenai.2003.2007 +
  geom_point(aes(fill = maxPT_spawn), shape = 21, color = "black", size = 2) +
  scale_fill_gradient2(name = "maxP_spawn + maxT_spawn (SD)", low="navy", mid="white", high="red", 
                       midpoint=0) 
prodCore.time.Kenai.maxPT_spawn
ggsave("./figs/Productivity timeseries Kenai_maxPT_spawn.png", width = 6, height = 3)

# Productivity timeseries plot for 1 population only (Deshka)
prodCore.time.Deshka <- ggplot(data = filter(prod, Population == "Deshka"),
                               aes(x = BroodYear, y = prodCore)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, lty = "dotted") +
  scale_x_continuous(name = "Brood Year", breaks = seq(1980, 2010, by = 10)) +
  expand_limits(x = 2011) +
  scale_y_continuous(name = "Productivity index (SD)")
prodCore.time.Deshka
ggsave("./figs/Productivity timeseries Deshka.png", width = 4, height = 3)

prodCore.time.Deshka.2003.2007 <- prodCore.time.Deshka +
  geom_rect(xmin = 2003, xmax = 2007, ymin = -2.5, ymax = 2.5, fill = "grey75", alpha = 0.2)+
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, lty = "dotted") 
prodCore.time.Deshka.2003.2007
ggsave("./figs/Productivity timeseries Deshka_2003-2007.png", width = 4, height = 3)

# 1B) Productivity after density dependence (ricker residuals) time-series plots-----------
# First, make a plain plot of productivity (ricker residuals) over time (faceted by population)
resid.time <- ggplot(data = envProd, aes(x = BroodYear, y = resid)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, lty = "dotted") +
  facet_wrap(.~Population, ncol = 3, scales = "free_y") +
  # facet_wrap(.~Population, ncol = 3, scales = "free_y") + # facets with free y-axis scales
  scale_x_continuous(name = "Brood Year", breaks = seq(1980, 2010, by = 10)) +
  expand_limits(x = c(1979,2011)) +
  scale_y_continuous(name = "Productivity (Ricker residuals)")
resid.time
ggsave("./figs/Productivity residual timeseries.png", width = 6, height = 6)

# Same plot with 2003-2009 low-productivity period highlighted
resid.time.2003.2009 <- resid.time +
  geom_rect(xmin = 2003, xmax = 2009, ymin = -3, ymax = 3, fill = "grey75", alpha = 0.2)+
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, lty = "dotted") 
resid.time.2003.2009
ggsave("./figs/Productivity residual timeseries_2003-2009.png", width = 6, height = 6)

# Next show max precip during spawning as dot fill color
resid.time.precip <- resid.time.2003.2009 +
  geom_point(aes(fill = zASON_max), shape = 21, color = "black", size = 2) +
  # scale_fill_distiller(palette = "Blues", trans = "reverse")
  scale_fill_gradient2(name = "Maximum\nprecipitation\nduring\nspawning &\nincubation\n(SD)", 
                       low="yellow", mid="white", high="navy", 
                       midpoint=0, limits=range(envProd$zASON_max))
resid.time.precip
ggsave("./figs/Productivity residual timeseries_maxP_spawn.png", width = 8, height = 6)

# # Then show max weekly temp during spawning as dot fill color (degrees C units)
# resid.time.temp <- resid.time.2003.2009 +
#   geom_point(aes(fill = maxWkJA), shape = 21, color = "black", size = 2) +
#   # scale_fill_gradient(low = "blue", high = "red")
#   # scale_fill_gradientn(colors = rainbow(5), trans = "reverse")
#   scale_fill_gradient2(name = "Maximum\nweekly\ntemperature\nduring\nspawning\n(˚C)", low="navy", mid="white", high="red", 
#                        midpoint=13, limits=range(envProd$maxWkJA)) 
#   # # shrink the font size of the legend title
#   # theme(legend.title = element_text(size=12))
# resid.time.temp
# ggsave("./figs/Productivity residual timeseries_maxT_spawn.png", width = 8, height = 6)
# # This is hard to interpret because most dots are close to white

# Then show max weekly temp during spawning as dot fill color (SD units)
resid.time.temp.SD <- resid.time.2003.2009 +
  geom_point(aes(fill = zmaxWkJA), shape = 21, color = "black", size = 2) +
  # scale_fill_gradient(low = "blue", high = "red")
  # scale_fill_gradientn(colors = rainbow(5), trans = "reverse")
  scale_fill_gradient2(name = "Maximum\nweekly\ntemperature\nduring\nspawning\n(SD)", 
                       low="navy", mid="white", high="red", 
                       midpoint=0, limits=range(envProd$zmaxWkJA)) 
resid.time.temp.SD
ggsave("./figs/Productivity residual timeseries_maxT_spawn_SD.png", width = 8, height = 6)
# This is easier to interpret than the one in degrees C units

# Then show max weekly temp during spawning as dot fill color (SD units)
# AND spawning abundance as dot size (SD units)
resid.time.temp.esc.SD <- resid.time.2003.2009 +
  geom_point(aes(fill = zmaxWkJA), shape = 21, color = "black", size = 2) +
  # scale_fill_gradient(low = "blue", high = "red")
  # scale_fill_gradientn(colors = rainbow(5), trans = "reverse")
  scale_fill_gradient2(name = "Maximum\nweekly\ntemperature\nduring\nspawning\n(SD)", 
                       low="navy", mid="white", high="red", 
                       midpoint=0, limits=range(envProd$zmaxWkJA)) 
resid.time.temp.SD
ggsave("./figs/Productivity residual timeseries_maxT_spawn_SD.png", width = 8, height = 6)
# This is easier to interpret than the one in degrees C units

# Next show NPGO as dot fill color
resid.time.npgo <- resid.time.2003.2009 +
  geom_point(aes(fill = zNPGO.lag2), shape = 21, color = "black", size = 2) +
  # scale_fill_distiller(palette = "Blues", trans = "reverse")
  scale_fill_gradient2(name = "North Pacific\nGyre Oscillation\n(NPGO; SD)", 
                       low="navy", mid="white", high="red", 
                       midpoint=0, limits=range(envProd$zASON_max))
resid.time.npgo
ggsave("./figs/Productivity residual timeseries_npgo.png", width = 8, height = 6)

# # Finally, show max weekly temp during spawning as dot fill color and maxP_spawn as dot size
# resid.time.maxT_spawn_maxP_spawn <- resid.time.2003.2009 +
#   geom_point(aes(fill = zmaxWkJA, size = zASON_max), shape = 21, color = "black") +
#   scale_fill_gradient2(name = "Maximum\nweekly\ntemperature\nduring\nspawning\n(maxT_spawn;\nSD)", 
#                        low="navy", mid="white", high="red", 
#                        midpoint=0, limits=range(envProd$zmaxWkJA)) +
#   scale_size_continuous(name = "Maximum\nprecipitation\nduring\nspawning &\nincubation\n(maxP_spawn;\nSD)",
#                         range = c(0.5, 4))
# resid.time.maxT_spawn_maxP_spawn
# ggsave("./figs/Productivity residual timeseries_maxT_spawn_maxP_spawn.png", width = 8, height = 6)
# This is way too complicated to interpret

# # Plot maxP_spawn + maxT_spawn on color axis (SD units)
# resid.time.maxPT_spawn <- resid.time.2003.2009 +
#   geom_point(aes(fill = maxPT_spawn), shape = 21, color = "black", size = 2) +
#   # scale_fill_gradient(low = "blue", high = "red")
#   # scale_fill_gradientn(colors = rainbow(5), trans = "reverse")
#   scale_fill_gradient2(name = "maxP_spawn\n+\nmaxT_spawn\n(SD)", 
#                        low="navy", mid="white", high="red", 
#                        midpoint=0, limits=range(envProd$maxPT_spawn)) 
# resid.time.maxPT_spawn
# ggsave("./figs/Productivity residual timeseries_maxPT_spawn.png", width = 8, height = 6)
# # This isn't statistically meaningful, but it's interesting to see how the combination of high precip and/or
# # high temp together explain more of the low-productivity years

# 2) Plot Ricker residuals vs environmental covariates-----------

resid.maxP_spawn <- ggplot(data = envProd, aes(x = zASON_max, y = resid)) +
  geom_point(shape = 1) + 
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(name = "Max. monthly precip. during spawning\n& incubation (maxP_spawn; SD)") +
  scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
resid.maxP_spawn
ggsave("./figs/Productivity_residuals_maxP_spawn.png", width = 4, height = 4)

# Also made residuals plots faceted by population, but these were generally less informative due to small sample
# sizes within each population
# resid.maxP_spawn.popn <- ggplot(data = envProd, aes(x = zASON_max, y = resid)) +
#   geom_point(shape = 1) + 
#   geom_smooth(method = "lm") +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_wrap(.~Population, ncol = 3, scales = "free_y") +
#   scale_x_continuous(name = "Maximum monthly precipitation during spawning & incubation (maxP_spawn; SD)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.maxP_spawn.popn
# ggsave("./figs/Productivity_residuals_maxP_spawn_by_popn.png", width = 8, height = 8)

# # Plot maxP_spawn on x and maxT_spawn on color axis
# resid.maxP_spawn.maxT_spawn <- resid.maxP_spawn +
#   geom_point(aes(fill = zmaxWkJA), shape = 21, color = "black", size = 2) +
#   scale_fill_gradient2(name = "Maximum\nweekly\ntemperature\nduring\nspawning\n(maxT_spawn;\nSD)", 
#                        low="navy", mid="white", high="red", 
#                        midpoint=0, limits=range(envProd$zmaxWkJA)) 
# resid.maxP_spawn.maxT_spawn
# ggsave("./figs/Productivity_residuals_maxP_spawn_maxT_spawn.png", width = 10, height = 8)

# resid.maxP_spawn.maxT_spawn.popn <- resid.maxP_spawn.popn +
#   geom_point(aes(fill = zmaxWkJA), shape = 21, color = "black", size = 2) +
#   scale_fill_gradient2(name = "Maximum\nweekly\ntemperature\nduring\nspawning\n(maxT_spawn;\nSD)", 
#                        low="navy", mid="white", high="red", 
#                        midpoint=0, limits=range(envProd$zmaxWkJA)) 
# resid.maxP_spawn.maxT_spawn.popn
# ggsave("./figs/Productivity_residuals_maxP_spawn_maxT_spawn_by_popn.png", width = 10, height = 8)

resid.avgP_grow <- ggplot(data = envProd, aes(x = zMJJA_avg.lag1, y = resid)) +
  geom_point(shape = 1) + 
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(name = "Mean precip. during juvenile rearing\n(avgP_grow; SD)") +
  scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
resid.avgP_grow
ggsave("./figs/Productivity_residuals_avgP_grow.png", width = 4, height = 4)

# resid.avgP_grow.popn <- ggplot(data = envProd, aes(x = zMJJA_avg.lag1, y = resid)) +
#   geom_point(shape = 1) + 
#   geom_smooth(method = "lm") +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_wrap(.~Population, ncol = 3) +
#   scale_x_continuous(name = "Mean precipitation during juvenile rearing (avgP_grow; SD)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.avgP_grow.popn
# ggsave("./figs/Productivity_residuals_avgP_grow_by_popn.png", width = 8, height = 10)

resid.maxT_spawn <- ggplot(data = envProd, aes(x = maxWkJA, y = resid)) +
  geom_point(shape = 1) + 
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(name = "Max. weekly temp. during spawning\n(maxT_spawn; ˚C)",
                     breaks = seq(8, 22, by = 2), limits = c(8, 22)) +
  scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
resid.maxT_spawn
ggsave("./figs/Productivity_residuals_maxT_spawn.png", width = 4, height = 4)

# A similar plot, with temperature expressed in standard deviations rather than degrees.  Went with degrees for
# the paper for ease of interpretability
# resid.maxT_spawn_SD <- ggplot(data = envProd, aes(x = zmaxWkJA, y = resid)) +
#   geom_point(shape = 1) + 
#   geom_smooth() +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   scale_x_continuous(name = "Maximum weekly temperature during spawning\n(maxT_spawn; SD)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.maxT_spawn_SD
# ggsave("./figs/Productivity_residuals_maxT_spawn_SD.png", width = 6, height = 6)
# 
# resid.maxT_spawn.popn <- ggplot(data = envProd, aes(x = maxWkJA, y = resid)) +
#   geom_point(shape = 1) + 
#   geom_smooth(method = "lm") +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_wrap(.~Population, ncol = 3, scales = "free_y") +
#   scale_x_continuous(name = "Maximum weekly temperature during spawning (maxT_spawn; ˚C)",
#                      breaks = seq(8, 22, by = 2), limits = c(8, 22)) +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.maxT_spawn.popn
# ggsave("./figs/Productivity_residuals_maxT_spawn_by_popn.png", width = 8, height = 10)
# 
# Another version based on suggestions from Becky
# resid.maxT_spawn.popn.poly <- ggplot(data = envProd, aes(x = maxWkJA, y = resid)) +
#   geom_point(shape = 1) + 
#   geom_smooth(method = "lm", formula = y ~ poly(x,2)) +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_wrap(.~Population, ncol = 3, scales = "free_y") +
#   scale_x_continuous(name = "Maximum weekly temperature during spawning (maxT_spawn; ˚C)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.maxT_spawn.popn.poly
# ggsave("./figs/Productivity_residuals_maxT_spawn_by_popn_poly.png", width = 8, height = 10)
# 
# resid.NPGO <- ggplot(data = envProd, aes(x = zNPGO.lag2, y = resid)) +
#   geom_point(shape = 1) + 
#   geom_smooth() +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   scale_x_continuous(name = "North Pacific Gyre Oscillation (NPGO; SD)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.NPGO
# ggsave("./figs/Productivity_residuals_NPGO.png", width = 6, height = 6)
# 
# Other residuals plots generated for exploratory purposes but not included in paper:
# resid.NPGO.popn <- ggplot(data = envProd, aes(x = zNPGO, y = resid)) +
#   geom_point(shape = 1) + 
#   geom_smooth(method = "lm") +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_wrap(.~Population, ncol = 3, scales = "free_y") +
#   scale_x_continuous(name = "North Pacific Gyre Oscillation (NPGO; SD)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.NPGO.popn
# ggsave("./figs/Productivity_residuals_NPGO_by_popn.png", width = 8, height = 8)
# 
# resid.avgT_grow <- ggplot(data = envProd, aes(x = meanWkJJA.lag1, y = resid)) +
#   geom_point(shape = 1) + 
#   geom_smooth() +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   scale_x_continuous(name = "Mean weekly temperature during juvenile rearing\n(avgT_grow, ˚C)",
#                      breaks = seq(8, 18, by = 2)) +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.avgT_grow
# ggsave("./figs/Productivity_residuals_avgT_grow.png", width = 6, height = 6)
# 
# resid.avgT_grow.popn <- ggplot(data = envProd, aes(x = meanWkJJA.lag1, y = resid)) +
#   geom_point(shape = 1) + 
#   geom_smooth(method = "lm") +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_wrap(.~Population, ncol = 3) +
#   scale_x_continuous(name = "Mean weekly temp. during juvenile rearing (˚C)",
#                      breaks = seq(8, 18, by = 2)) +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.avgT_grow.popn
# ggsave("./figs/Productivity_residuals_avgT_grow_by_popn.png", width = 8, height = 10)

# # Plot residuals vs. combined maxPT_spawn (SD)
# resid.maxPT_spawn <- ggplot(data = envProd, aes(x = maxPT_spawn, y = resid)) +
#   geom_point(shape = 1) + 
#   geom_smooth() +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   scale_x_continuous(name = "maxP_spawn + maxT_spawn (SD)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.maxPT_spawn
# ggsave("./figs/Productivity_residuals_maxPT_spawn.png", width = 6, height = 6)
# 
# resid.maxPT_spawn.popn <- ggplot(data = envProd, aes(x = maxPT_spawn, y = resid)) +
#   geom_point(shape = 1) + 
#   geom_smooth(method = "lm") +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_wrap(.~Population, ncol = 3, scales = "free_y") +
#   scale_x_continuous(name = "maxP_spawn + maxT_spawn (SD)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.maxPT_spawn.popn
# ggsave("./figs/Productivity_residuals_maxPT_spawn_by_popn.png", width = 8, height = 8)

# Combine the residuals plots for the 3 stronest effects (maxP_spawn, maxT_spawn, avgP_grow) into a 3-panel fig
resid.maxT_spawn.g <- resid.maxT_spawn +
  scale_y_continuous(name = NULL, labels = NULL)
resid.avgP_grow.g <- resid.avgP_grow +
  scale_y_continuous(name = NULL, labels = NULL)
resid.3panel <- plot_grid(resid.maxP_spawn, resid.maxT_spawn.g, resid.avgP_grow.g, 
                          labels = c("     A", "B", "C"), ncol = 3, align = "h", label_x = 0.08)
resid.3panel
ggsave("./figs/Productivity_residuals_3panel.png", width = 12, height = 6)

# TODO: A single faceted plot showing residuals for all covariates
# rename covariates and reshape envProd into long form
# (Can I save some work here by calling Curry's script that does all this?)
envProd.long <- envProd %>%
  select(Population, BroodYear, Spawners, ln.rpsCore, prodCore, 
         maxP_spawn = zASON_max,
         avgP_grow = zMJJA_avg.lag1)
  gather(maxWkJA:zNPGO.lag2, key = "Covariate", value = "Value")
resid.all <- ggplot(data = envProd, aes(x = zASON_max, y = resid)) +
  geom_point(shape = 1) + 
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(name = "Max. monthly precip. during spawning\n& incubation (maxP_spawn; SD)") +
  scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
resid.maxP_spawn
ggsave("./figs/Productivity_residuals_maxP_spawn.png", width = 4, height = 4)

# 3) Plot productivity ln(R/S) vs the environmental covariates---------
# NOTE: These plots don't take density dependence into account (but they also don't assume
# a Ricker model correctly describes the effect of density dependence)

prodCore.maxP_spawn <- ggplot(data = envProd, aes(x = zASON_max, y = prodCore)) +
  geom_point(shape = 1) +
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(name = "Max. precip. during spawning & incubation (maxP_spawn; SD)") +
  scale_y_continuous(name = "Productivity index (SD)", limits=range(envProd$prodCore))
prodCore.maxP_spawn
ggsave("./figs/Productivity_maxP_spawn.png", width = 10, height = 6)

prodCore.maxP_spawn.popn <- ggplot(data = envProd, aes(x = zASON_max, y = prodCore)) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(.~Population, ncol = 3) +
  scale_x_continuous(name = "Maximum monthly precipitation during spawning & incubation (SD)") +
  scale_y_continuous(name = "Productivity index (SD)", limits=range(envProd$prodCore))
prodCore.maxP_spawn.popn
ggsave("./figs/Productivity_maxP_spawn_by_popn.png", width = 8, height = 10)

# Plot maxP_spawn on x and maxT_spawn on color axis
prodCore.maxP_spawn.maxT_spawn <- prodCore.maxP_spawn +
  geom_point(aes(fill = zmaxWkJA), shape = 21, color = "black", size = 2) +
  scale_fill_gradient2(name = "Max. weekly\ntemperature\nduring\nspawning (SD)", 
                       low="navy", mid="white", high="red", 
                       midpoint=0, limits=range(envProd$zmaxWkJA)) 
prodCore.maxP_spawn.maxT_spawn
ggsave("./figs/Productivity_maxP_spawn_maxT_spawn.png", width = 10, height = 6)

prodCore.maxP_spawn.maxT_spawn.popn <- prodCore.maxP_spawn.popn +
  geom_point(aes(fill = zmaxWkJA), shape = 21, color = "black", size = 2) +
  scale_fill_gradient2(name = "Max. weekly\ntemperature\nduring\nspawning (SD)", 
                       low="navy", mid="white", high="red", 
                       midpoint=0, limits=range(envProd$zmaxWkJA)) 
prodCore.maxP_spawn.maxT_spawn.popn
ggsave("./figs/Productivity_maxP_spawn_maxT_spawn_by_popn.png", width = 8, height = 10)

prodCore.avgP_grow <- ggplot(data = envProd, aes(x = zMJJA_avg.lag1, y = prodCore)) +
  geom_point(shape = 1) +
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(name = "Mean precipitation during juvenile rearing (SD)") +
  scale_y_continuous(name = "Productivity index (SD)", limits=range(envProd$prodCore))
prodCore.avgP_grow
ggsave("./figs/Productivity_avgP_grow.png", width = 10, height = 6)

prodCore.avgP_grow.popn <- ggplot(data = envProd, aes(x = zMJJA_avg.lag1, y = prodCore)) +
  geom_point(shape = 1) +
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(.~Population, ncol = 3) +
  scale_x_continuous(name = "Mean precipitation during juvenile rearing (SD)") +
  scale_y_continuous(name = "Productivity index (SD)", limits=range(envProd$prodCore))
prodCore.avgP_grow.popn
ggsave("./figs/Productivity_avgP_grow_by_popn.png", width = 8, height = 10)

prodCore.maxT_spawn <- ggplot(data = envProd, aes(x = maxWkJA, y = prodCore)) +
  geom_point(shape = 1) +
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(name = "Max. weekly temp. during spawning (˚C)") +
  scale_y_continuous(name = "Productivity index (SD)", limits=range(envProd$prodCore))
prodCore.maxT_spawn
ggsave("./figs/Productivity_maxT_spawn.png", width = 10, height = 6)

prodCore.maxT_spawn.popn <- ggplot(data = envProd, aes(x = maxWkJA, y = prodCore)) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(.~Population, ncol = 3) +
  scale_x_continuous(name = "Max. weekly temp. during spawning (˚C)") +
  scale_y_continuous(name = "Productivity index (SD)", limits=range(envProd$prodCore))
prodCore.maxT_spawn.popn
ggsave("./figs/Productivity_maxT_spawn_by_popn.png", width = 8, height = 10)

prodCore.avgT_grow <- ggplot(data = envProd, aes(x = meanWkJJA.lag1, y = prodCore)) +
  geom_point(shape = 1) +
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(name = "Mean weekly temp. during juvenile rearing (˚C)") +
  scale_y_continuous(name = "Productivity index (SD)", limits=range(envProd$prodCore))
prodCore.avgT_grow
ggsave("./figs/Productivity_avgT_grow.png", width = 10, height = 6)

prodCore.avgT_grow.popn <- ggplot(data = envProd, aes(x = meanWkJJA.lag1, y = prodCore)) +
  geom_point(shape = 1) +
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(.~Population, ncol = 3) +
  scale_x_continuous(name = "Mean weekly temp. during juvenile rearing (˚C)") +
  scale_y_continuous(name = "Productivity index (SD)", limits=range(envProd$prodCore))
prodCore.avgT_grow.popn
ggsave("./figs/Productivity_avgT_grow_by_popn.png", width = 8, height = 10)


# 4) Plot Ricker residuals vs environmental covariates WITHOUT ALEXANDER CREEK-----------
# Goal here is to determine whether Alexander Creek is driving the overall patterns above.
# We have reason to believe the Alexander Creek decline was strongly influenced by pike 
# predation (perhaps in addition to the environmental covariates examined here).  So if our
# main results go away when we exclude Alexander, then we should take them with a grain of salt.
# (No population-specific plots included here, because those would be the same as above)

envProd_noAlex <- filter(envProd, Population != "Alexander")

resid.maxP_spawn_noAlex <- ggplot(data = envProd_noAlex, aes(x = zASON_max, y = resid)) +
  geom_point(shape = 1) + 
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(name = "Max. precip. during spawning & incubation (maxP_spawn; SD)") +
  scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
resid.maxP_spawn_noAlex
ggsave("./figs/Productivity_residuals_maxP_spawn_noAlex.png", width = 10, height = 6)

# Plot maxP_spawn on x and maxT_spawn on color axis
resid.maxP_spawn.maxT_spawn_noAlex <- resid.maxP_spawn_noAlex +
  geom_point(aes(fill = zmaxWkJA), shape = 21, color = "black", size = 2) +
  scale_fill_gradient2(name = "Max. weekly\ntemperature\nduring\nspawning (SD)", 
                       low="navy", mid="white", high="red", 
                       midpoint=0, limits=range(envProd$zmaxWkJA)) 
resid.maxP_spawn.maxT_spawn_noAlex
ggsave("./figs/Productivity_residuals_maxP_spawn_maxT_spawn_noAlex.png", width = 10, height = 6)

resid.avgP_grow_noAlex <- ggplot(data = envProd_noAlex, aes(x = zMJJA_avg.lag1, y = resid)) +
  geom_point(shape = 1) + 
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(name = "Mean precipitation during juvenile rearing (SD)") +
  scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
resid.avgP_grow_noAlex
ggsave("./figs/Productivity_residuals_avgP_grow_noAlex.png", width = 10, height = 6)

resid.maxT_spawn_noAlex <- ggplot(data = envProd_noAlex, aes(x = maxWkJA, y = resid)) +
  geom_point(shape = 1) + 
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(name = "Max. weekly temp. during spawning (˚C)",
                     breaks = seq(8, 22, by = 2), limits = c(8, 22)) +
  scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
resid.maxT_spawn_noAlex
ggsave("./figs/Productivity_residuals_maxT_spawn_noAlex.png", width = 10, height = 6)

resid.avgT_grow_noAlex <- ggplot(data = envProd_noAlex, aes(x = meanWkJJA.lag1, y = resid)) +
  geom_point(shape = 1) + 
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(name = "Mean weekly temp. during juvenile rearing (˚C)",
                     breaks = seq(8, 18, by = 2)) +
  scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
resid.avgT_grow_noAlex
ggsave("./figs/Productivity_residuals_avgT_grow_noAlex.png", width = 10, height = 6)

# Plot correlations between spawning abundance and key predictors--------
# maxPspawn
maxPspawn.esc <- ggplot(data = envProd, aes (x = zSpawners, y = zASON_max)) +
  geom_point(shape = 1) +
  # geom_text(aes(label = BroodYear)) + 
  geom_smooth(method = "lm") +
  scale_x_continuous(name = "Escapement (SD)") +
  scale_y_continuous(name = "maxP_spawn (SD)")
maxPspawn.esc
ggsave("./figs/maxPspawn vs escape.png", width = 10, height = 6)

maxPspawn.esc.pop <- ggplot(data = envProd, aes (x = zSpawners, y = zASON_max)) +
  geom_point(shape = 1) +
  # geom_text(aes(label = BroodYear)) + 
  geom_smooth(method = "lm") +
  facet_wrap(.~Population, ncol = 3) +
  scale_x_continuous(name = "Escapement (SD)") +
  scale_y_continuous(name = "maxP_spawn (SD)")
maxPspawn.esc.pop
ggsave("./figs/maxPspawn vs escape_by popn.png", width = 10, height = 6)

# maxTspawn
maxTspawn.esc <- ggplot(data = envProd, aes (x = zSpawners, y = zmaxWkJA)) +
  geom_point(shape = 1) +
  # geom_text(aes(label = BroodYear)) + 
  geom_smooth(method = "lm") +
  scale_x_continuous(name = "Escapement (SD)") +
  scale_y_continuous(name = "maxT_spawn (SD)")
maxTspawn.esc
ggsave("./figs/maxTspawn vs escape.png", width = 10, height = 6)

maxTspawn.esc.pop <- ggplot(data = envProd, aes (x = zSpawners, y = zmaxWkJA)) +
  geom_point(shape = 1) +
  # geom_text(aes(label = BroodYear)) + 
  geom_smooth(method = "lm") +
  facet_wrap(.~Population, ncol = 3) +
  scale_x_continuous(name = "Escapement (SD)") +
  scale_y_continuous(name = "maxT_spawn (SD)")
maxTspawn.esc.pop
ggsave("./figs/maxTspawn vs escape_by popn.png", width = 10, height = 6)
