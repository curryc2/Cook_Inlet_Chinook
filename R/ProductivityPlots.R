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
library(Cairo)

setwd("~/Desktop/Cook Inlet Chinook/Analysis")
covarsSR <- read_csv("./data/covarsSR.csv")
theme_set(theme_bw(12))

# Munge data---------

# Calculate basic productivity indices: ln(Recruits/Spawner) and ln(Recruits/Spawner) core ages
# only. Then standardize the productivity indices
envProd <- covarsSR %>%
  group_by(Population) %>%
  mutate(ln.rpsCore = log(CoreRecruitsPerSpawner),
         prodCore = scale(ln.rpsCore)) 
  
# Double-check that productivity indices standardized properly (mean of each population should 
# be 0)
meanProd <- envProd %>%
  group_by(Population) %>%
  summarize(meanProdCore = mean(prodCore, na.rm = T))
# Yes, good to go

# In what year was the lowest productivity value for each population?
lowestProd <- envProd %>%
  group_by(Population) %>%
  summarize(minProdCore = min(prodCore, na.rm = T))

yearOfLowestProd <- lowestProd %>%
  left_join(envProd, by = c("Population", "minProdCore" = "prodCore"))

# Plot NPGO trend over time for brief description in the results
npgo.trend <- ggplot(data = envProd, aes(x = BroodYear, y = NPGO.std)) + 
  geom_col() +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Brood year") +
  scale_y_continuous(name = "NPGO (std) during\nyear of ocean entry")
npgo.trend

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
# ggsave("./figs/Ricker residuals.png", width = 12, height = 10)

# Fit linearized simple Ricker models (no environmental covariates) for each population
envProd.no.nas <- drop_na(envProd, prodCore) 
rickers <- lm(ln.rpsCore ~ Population + Population:Spawners, data = envProd.no.nas)
envProd.no.nas$ln.rpsCore.pred <- predict(rickers)
envProd.no.nas$resid <- envProd.no.nas$ln.rpsCore - envProd.no.nas$ln.rpsCore.pred

# Fill missing years with NAs to avoid interpolating across years with missing 
# data in the timeseries plots
envProd <- envProd.no.nas %>%
  ungroup() %>%
  complete(Population, BroodYear)
                     
# Plot the data----------
# 1) Productivity (ln R/S) time-series plots-----------
# First, make a plain plot of productivity over time (faceted by population)
prodCore.time <- ggplot(data = envProd, aes(x = BroodYear, y = prodCore)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, lty = "dotted") +
  facet_wrap(.~Population, ncol = 3) +
  # facet_wrap(.~Population, ncol = 3, scales = "free_y") + # facets with free y-axis scales
  scale_x_continuous(name = "Brood year", breaks = seq(1980, 2010, by = 10)) +
  expand_limits(x = c(1979,2011)) +
  scale_y_continuous(name = "Productivity index (ln[R/S])")
prodCore.time
# ggsave("./figs/Productivity timeseries.png", width = 6, height = 6)

# Same plot with 2003-2007 highlighted
prodCore.time.2003.2007 <- prodCore.time +
  geom_rect(xmin = 2003, xmax = 2007, ymin = -3, ymax = 3, fill = "grey75", alpha = 0.2)+
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, lty = "dotted") 
prodCore.time.2003.2007
ggsave("./figs/Fig 2_Productivity timeseries_2003-2007.png", width = 6, height = 6)
ggsave("./figs/Fig 2_Productivity timeseries_2003-2007.pdf", width = 6, height = 6)


# # 2) Plot Ricker residuals vs environmental covariates-----------
# 
# resid.maxP_spawn <- ggplot(data = envProd, aes(x = maxP_spawn.std, y = resid)) +
#   geom_point(shape = 1) + 
#   geom_smooth() +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   scale_x_continuous(name = "Max. monthly precip. during spawning\n& incubation (maxP_spawn; SD)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.maxP_spawn
# ggsave("./figs/Productivity_residuals_maxP_spawn.png", width = 4, height = 4)
# 
# # Also made residuals plots faceted by population, but these were generally less informative due to small sample
# # sizes within each population
# resid.maxP_spawn.popn <- ggplot(data = envProd, aes(x = maxP_spawn.std, y = resid)) +
#   geom_point(shape = 1) +
#   geom_smooth(method = "lm") +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_wrap(.~Population, ncol = 3, scales = "free_y") +
#   scale_x_continuous(name = "Maximum monthly precipitation during spawning & incubation (maxP_spawn; SD)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.maxP_spawn.popn
# ggsave("./figs/Productivity_residuals_maxP_spawn_by_popn.png", width = 8, height = 8)
# 
# # # Plot maxP_spawn on x and maxT_spawn on color axis
# # resid.maxP_spawn.maxT_spawn <- resid.maxP_spawn +
# #   geom_point(aes(fill = maxT_spawn.std.all), shape = 21, color = "black", size = 2) +
# #   scale_fill_gradient2(name = "Maximum\nweekly\ntemperature\nduring\nspawning\n(maxT_spawn;\nSD)", 
# #                        low="navy", mid="white", high="red", 
# #                        midpoint=0, limits=range(envProd$maxT_spawn.std.all)) 
# # resid.maxP_spawn.maxT_spawn
# # ggsave("./figs/Productivity_residuals_maxP_spawn_maxT_spawn.png", width = 10, height = 8)
# 
# # resid.maxP_spawn.maxT_spawn.popn <- resid.maxP_spawn.popn +
# #   geom_point(aes(fill = maxT_spawn.std.all), shape = 21, color = "black", size = 2) +
# #   scale_fill_gradient2(name = "Maximum\nweekly\ntemperature\nduring\nspawning\n(maxT_spawn;\nSD)", 
# #                        low="navy", mid="white", high="red", 
# #                        midpoint=0, limits=range(envProd$maxT_spawn.std.all)) 
# # resid.maxP_spawn.maxT_spawn.popn
# # ggsave("./figs/Productivity_residuals_maxP_spawn_maxT_spawn_by_popn.png", width = 10, height = 8)
# 
# resid.avgP_rear <- ggplot(data = envProd, aes(x = avgP_rear.std, y = resid)) +
#   geom_point(shape = 1) + 
#   geom_smooth() +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   scale_x_continuous(name = "Mean precip. during juvenile rearing\n(avgP_rear; SD)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.avgP_rear
# ggsave("./figs/Productivity_residuals_avgP_rear.png", width = 4, height = 4)
# 
# resid.avgP_rear.popn <- ggplot(data = envProd, aes(x = avgP_rear.std, y = resid)) +
#   geom_point(shape = 1) +
#   geom_smooth(method = "lm") +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_wrap(.~Population, ncol = 3) +
#   scale_x_continuous(name = "Mean precipitation during juvenile rearing (avgP_rear; SD)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.avgP_rear.popn
# ggsave("./figs/Productivity_residuals_avgP_rear_by_popn.png", width = 8, height = 10)
# 
# TODO: convert x-axis label to expression() format so degree symbol will print in pdf
resid.maxT_spawn <- ggplot(data = envProd, aes(x = maxT_spawn, y = resid)) +
  geom_point(shape = 1) +
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  # scale_x_continuous(name = "Max. weekly temp. during spawning\n(maxT_spawn; ˚C)",
  #                    breaks = seq(8, 22, by = 2), limits = c(8, 22)) +
  # scale_x_continuous(name = expression("Max. weekly temp. during spawning (maxT_spawn " ( degree*C)),
  #                    breaks = seq(8, 22, by = 2), limits = c(8, 22)) +
  scale_x_continuous(name = expression("maxT_spawn ("*degree*C*")"),
                     breaks = seq(8, 22, by = 2), limits = c(8, 22)) +
  scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
resid.maxT_spawn
# ggsave("./figs/Productivity_residuals_maxT_spawn.png", width = 4, height = 4)
# 
# # Same plot with the coldest (Chulitna) and warmest (Deshka) streams highlighted in color,
# # with their own linear regression fits
# resid.maxT_spawn_ChuliVDeshka <- resid.maxT_spawn +
#   geom_smooth(color = "black") +
#   geom_point(data = filter(envProd, Population == "Deshka"), shape = 1, color = "red") +
#   geom_smooth(data = filter(envProd, Population == "Deshka"), method = "lm", se = F,
#               color = "red", lty = "dashed") +
#   geom_point(data = filter(envProd, Population == "Chulitna"), shape = 1, color = "blue") +
#   geom_smooth(data = filter(envProd, Population == "Chulitna"), method = "lm", se = F,
#               color = "blue", lty = "dashed")
# resid.maxT_spawn_ChuliVDeshka
# ggsave("./figs/Productivity_residuals_maxT_spawn_Chuli vs Deshka.png", width = 4, height = 4)

# Same plot with the 2 coldest and 2 warmest streams highlighted in color,
# with their own linear regression fits
resid.maxT_spawn_ColdVWarm <- resid.maxT_spawn +
  geom_smooth(color = "black") +
  geom_point(data = filter(envProd, Population == "Deshka"), shape = 1, color = "red") +
  geom_smooth(data = filter(envProd, Population == "Deshka"), method = "lm", se = F,
              color = "red", lty = "dashed") +
  geom_point(data = filter(envProd, Population == "Alexander"), shape = 1, color = "orange") +
  geom_smooth(data = filter(envProd, Population == "Alexander"), method = "lm", se = F,
              color = "orange", lty = "dashed") +
  geom_point(data = filter(envProd, Population == "Chulitna"), shape = 1, color = "blue") +
  geom_smooth(data = filter(envProd, Population == "Chulitna"), method = "lm", se = F,
              color = "blue", lty = "dashed") +
  geom_point(data = filter(envProd, Population == "Little Susitna"), shape = 1, color = "cyan") +
  geom_smooth(data = filter(envProd, Population == "Little Susitna"), method = "lm", se = F,
              color = "cyan", lty = "dashed")
resid.maxT_spawn_ColdVWarm
# ggsave("./figs/Productivity_residuals_maxT_spawn_ColdVWarm.png", width = 4, height = 4)
# ggsave("./figs/Productivity_residuals_maxT_spawn_ColdVWarm.pdf", width = 4, height = 4)
# 
# # # A similar plot, with temperature expressed in standard deviations rather than degrees.  Went with degrees for
# # # the paper for ease of interpretability
# resid.maxT_spawn_SD <- ggplot(data = envProd, aes(x = maxT_spawn.std, y = resid)) +
#   geom_point(shape = 1) +
#   geom_smooth() +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   scale_x_continuous(name = "Maximum weekly temperature during spawning\n(maxT_spawn; SD)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.maxT_spawn_SD
# ggsave("./figs/Productivity_residuals_maxT_spawn_SD.png", width = 6, height = 6)
# 
# resid.maxT_spawn.popn <- ggplot(data = envProd, aes(x = maxT_spawn, y = resid)) +
#   geom_point(shape = 1) +
#   geom_smooth(method = "lm") +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_wrap(.~Population, ncol = 3, scales = "free_y") +
#   scale_x_continuous(name = "Maximum weekly temperature during spawning (maxT_spawn; ˚C)",
#                      breaks = seq(8, 22, by = 2), limits = c(8, 22)) +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.maxT_spawn.popn
# ggsave("./figs/Productivity_residuals_maxT_spawn_by_popn.png", width = 6, height = 8)
# 
# # Another version based on suggestions from Becky
# resid.maxT_spawn.popn.poly <- ggplot(data = envProd, aes(x = maxT_spawn, y = resid)) +
#   geom_point(shape = 1) +
#   geom_smooth(method = "lm", formula = y ~ poly(x,2)) +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_wrap(.~Population, ncol = 3, scales = "free_y") +
#   scale_x_continuous(name = "Maximum weekly temperature during spawning (maxT_spawn; ˚C)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.maxT_spawn.popn.poly
# ggsave("./figs/Productivity_residuals_maxT_spawn_by_popn_poly.png", width = 8, height = 10)
# 
# resid.NPGO <- ggplot(data = envProd, aes(x = NPGO.std, y = resid)) +
#   geom_point(shape = 1) +
#   geom_smooth() +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   scale_x_continuous(name = "North Pacific Gyre Oscillation (NPGO; SD)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.NPGO
# ggsave("./figs/Productivity_residuals_NPGO.png", width = 6, height = 6)
# 
# resid.medianQ_rear <- ggplot(data = envProd, aes(x = medianQ_rear.std, y = resid)) +
#   geom_point(shape = 1) +
#   geom_smooth() +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   scale_x_continuous(name = "Median river discharge (SD)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.medianQ_rear
# ggsave("./figs/Productivity_residuals_medianQ_rear.png", width = 6, height = 6)
# # 
# # Other residuals plots generated for exploratory purposes but not included in paper:
# resid.NPGO.popn <- ggplot(data = envProd, aes(x = NPGO.std, y = resid)) +
#   geom_point(shape = 1) +
#   geom_smooth(method = "lm") +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_wrap(.~Population, ncol = 3, scales = "free_y") +
#   scale_x_continuous(name = "North Pacific Gyre Oscillation (NPGO; SD)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.NPGO.popn
# ggsave("./figs/Productivity_residuals_NPGO_by_popn.png", width = 8, height = 8)
# # 
# 
# resid.wksGT15_rear <- ggplot(data = envProd, aes(x = wksGT15_rear, y = resid)) +
#   geom_point(shape = 1) +
#   geom_smooth() +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   scale_x_continuous(name = "Weeks exceeding 15˚C during juvenile rearing\n(wksGT15_rear)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.wksGT15_rear
# ggsave("./figs/Productivity_residuals_wksGT15_rear.png", width = 6, height = 6)
# 
# TODO: convert x-axis label to expression() format so degree symbol will print in pdf
resid.avgT_rear <- ggplot(data = envProd, aes(x = avgT_rear, y = resid)) +
  geom_point(shape = 1) +
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  # scale_x_continuous(name = "Mean weekly temp. during juvenile rearing\n(avgT_rear, ˚C)",
  #                    breaks = seq(8, 18, by = 2)) +
  scale_x_continuous(name = expression("avgT_rear ("*degree*C*")"),
                     breaks = seq(8, 18, by = 2)) +
  scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
resid.avgT_rear
# ggsave("./figs/Productivity_residuals_avgT_rear.png", width = 6, height = 6)
# 
# # Same plot with the coldest (Chulitna) and warmest (Deshka) streams highlighted in color,
# # with their own linear regression fits
# resid.avgT_rear_ChuliVDeshka <- resid.avgT_rear +
#   geom_smooth(color = "black") +
#   geom_point(data = filter(envProd, Population == "Deshka"), shape = 1, color = "red") +
#   geom_smooth(data = filter(envProd, Population == "Deshka"), method = "lm", se = F,
#               color = "red", lty = "dashed") +
#   geom_point(data = filter(envProd, Population == "Chulitna"), shape = 1, color = "blue") +
#   geom_smooth(data = filter(envProd, Population == "Chulitna"), method = "lm", se = F,
#               color = "blue", lty = "dashed")
# resid.avgT_rear_ChuliVDeshka
# ggsave("./figs/Productivity_residuals_avgT_rear_Chuli vs Deshka.png", width = 4, height = 4)
# 
# # Same plot with the 2 coldest and 2 warmest streams highlighted in color,
# # with their own linear regression fits
resid.avgT_rear_ColdVWarm <- resid.avgT_rear +
  geom_smooth(color = "black") +
  geom_point(data = filter(envProd, Population == "Deshka"), shape = 1, color = "red") +
  geom_smooth(data = filter(envProd, Population == "Deshka"), method = "lm", se = F,
              color = "red", lty = "dashed") +
  geom_point(data = filter(envProd, Population == "Alexander"), shape = 1, color = "orange") +
  geom_smooth(data = filter(envProd, Population == "Alexander"), method = "lm", se = F,
              color = "orange", lty = "dashed") +
  geom_point(data = filter(envProd, Population == "Chulitna"), shape = 1, color = "blue") +
  geom_smooth(data = filter(envProd, Population == "Chulitna"), method = "lm", se = F,
              color = "blue", lty = "dashed") +
  geom_point(data = filter(envProd, Population == "Little Susitna"), shape = 1, color = "cyan") +
  geom_smooth(data = filter(envProd, Population == "Little Susitna"), method = "lm", se = F,
              color = "cyan", lty = "dashed")
resid.avgT_rear_ColdVWarm
# ggsave("./figs/Productivity_residuals_avgT_rear_ColdVWarm.png", width = 4, height = 4)
# 
# 
# resid.avgT_rear.popn <- ggplot(data = envProd, aes(x = avgT_rear, y = resid)) +
#   geom_point(shape = 1) +
#   geom_smooth(method = "lm") +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_wrap(.~Population, ncol = 3) +
#   scale_x_continuous(name = "Mean weekly temp. during juvenile rearing (˚C)",
#                      breaks = seq(8, 18, by = 2)) +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.avgT_rear.popn
# ggsave("./figs/Productivity_residuals_avgT_rear_by_popn.png", width = 8, height = 10)
# 

# A 2-panel plot showing the residuals of maxT_spawn and avgT_rear with 2 coldest and 2 warmest
# streams highlighted

# # First, make new subplot without y-axis labels
resid.avgT_rear_ColdVWarm.g <- resid.avgT_rear_ColdVWarm +
  scale_y_continuous(name = NULL, labels = NULL)

resid.temp.2panel <- plot_grid(resid.maxT_spawn_ColdVWarm, resid.avgT_rear_ColdVWarm.g,
                               labels = c("    (a)", "(b)"),
                               ncol = 2, align = "h", label_x = 0.05, label_y = 0.97)
resid.temp.2panel
ggsave("./figs/Fig 5_Productivity_residuals_temp_2panel_ColdVWarm.png", width = 8, height = 6)
ggsave("./figs/Fig 5_Productivity_residuals_temp_2panel_ColdVWarm.pdf", width = 8, height = 6)


# Make faceted plots showing productivity residuals vs multiple covariates of interest----------

# # rename covariates and reshape envProd into long form
envProd.long <- envProd %>%
  select(Population, BroodYear, Spawners, Spawners.std, ln.rpsCore, prodCore, resid,
         # Include all unstandardized covariates (except precip covariates, which must be standardized
         # to be meaningful because of differences in watershed area and topography)
         maxT_spawn, avgT_rear, RB_spawn, RB_emerge, medianQ_rear, breakup, NPGO,
         # Also include all standardized covariates
         maxT_spawn.std, avgT_rear.std, 
         maxP_spawn.std, avgP_rear.std,
         RB_spawn.std, RB_emerge.std, medianQ_rear.std,
         breakup.std, NPGO.std) %>%
  gather(maxT_spawn:NPGO.std, key = "Covariate", value = "Value") %>%
  # drop nas
  drop_na(resid) 

# 
# # Plot residuals against all covariates
# resid.all <- ggplot(data = envProd.long, aes(x = Value, y = resid)) +
#   geom_point(shape = 1) + 
#   geom_smooth() +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_wrap(.~Covariate, ncol = 3, scales = "free") +
#   scale_x_continuous(name = "Covariate value (SD)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.all
# ggsave("./figs/Productivity_residuals_all_covariates.png", width = 6, height = 8)
# 

# Plot top 4 covariates
envProd.long.top4 <- envProd.long %>%
  filter(Covariate == "maxP_spawn.std" | Covariate == "avgP_rear.std" | Covariate == "NPGO" | 
           Covariate == "medianQ_rear") %>%
  # Order and label the covariates for plotting
  mutate(Covariate = factor(Covariate,
                            levels = c("maxP_spawn.std", "avgP_rear.std", "medianQ_rear", "NPGO"),
                            labels = c("maxP_spawn", "avgP_rear", "medianQ_rear","NPGO")))

resid.top4 <- ggplot(data = envProd.long.top4, aes(x = Value, y = resid)) +
  geom_point(shape = 1) + 
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(.~Covariate, ncol = 2, scales = "free_x") +
  # facet_wrap(.~Covariate, ncol = 2, scales = "free_x") +
  scale_x_continuous(name = "Covariate value") +
  scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
resid.top4
ggsave("./figs/Fig S6_Productivity_residuals_top4_covariates.png", width = 6, height = 8)
ggsave("./figs/Fig S6_Productivity_residuals_top4_covariates.pdf", width = 6, height = 8)

# # A single faceted plot showing residuals for all STANDARDIZED covariates (original plot from
# submitted manuscript)
# # rename covariates and reshape envProd into long form
envProd.std.long <- envProd %>%
  select(Population, BroodYear, Spawners, Spawners.std, ln.rpsCore, prodCore, resid,
         maxT_spawn.std, avgT_rear.std, maxP_spawn.std, avgP_rear.std,
         RB_spawn.std, RB_emerge.std, medianQ_rear.std,
         breakup.std, NPGO.std) %>%
  gather(maxT_spawn.std:NPGO.std, key = "Covariate", value = "Value") %>%
  # drop nas
  drop_na(resid) %>%
  # Order and label the covariates for plotting
  mutate(Covariate = factor(Covariate,
                            levels = c("maxP_spawn.std", "avgP_rear.std", "NPGO.std",
                                       "medianQ_rear.std", "maxT_spawn.std", 
                                       "RB_emerge.std", "RB_spawn.std",
                                       "breakup.std", "avgT_rear.std"),
                            labels = c("maxP_spawn", "avgP_rear", "NPGO",
                                       "medianQ_rear", "maxT_spawn", 
                                       "RB_emerge.std", "RB_spawn.std",
                                       "breakup.std", "avgT_rear.std")))
# 
# # Plot residuals against all covariates
# resid.all.std <- ggplot(data = envProd.std.long, aes(x = Value, y = resid)) +
#   geom_point(shape = 1) + 
#   geom_smooth() +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_wrap(.~Covariate, ncol = 3, scales = "free") +
#   scale_x_continuous(name = "Covariate value (SD)") +
#   scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
# resid.all.std
# ggsave("./figs/Productivity_residuals_all_covariates_std.png", width = 6, height = 8)
# 

# Plot top 4 covariates

resid.top4.std <- ggplot(data = envProd.long.top4, aes(x = Value, y = resid)) +
  geom_point(shape = 1) + 
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(.~Covariate, ncol = 2, scales = "free_x") +
  scale_x_continuous(name = "Standardized covariate value (SD)") +
  scale_y_continuous(name = "Productivity (Ricker residuals)", limits=range(envProd$resid))
resid.top4.std
# ggsave("./figs/Productivity_residuals_top4_covariates_std.png", width = 6, height = 8)
# ggsave("./figs/Productivity_residuals_top4_covariates_std.pdf", width = 6, height = 8)

