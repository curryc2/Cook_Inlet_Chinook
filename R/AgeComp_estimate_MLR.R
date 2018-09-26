# AgeComp_estimate.R
# Erik Schoen
# eschoen@alaska.edu
# 2-2018

# Cook Inlet Chinook project
# Estimate age comps for stocks / brood years with missing data

# Load packages and read in data-------------
library(MASS)
library(dplyr)
library(tidyr)
library(readr)
library(nnet)
library(MuMIn)
library(GGally)
library(broom)
library(stringr)

theme_set(theme_bw(12) + theme(panel.grid.minor = element_blank())) 

setwd("~/Desktop/Cook Inlet Chinook/Analysis/data")

# Read in data
sites <- read_csv("Sites.csv")
# Age composition data compiled by AgeComp_compile.R
ageFull <- read_csv("AgeFull.csv")
# Escapement
escape <- read_csv("Escapement.csv")

# Generate a simplified dataframe of the best quality age data for each 
# Group X Run Component X Return Year
# (filtering out extraneous and redundant data, e.g., multiple gear types)
ageSimple <- ageFull %>%
  left_join(sites, by = "Group") %>%
  # Make a new variable including Gear and mesh size (for gill nets)
  mutate(GearMesh = ifelse(is.na(Mesh), Gear, paste(Mesh, Gear))) %>%
  # Filter out the hatchery fish
  filter(HatcheryWild != "Hatchery") %>%
  # Filter out data from outside the study years
  filter(ReturnYear > 1979 & ReturnYear < 2016) %>%
  # Filter out redundant data for population-years with multiple data sources
  filter(Gear != "Model Estimate") %>%
  filter(Component != "Commercial + Sport Harvest") %>%
  filter(!(GearMesh == "7.5 in Gill Net" & ReturnYear > 2001)) %>%
  filter(!(Group == "Kenai.Late" & GearMesh == "5 in Gill Net")) %>%
  filter(!(GearMesh == "5 and 7.5 in Gill Net" & ReturnYear < 2002)) 

# Correct for the change in gill net mesh size used in the Kenai in-river ASL sampling.  7.5" mesh
# was used from 1986-2001, then 5" and 7.5" mesh (combined) were used for the following years.
# Correct the earlier data to make it comparable with the later data.

selectivity7.5 <- ageFull %>%
  filter(Group == "Kenai.Late") %>%
  filter(Mesh == "7.5 in" | Mesh == "5 and 7.5 in") %>%
  # exclude the 2002 data: it seems bogus because the exact same age proportions were reported for both
  # mesh sizes
  filter(ReturnYear > 2002) %>% 
  select(-n) %>%
  spread(key = Mesh, value = Prop) %>%
  mutate(selectivity7.5 = ifelse(`5 and 7.5 in` == 0, 1, `7.5 in` / `5 and 7.5 in`)) %>%
  group_by(Age) %>%
  summarize(selectivity7.5 = mean(selectivity7.5))

ageSimple <- ageSimple %>%
  left_join(selectivity7.5, by = "Age") %>%
  mutate(Prop = ifelse(GearMesh == "7.5 in Gill Net", Prop / selectivity7.5, Prop)) %>%
  select(-selectivity7.5) %>%
  write_csv("AgeSimple.csv")

# Transform data for fitting multinomial and ordinal logistic regression models
# Z-score continuous variables, convert age proportions to counts
age <- ageSimple %>%
  # Assign an effective sample size n_eff, where n_eff = 100 when n is > 100 or NA, (Fleischman and Reimer 2017).
  mutate(n_eff = ifelse(is.na(n), 100, ifelse(n > 100, 100, n))) %>% 
  # Convert proportions to counts 
  mutate(Count = round(Prop * n_eff)) %>%
  # Z-score ReturnYear
  mutate(zReturnYear = scale(ReturnYear))

# Drop groups with < 5 years of data from the analysis (to avoid problems with multicolinearity)
# First, tally return years of data for each group
groupYears <- age %>%
  select(Group, Region, Component, ReturnYear) %>%
  distinct() %>% # remove duplicate group-years
  group_by(Group, Region, Component) %>%
  summarize(ReturnYears = n())

# Next, identify groups with > 4 return years of data
groupsWith5YearsData <- groupYears %>%
  filter(ReturnYears > 4)

# Finally, drop groups with < 5 return years of data from the age dataframe, and turn categorical
# variables into factors
age <- age %>%
  right_join(groupsWith5YearsData, by = c("Group", "Region", "Component")) %>%
  mutate(# Turn strings into factors
          Group = factor(Group),
          Region = factor(Region),
          Component = factor(Component),
          Gear = factor(Gear),
          Mesh = factor(Mesh),
          GearMesh = factor(GearMesh),
          GroupComponent = str_c(Group, Component, sep = " "),
          RegionComponent = str_c(Region, Component, sep = " "),
          # Turn Age into an ordered factor (important for fitting ordered models below)
          Age = factor(Age, levels = c("Age1.1", "Age1.2", "Age1.3", "Age1.4", "Age1.5"), ordered = T),
          # Relevel factors to set baseline reference levels
          Region = relevel(Region, ref = "North"),
          Group = relevel(Group, ref = "Deshka"),
          Component = relevel(Component, ref = "Escapement"),
          Gear = relevel(Gear, ref = "Weir"),
          GearMesh = relevel(GearMesh, ref = "Weir"))


# # Exploratory data analysis--------------
# # Which return years do we need to estimate age comp for?  (Years with escapement but no age comp)
# RYsMissingAge <- escape %>%
#   filter(!is.na(Escapement)) %>%
#   filter(ReturnYear > 1979 & ReturnYear < 2016) %>%
#   left_join(sites, by = c("Population" = "Group")) %>%
#   filter(HatcheryWild != "Hatchery") %>%
#   filter(Population != "Funny" & Population != "Lewis" & Population != "Quartz" & 
#            Population != "Slikok") %>%
#   anti_join(ageFull, by = c("Population" = "Group", "ReturnYear")) %>%
#   select(Population, ReturnYear, Region, GroupType, Escapement)
# # We are missing lots of Northern populations (various years 1980-2015) and 1 Southern population 
# #  (Deep Creek, 1981-2015)
# 
# Plot the data
# 
# # Pairs plot of the predictors to visualize the data
# pairs.plot <- ggpairs(age, columns = c("Region", "Group", "ReturnYear", "Component"))
# pairs.plot

# Make a vector of all years 1980-2015
studyYears <- data_frame(ReturnYear = 1980:2015)

# Fill missing years with NA values (to prevent lines interpolating across years with missing data)
groupComponents <- age %>%
  select(Group, Component, GroupComponent, Region, RegionComponent, Gear) %>%
  distinct()

agePlot <- age %>%
  select(GroupComponent, ReturnYear, Age, Prop) %>%
  spread(key = GroupComponent, value = Prop) %>%
  full_join(studyYears, by = "ReturnYear") %>%
  gather(key = GroupComponent, value = Prop, 3:12) %>%
  left_join(groupComponents, by = "GroupComponent")

ageByComponent.plot <- ggplot(data = agePlot, aes(x = ReturnYear, y = Prop, group = GroupComponent, 
                                                  color = Component)) +
  facet_grid(Age ~ .) +
  geom_line() +
  labs(x = "Return year", y = "Age Proportion") +
  scale_x_continuous(breaks = seq(1980, 2015, 5))
ageByComponent.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/figs/Age comp by Component.png")

ageByGroup.plot <- ggplot(data = agePlot, aes(x = ReturnYear, y = Prop, group = GroupComponent, 
                                              color = Group)) +
  facet_grid(Age ~ .) +
  geom_line() +
  labs(x = "Return year", y = "Age Proportion") +
  scale_color_discrete(name = "River or fishery") +
  scale_x_continuous(breaks = seq(1980, 2015, 5))
ageByGroup.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/figs/Age comp by river or fishery.png")

# Plot the age comp for Kenai and ESSN only to compare run components and gear types
agePlotKenai <- agePlot %>%
  filter(Group == "ESSN" | Group == "Kenai.Late") 

ageByComponentKenai.plot <- ggplot(data = agePlotKenai, 
                                   aes(x = ReturnYear, y = Prop, color = Component)) +
  facet_grid(Age ~ .) +
  geom_line() +
  # geom_point(shape = 1, fill = NA) +
  labs(x = "Return year", y = "Age Proportion") +
  scale_color_discrete(name = "Run component") +
  scale_x_continuous(breaks = seq(1980, 2015, 5))
ageByComponentKenai.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/figs/Kenai late run age comp by component.png")

# Plot the age comp for Deshka and NSN only to compare gear types

agePlotDeshka <- agePlot %>%
  filter(Group == "NSN" | Group == "Deshka")

ageByGearDeshka.plot <- ggplot(data = agePlotDeshka, 
                               aes(x = ReturnYear, y = Prop, color = GroupComponent)) +
  facet_grid(Age ~ .) +
  geom_line() +
  labs(x = "Return year", y = "Age Proportion") +
  scale_color_discrete(name = "Run component") +
  scale_x_continuous(breaks = seq(1980, 2015, 5))
ageByGearDeshka.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/figs/Deshka age comp by component.png")

ageByRegion.plot <- ggplot(data = agePlot, aes(x = ReturnYear, y = Prop, color = Region)) +
  facet_grid(Age ~ .) +
  geom_point(shape = 1, fill = NA) +
  labs(x = "Return year", y = "Age Prop") +
  scale_x_continuous(breaks = seq(1980, 2015, 5)) +
  geom_smooth(method = 'lm', se = F)
ageByRegion.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/figs/Age comp by region.png")

ageByRegionComp.plot <- ggplot(data = agePlot, aes(x = ReturnYear, y = Prop, color = RegionComponent)) +
  facet_grid(Age ~ .) +
  geom_point(shape = 1, fill = NA) +
  labs(x = "Return year", y = "Age Prop") +
  scale_x_continuous(breaks = seq(1980, 2015, 5)) +
  scale_color_manual(name = "Region and run component", values = c("dark red", "red", "orange",
                                                                   "dark blue", "blue", "cyan",
                                                                   "green")) +
  geom_smooth(method = 'lm', se = F)
ageByRegionComp.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/figs/Age comp by region & run component.png")

# # Fit a predictive model to estimate age composition-----------
# Method 1: Fit hierarchical multinomial logistic regression (MLR) models ----------
# to the full age composition dataset to test for effects of run component (i.e., escapement, commercial
# harvest or sport harvest), group (i.e., population or fishery), region, and return year.

# Fit multinomial models (Hess = T is necessary for model.sel to work)
# null model
age.mlr0 <- multinom(data = age, Age ~ 1, weights = Count, Hess = T)
# full model
age.mlr1 <- multinom(data = age, Age ~ Component + Group * zReturnYear, weights = Count, Hess = T)
age.mlr2 <- multinom(data = age, Age ~ Component + Group + zReturnYear, weights = Count, Hess = T)
age.mlr3 <- multinom(data = age, Age ~ Component + Group, weights = Count, Hess = T)
age.mlr4 <- multinom(data = age, Age ~ Component + zReturnYear, weights = Count, Hess = T)
age.mlr5 <- multinom(data = age, Age ~ Component, weights = Count, Hess = T)
age.mlr6 <- multinom(data = age, Age ~ Group * zReturnYear, weights = Count, Hess = T)
age.mlr7 <- multinom(data = age, Age ~ Group + zReturnYear, weights = Count, Hess = T)
age.mlr8 <- multinom(data = age, Age ~ Group, weights = Count, Hess = T)
age.mlr9 <- multinom(data = age, Age ~ zReturnYear, weights = Count, Hess = T) 
age.mlr10 <- multinom(data = age, Age ~ Component + Region * zReturnYear, weights = Count, Hess = T)
age.mlr11 <- multinom(data = age, Age ~ Component + Region + zReturnYear, weights = Count, Hess = T)
age.mlr12 <- multinom(data = age, Age ~ Component + Region, weights = Count, Hess = T)
age.mlr13 <- multinom(data = age, Age ~ Region * zReturnYear, weights = Count, Hess = T)
age.mlr14 <- multinom(data = age, Age ~ Region + zReturnYear, weights = Count, Hess = T)
age.mlr15 <- multinom(data = age, Age ~ Region, weights = Count, Hess = T)

# Model selection table 
#            (Int) Cmp Grp zRY Grp:zRY Rgn Rgn:zRY df    logLik    AICc   delta weight
# age.mlr1      +   +   +   +       +             68 -20969.45 42075.4    0.00      1
# age.mlr6      +       +   +       +             64 -21003.27 42135.0   59.57      0
# age.mlr2      +   +   +   +                     40 -21088.48 42257.1  181.73      0
# age.mlr7      +       +   +                     36 -21108.51 42289.2  213.76      0
# age.mlr10     +   +       +           +       + 32 -21292.46 42649.0  573.62      0
# age.mlr11     +   +       +           +         24 -21330.90 42709.9  634.45      0
# age.mlr3      +   +   +                         36 -21596.18 43264.5 1189.10      0
# age.mlr13     +           +           +       + 24 -21639.84 43327.7 1252.33      0
# age.mlr14     +           +           +         16 -21679.44 43390.9 1315.51      0
# age.mlr8      +       +                         32 -21694.06 43452.2 1376.83      0
# age.mlr12     +   +                   +         20 -21834.06 43708.2 1632.76      0
# age.mlr15     +                       +         12 -22181.18 44386.4 2310.97      0
# age.mlr4      +   +       +                     16 -22555.31 45142.6 3067.24      0
# age.mlr9      +           +                      8 -22918.32 45852.7 3777.25      0
# age.mlr5      +   +                             12 -23115.21 46254.4 4179.03      0
# age.mlr0      +                                  4 -23736.62 47481.2 5405.84      0
# Models ranked by AICc(x) 
# Results: the full model was by far the most parsimonious.  

# Best population-specific model
# summary(age.mlr1)

# Best region-specific model
# summary(age.mlr10)

# Calculate McFadden's pseudo R-squared 
# (following https://stackoverflow.com/questions/43623076/multinomial-logit-in-r-mlogit-versus-nnet)
# Calculate log likelihood for the best population-specific model
age.mlr1.loglik <- nnet:::logLik.multinom(age.mlr1)
# Calculate log likelihood for a null (intercepts only) model
age.mlr0.loglik <- nnet:::logLik.multinom(age.mlr0)
(age.mlr1.mfr2 <- as.numeric(1 - age.mlr1.loglik / age.mlr0.loglik))
# [1] 0.116578
# The full model explains 12% of the variability in age composition

# Calculate pseudo R-squared for the best regional model
age.mlr10.loglik <- nnet:::logLik.multinom(age.mlr10)
(age.mlr9.mfr2 <- as.numeric(1 - age.mlr10.loglik / age.mlr0.loglik))
# [1] 0.1029702
# The best regional model explained 10% of the variability in age composition (nearly as much as the
# population-specific model)

# Hosmer-Lemeshow test?

# Method 2: Fit an identical set of ordinal logistic regression (OLR) models----------------
# This approach is simpler because it assumes each fixed effect has the same overall effect on
# the entire age distribution (i.e., it is associated with Chinook younger or older across the board),
# rather than modeling separate effects on each age class.  As a result, is likely not to fit the data 
# as well, but it also requires way fewer parameters.  

# null model
age.olr0 <- polr(data = age, Age ~ 1, weights = Count, Hess = T)
# full model
age.olr1 <- polr(data = age, Age ~ Component + Group * zReturnYear, weights = Count, Hess = T)
age.olr2 <- polr(data = age, Age ~ Component + Group + zReturnYear, weights = Count, Hess = T)
age.olr3 <- polr(data = age, Age ~ Component + Group, weights = Count, Hess = T)
age.olr4 <- polr(data = age, Age ~ Component + zReturnYear, weights = Count, Hess = T)
age.olr5 <- polr(data = age, Age ~ Component, weights = Count, Hess = T)
age.olr6 <- polr(data = age, Age ~ Group * zReturnYear, weights = Count, Hess = T)
age.olr7 <- polr(data = age, Age ~ Group + zReturnYear, weights = Count, Hess = T)
age.olr8 <- polr(data = age, Age ~ Group, weights = Count, Hess = T)
age.olr9 <- polr(data = age, Age ~ zReturnYear, weights = Count, Hess = T) 
age.olr10 <- polr(data = age, Age ~ Component + Region * zReturnYear, weights = Count, Hess = T)
age.olr11 <- polr(data = age, Age ~ Component + Region + zReturnYear, weights = Count, Hess = T)
age.olr12 <- polr(data = age, Age ~ Component + Region, weights = Count, Hess = T)
age.olr13 <- polr(data = age, Age ~ Region * zReturnYear, weights = Count, Hess = T)
age.olr14 <- polr(data = age, Age ~ Region + zReturnYear, weights = Count, Hess = T)
age.olr15 <- polr(data = age, Age ~ Region, weights = Count, Hess = T)
# Model selection using AICc
modsel.olr <- model.sel(age.olr0, age.olr1, age.olr2, age.olr3, age.olr4, age.olr5, age.olr6, age.olr7, 
                    age.olr8, age.olr9, age.olr10, age.olr11, age.olr12, age.olr13, age.olr14,
                    age.olr15)
modsel.olr

# Model selection table 
#           (Int) Cmp Grp     zRY Grp:zRY Rgn Rgn:zRY df    logLik    AICc   delta weight
# age.olr1      +   +   + -0.2046       +             20 -21607.21 43254.5    0.00      1
# age.olr6      +       + -0.3421       +             19 -21634.41 43306.9   52.41      0
# age.olr2      +   +   + -0.5453                     13 -21719.54 43465.1  210.65      0
# age.olr7      +       + -0.5581                     12 -21722.43 43468.9  214.41      0
# age.olr10     +   +     -0.4556           +       + 11 -21853.53 43729.1  474.61      0
# age.olr11     +   +     -0.5364           +          9 -21861.40 43740.8  486.36      0
# age.olr13     +         -0.4306           +       +  9 -22214.38 44446.8 1192.32      0
# age.olr14     +         -0.4960           +          7 -22221.99 44458.0 1203.53      0
# age.olr3      +   +   +                             12 -22223.55 44471.1 1216.65      0
# age.olr8      +       +                             11 -22307.37 44636.8 1382.29      0
# age.olr12     +   +                       +          8 -22359.47 44734.9 1480.48      0
# age.olr4      +   +     -0.5192                      7 -22679.01 45372.0 2117.57      0
# age.olr15     +                           +          6 -22708.21 45428.4 2173.96      0
# age.olr9      +         -0.5706                      5 -22956.18 45922.4 2667.90      0
# age.olr5      +   +                                  6 -23198.03 46408.1 3153.61      0
# age.olr0      +                                      4 -23736.62 47481.2 4226.78      0
# Models ranked by AICc(x) 
# Same result as for the multinomial models

summary(age.olr10)
# Call:
#   polr(formula = Age ~ Component + Region * zReturnYear, data = age, 
#        weights = Count, Hess = T)
# 
# Coefficients:
#   Value Std. Error t value
# ComponentCommercial Harvest -0.8571    0.04074 -21.035
# ComponentSport Harvest       0.1689    0.04458   3.787
# RegionKenai                  1.1137    0.03556  31.322
# RegionSouth                 -0.2281    0.05423  -4.206
# zReturnYear                 -0.4556    0.02665 -17.099
# RegionKenai:zReturnYear     -0.1325    0.03518  -3.765
# RegionSouth:zReturnYear     -0.1459    0.06085  -2.397
# 
# Intercepts:
#   Value    Std. Error t value 
# Age1.1|Age1.2  -3.3512   0.0480   -69.7658
# Age1.2|Age1.3  -0.9515   0.0308   -30.8446
# Age1.3|Age1.4   0.8008   0.0307    26.0634
# Age1.4|Age1.5   4.5790   0.0610    75.0130
# 
# Residual Deviance: 43707.06 
# AIC: 43729.06 

# Just out of curiosity, compare multinomial models against ordinal models
modsel.mlrolr <- model.sel(age.mlr0, age.mlr1, age.mlr2, age.mlr3, age.mlr4, age.mlr5, age.mlr6, age.mlr7, 
                           age.mlr8, age.mlr9, age.mlr10, age.mlr11, age.mlr12, age.mlr13, age.mlr14,
                           age.mlr15,
                           age.olr0, age.olr1, age.olr2, age.olr3, age.olr4, age.olr5, age.olr6, age.olr7, 
                           age.olr8, age.olr9, age.olr10, age.olr11, age.olr12, age.olr13, age.olr14,
                           age.olr15)
modsel.mlrolr
# As expected, the more complex MLR models fit the data better (but required many more parameters).
# Based on AICc, the MLR models were more parsimonious, so we used the MLR approach for the run 
# reconstruction.

# Predict the age composition for Population-ReturnYears lacking data --------------
# using the best regional MLR model

# 1) Make a dataframe of x data:
Component <- c(rep("Escapement", 3*36), rep("Sport Harvest", 3*36))
Region <- rep(c(rep("North",36), rep("Kenai",36), rep("South", 36)), 2)
ReturnYear <- rep(1980:2015, 2*3)
xdata <- data.frame(Component, Region, ReturnYear)

# 2) Convert ReturnYear to zReturnYear using same scale as before
zReturnYear <- age %>%
  select(ReturnYear, zReturnYear) %>%
  distinct() %>%
  arrange(ReturnYear)

xdata <- left_join(xdata, zReturnYear, by = "ReturnYear")

# 2) Predict Age as a function of Component, Region, and ReturnYear
predictions <- predict(age.mlr10, newdata = xdata, type = "probs")

agePredicted <- cbind(xdata, predictions) %>%
  gather(key = Age, value = Prop, Age1.1:Age1.5) %>%
  write_csv("AgePredicted.csv")

# 3) Plot the predictions to make sure they make sense
AgePredictedByRegion.plot <- ggplot(data = agePredicted, aes(x = ReturnYear, y = Prop, color = Region,
                                                             linetype = Component)) +
  facet_grid(Age ~ .) +
  labs(x = "Return year", y = "Age Prop") +
  scale_x_continuous(breaks = seq(1980, 2015, 5)) +
  geom_smooth(se = F)
AgePredictedByRegion.plot
ggsave("~/Desktop/Cook Inlet Chinook/Analysis/figs/Predicted age comp by region.png")

# Old code, no longer used:----------------

# # Make subsets of the main age dataset for more specific analyses:
# # Look at terminal age data only (remove data from mixed-stock fisheries)
# ageTerm <- age %>%
#   filter(Component != "Commercial Harvest" & Component != "Subsistence Harvest") %>%
#   # Relevel factors to set baseline reference levels
#   mutate(Component = factor(Component),
#          Group = factor(Group),
#          Gear = factor(Gear),
#          GearMesh = factor(GearMesh),
#          Component = relevel(Component, ref = "Escapement"),
#          Group = relevel(Group, ref = "Deshka"),
#          Gear = relevel(Gear, ref = "Weir"),
#          GearMesh = relevel(GearMesh, ref = "Weir"))
# 
# # Filter down to only the "core" age classes ocean ages 2, 3, and 4 (excluding 1-ocean and 5-ocean)
# ageCore <- age %>%
#   filter (Age == "Age1.2" | Age == "Age1.3" | Age == "Age1.4") %>%
#   mutate(Age = factor(Age, levels = c("Age1.2", "Age1.3", "Age1.4")), ordered = T)
# 
# # Core age classes from terminal sampling only
# ageCoreTerm <- ageTerm %>%
#   filter (Age == "Age1.2" | Age == "Age1.3" | Age == "Age1.4") %>%
#   mutate(Age = factor(Age, levels = c("Age1.2", "Age1.3", "Age1.4")), ordered = T)


# Estimate and correct for age-selectivity of different sampling gears in the Deshka--------
# Use a statistical model to test for an effect of sampling gear (angling / comm gill net / weir)
# on age composition of the Deshka River and Northern Set Net fishery.

# deshka <- age %>%
#   filter(Group == "Deshka" | Group == "NSN") %>%
#   # Relevel factor
#   mutate(Gear = factor(Gear, levels = c("Weir", "Angling", "Gill Net")))
# 
# deshkaCore <- ageCore %>%
#   filter(Group == "Deshka" | Group == "NSN") %>%
#   # Relevel factor
#   mutate(Gear = factor(Gear, levels = c("Weir", "Angling", "Gill Net")))
# 
# deshka.olr1 <- polr(Age ~ Gear * zReturnYear, data = deshka, weights = Count, Hess = T)
# deshka.olr2 <- polr(Age ~ Gear + zReturnYear, data = deshka, weights = Count, Hess = T)
# deshka.olr3 <- polr(Age ~ Gear, data = deshka, weights = Count, Hess = T)
# deshka.olr4 <- polr(Age ~ zReturnYear, data = deshka, weights = Count, Hess = T)
# deshka.olr5 <- polr(Age ~ Gear + factor(ReturnYear), data = deshka, weights = Count, Hess = T)
# deshka.olr6 <- polr(Age ~ factor(ReturnYear), data = deshka, weights = Count, Hess = T)
# 
# deshka.olr.models <- model.sel(deshka.olr1, deshka.olr2, deshka.olr3, deshka.olr4, deshka.olr5, deshka.olr6)
# deshka.olr.models
# 
# summary(deshka.olr5)
# 
# deshka.mlr1 <- multinom(Age ~ Gear * zReturnYear, data = deshka, weights = Count, Hess = T)
# deshka.mlr2 <- multinom(Age ~ Gear + zReturnYear, data = deshka, weights = Count, Hess = T)
# deshka.mlr3 <- multinom(Age ~ Gear, data = deshka, weights = Count, Hess = T)
# deshka.mlr4 <- multinom(Age ~ zReturnYear, data = deshka, weights = Count, Hess = T)
# deshka.mlr5 <- multinom(Age ~ Gear + factor(ReturnYear), data = deshka, weights = Count, Hess = T)
# deshka.mlr6 <- multinom(Age ~ factor(ReturnYear), data = deshka, weights = Count, Hess = T)
# 
# summary(deshka.mlr5)
# 
# deshka.mlr.models <- model.sel(deshka.mlr1, deshka.mlr2, deshka.mlr3, deshka.mlr4, deshka.mlr5, deshka.mlr6)
# deshka.mlr.models
# 
# deshka.all.models <- model.sel(deshka.olr1, deshka.olr2, deshka.olr3, deshka.olr4, deshka.olr5, deshka.olr6,
#                                deshka.mlr1, deshka.mlr2, deshka.mlr3, deshka.mlr4, deshka.mlr5, deshka.mlr6)
# deshka.all.models
# 
# deshkaCore.olr1 <- polr(Age ~ Gear * zReturnYear, data = deshkaCore, weights = Count, Hess = T)
# deshkaCore.olr2 <- polr(Age ~ Gear + zReturnYear, data = deshkaCore, weights = Count, Hess = T)
# deshkaCore.olr3 <- polr(Age ~ Gear, data = deshkaCore, weights = Count, Hess = T)
# deshkaCore.olr4 <- polr(Age ~ zReturnYear, data = deshkaCore, weights = Count, Hess = T)
# deshkaCore.olr5 <- polr(Age ~ Gear + factor(ReturnYear), data = deshkaCore, weights = Count, Hess = T)
# deshkaCore.olr6 <- polr(Age ~ factor(ReturnYear), data = deshkaCore, weights = Count, Hess = T)
# 
# deshkaCore.olr.models <- model.sel(deshkaCore.olr1, deshkaCore.olr2, deshkaCore.olr3, deshkaCore.olr4,
#                                    deshkaCore.olr5, deshkaCore.olr6)
# deshkaCore.olr.models
# 
# deshkaCore.olr1
# 
# summary(deshkaCore.olr1)

# # Estimate and correct for age-selectivity of different sampling gears in the Kenai-----------
# # I abandoned this effort after examining the methods of the Kenai run reconstruction more closely.
# # Fleischman and McKinley used age comps from the ESSN fishery for their "harvest below RM 9"
# # component of the run and age comps from the in-river gill netting at RM 9 for the "in-river run"
# # component.  So calculating selectivities using the same data is circular.

# # Join the observed age composition (from the 3 gear types) with the age composition
# # of the total run (from the run reconstruction) and calculate selectivities for each
# # gear g, age a, and year t
# kenaiAgeComp <- ageFull %>%
#   filter(Group == "Kenai.Late" | Group == "ESSN") %>%
#   filter(ReturnYear > 2001 & ReturnYear < 2013) %>%
#   filter(Gear != "Model Estimate") %>%
#   mutate(GearMesh = ifelse(is.na(Mesh), Gear, paste(Mesh, Gear))) %>%
#   select(ReturnYear, n, GearMesh, Age, Prop) %>%
#   left_join(select(kenaiTR, ReturnYear, Age, PropRR = Prop), by = c("ReturnYear", "Age")) %>%
#   # rename(Prop = Prop.x, PropRR = Prop.y) %>%
#   mutate(Selectivity_gat = Prop / PropRR)
# 
# # Q: Can we use the 5" gill net (in-river test fishery) age comp data to correct for the selectivity of 
# # the < 6" gill net mixed-stock fisheries (ESSN, NSN, WSN, Tyonek sub)?
# 
# selectivity.trend.plot <- ggplot(data = kenaiAgeComp, aes(x = ReturnYear, y = Selectivity_gat)) +
#   facet_grid(rows = vars(Age), cols = vars(GearMesh)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   scale_y_continuous(name = "Selectivity") +
#   scale_x_continuous(name = "Return Year") +
#   geom_hline(yintercept = 1)
# selectivity.trend.plot
# 
# # Plot the core age classes only...
# selectivity.trend.core.plot <- ggplot(data = filter(kenaiAgeComp, Age != "Age1.1" & Age != "Age1.5"),
#                                  aes(x = ReturnYear, y = Selectivity_gat)) +
#   facet_grid(rows = vars(Age), cols = vars(GearMesh)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   scale_y_continuous(name = "Selectivity") +
#   scale_x_continuous(name = "Return Year") +
#   geom_hline(yintercept = 1)
# selectivity.trend.core.plot
# 
#A: No, the selectivities of the 5" and <6" gill net samples are totally different. Don't use 5"
# selectivities to correct the <6" data.  The 5" and 7.5" gill net samples are not very selective, so
# use those instead of the 5" gill net samples.

# Fit linear models to describe changes in selectivity over time (using broom package)
# selectivity.trends <- kenaiAgeComp %>%
#   filter(GearMesh != "5 in Gill Net") %>%
#   group_by(GearMesh, Age) %>%
#   do(fit.lm = lm(Selectivity_gat ~ ReturnYear, data = .))
# 
# Summarize the coeficients and summary stats of these linear models
# selectivity.trends.coef <- tidy(selectivity.trends, fit.lm)
# selectivity.trends.coef
# selectivity.trends.stats <- glance(selectivity.trends, fit.lm)
# selectivity.trends.stats
# 
# Fit intercept-only models to describe fixed selectivities (null models for comparison against lms)
# selectivity.static <- kenaiAgeComp %>%
#   group_by(GearMesh, Age) %>%
#   do(fit.lm = lm(Selectivity_gat ~ 1, data = .))
# selectivity.static.stats <- glance(selectivity.static, fit.lm)
# selectivity.static.stats
# 
# selectivity.static.aicc <- selectivity.static
# Compare the linear and intercept-only models using AICc (for each GearMesh and Age)
# First combine the dataframes of summary stats
# selectivity.models.stats <- selectivity.trends.stats %>%
#   bind_rows(selectivity.static.stats) %>%
#   # Then calculate AICc for each model
#   mutate(Model = ifelse(df == 1, "Static", "Linear"),
#          k = df,
#          n = df.residual + df,
#          AICc = AIC + (2*k^2 + 2*k)/(n-k-1)) %>%
#   arrange(GearMesh, Age)
# 
# Next, calculate the lowest AICc for each GearMesh and Age
# selectivity.models.lowAICc <- selectivity.models.stats %>%
#   group_by(GearMesh, Age) %>%
#   summarize(lowAICc = min(AICc))
# 
# Join low AICc dataframe with the summary stats
# selectivity.models.stats <- selectivity.models.stats %>%
#   left_join(selectivity.models.lowAICc, by = c("GearMesh", "Age")) %>%
#   # calculate delta AICc for each model
#   mutate(deltaAICc = AICc - lowAICc) %>%
#   select(GearMesh, Age, Model, deltaAICc, r.squared) %>%
#   arrange(GearMesh, Age, desc(Model))
# 
# Which linear (time-varying) selectivity functions are supported by the data (e.g. delta AICc > 0
# for the static intercept-only model)
# selectivity.trends.parsim <- selectivity.models.stats %>%
#   filter(Model == "Static" & deltaAICc > 0)
# 
# # Should I use time-varying selectivities when I correct the age comp data?
# # Pros: Many selectivities do appear to be changing over time based on AICc.
# # It does make sense that age-selectivity would be changing, given that size-at-age
# # is changing (Lewis et al. 2015).  It will be fairly straightforward to predict the selectivity_gat
# # from 1986-2012 for the 7" gill net and angling gears.
# # Cons: I want to predict the selectivity_gat for years outside the range of the data.  This is
# # probably not a huge deal because we have long time series 1986-2012, and we only want to go a few
# # years forward and back
# # Conclusion: use time-varying sensitivities for all gear types
# 
# # Summarize the time-varying selectivity model coefficients
# selectivity <- selectivity.trends.coef %>%
#   select(GearMesh, Age, term, estimate) %>%
#   spread(key = term, value = estimate) %>%
#   rename(Intercept = `(Intercept)`, Slope = ReturnYear)
# 
# # Correct all age composition data for age selectivity of the gear (assuming weirs are non-selective)
# ageCorrRaw <- age %>%
#   left_join(selectivity, by = c("GearMesh", "Age")) %>%
#   mutate(Intercept = ifelse(Gear == "Weir", 1, Intercept),
#          Slope = ifelse(Gear == "Weir", 0, Slope),
#          Selectivity_gat = Intercept + ReturnYear*Slope,
#          PropCorr = Prop / Selectivity_gat)
# 
# # Recalculate proportions so that the sum of all corrected proportions equals 1
# # Reshape data with one row for each Group, ReturnYear, and GearMesh (age classes in columns)
# 
# ageCorrWide <- ageCorrRaw %>%
#   select(-Prop, -Selectivity_gat, -Intercept, -Slope) %>%
#   spread(key = Age, value = PropCorr) %>%
#   # Calculate the sum of the corrected proportions
#   mutate(SumPropCorr = rowSums(cbind(Age1.1, Age1.2, Age1.3, Age1.4, Age1.5), na.rm = T),
#          
#          #Divide each proportion by the SumProportions for that row.  New values should add to 1
#          Age1.1 = Age1.1/SumPropCorr,
#          Age1.2 = Age1.2/SumPropCorr,
#          Age1.3 = Age1.3/SumPropCorr,
#          Age1.4 = Age1.4/SumPropCorr,
#          Age1.5 = Age1.5/SumPropCorr,
#          # Verify that the corrected values add to 1
#          SumPropCorr2 = rowSums(cbind(Age1.1, Age1.2, Age1.3, Age1.4, Age1.5), na.rm = T))
# 
# # Now reshape back to long form with age classes in rows
# ageCorr <- ageCorrWide %>%
#   gather(key = Age, value = Prop, Age1.1:Age1.5) %>%
#   # Now that the data is corrected for age-selectivity of the gear, remove the data that will not be
#   # used for fitting the age composition models (e.g., all 5" gill net, and 7.5" gill net starting in 2002)
#   filter(GearMesh != "5 in gill net")
# 
# # Plot the age comp for Kenai and ESSN only to compare gear types
# agePlotKenai <- ageCorr %>%
#   filter(Group == "ESSN" | Group == "Kenai.Late") %>%
#   filter(GearMesh != "5 in Gill Net") %>%
#   mutate(GroupGear = ifelse(is.na(Mesh), str_c(Group, Gear, sep = " "), 
#                             str_c(Group, Mesh, Gear, sep = " ")))
# 
# ageByGearKenai.plot <- ggplot(data = agePlotKenai, 
#                               aes(x = ReturnYear, y = Prop, color = GroupGear)) +
#   facet_grid(Age ~ .) +
#   geom_line() +
#   # geom_point(shape = 1, fill = NA) +
#   labs(x = "Return year", y = "Age Prop") +
#   scale_color_discrete(name = "Gear type") +
#   scale_x_continuous(breaks = seq(1980, 2015, 5))
# ageByGearKenai.plot
# ggsave("~/Desktop/Cook Inlet Chinook/Analysis/figs/Kenai late run age comp by gear type_corrected.png")
# 
# # Plot the age comp for Deshka and NSN only to compare gear types
# agePlotDeshka <- ageCorr %>%
#   filter(Group == "NSN" | Group == "Deshka") %>%
#   mutate(GroupGear = ifelse(is.na(Mesh), str_c(Group, Gear, sep = " "), 
#                             str_c(Group, Mesh, Gear, sep = " ")))
# 
# ageByGearDeshka.plot <- ggplot(data = agePlotDeshka, 
#                                aes(x = ReturnYear, y = Prop, color = GroupGear)) +
#   facet_grid(Age ~ .) +
#   geom_line() +
#   geom_point(shape = 1, fill = NA) +
#   labs(x = "Return year", y = "Age Prop") +
#   scale_color_discrete(name = "Gear type") +
#   scale_x_continuous(breaks = seq(1980, 2015, 5))
# ageByGearDeshka.plot
# ggsave("~/Desktop/Cook Inlet Chinook/Analysis/figs/Deshka age comp by gear type_corrected.png")
# # Uh oh.  The gear selectivity corrections from the Kenai data DO NOT appear to improve the Deshka data.
# 
# # Plot pairwise correlations of age comp by gear for Deshka / NSN
# # Uncorrected
# ageWideDeshka <- age %>%
#   filter(Group == "NSN" | Group == "Deshka") %>%
#   mutate(GroupGear = ifelse(is.na(Mesh), str_c(Group, Gear, sep = " "), 
#                             str_c(Group, Mesh, Gear, sep = " "))) %>%
#   select(GroupGear, ReturnYear, Age, Prop) %>%
#   spread(key = GroupGear, value = Prop, fill = NA)
# 
# ggpairs(data = filter(ageWideDeshka, Age == "Age1.2"), columns = 3:5)
# ggpairs(data = filter(ageWideDeshka, Age == "Age1.3"), columns = 3:5)
# ggpairs(data = filter(ageWideDeshka, Age == "Age1.4"), columns = 3:5)
# 
# # Corrected
# ageWideDeshkaCorr <- ageCorr %>%
#   filter(Group == "NSN" | Group == "Deshka") %>%
#   mutate(GroupGear = ifelse(is.na(Mesh), str_c(Group, Gear, sep = " "), 
#                             str_c(Group, Mesh, Gear, sep = " "))) %>%
#   select(GroupGear, ReturnYear, Age, Prop) %>%
#   spread(key = GroupGear, value = Prop, fill = NA)
# 
# ggpairs(data = filter(ageWideDeshkaCorr, Age == "Age1.2"), columns = 3:5)
# ggpairs(data = filter(ageWideDeshkaCorr, Age == "Age1.3"), columns = 3:5)
# ggpairs(data = filter(ageWideDeshkaCorr, Age == "Age1.4"), columns = 3:5)
# 
# 