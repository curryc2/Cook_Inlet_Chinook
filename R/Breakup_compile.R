# Breakup_compile.R
# Erik Schoen
# eschoen@alaska.edu
# 8-2018

# Calculate breakup dates in terms of DOY

# Load packages and read in data---------
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readr)
library(GGally)
library(MuMIn)

setwd("~/Desktop/Cook Inlet Chinook/Analysis")
theme_set(theme_bw(12))

# Read in data
breakupRaw <- read_csv("./data/Breakup_Raw.csv")

# Clean up data and calculate breakup data in terms of DOY ----------
breakupRaw <- breakupRaw %>%
  mutate(BreakupDate = mdy(`Breakup Date`),
         BreakupDOY = yday(BreakupDate)) 

# Reshape data and plot pairs plot of all sites
breakup.wide <- breakupRaw %>%
  select(Year, Location, BreakupDOY) %>%
  spread(key = Location, value = BreakupDOY) %>%
  rename(Alexander = `Alexander Creek`,
         Yentna = `Lake Creek`)
pairs.plot <- ggpairs(breakup.wide)
pairs.plot

# Fit linear models predicting breakup date at Sunshine based on Susitna at Alexander Creek, Matanuska at Palmer, 
# or Yentna at Lake Creek.  Also try including Year as a covariate and an interaction between year and the other
# time series
breakup.A.lm1 <- lm(Sunshine ~ Alexander, data = breakup.wide)
breakup.A.lm2 <- lm(Sunshine ~ Year + Alexander, data = breakup.wide)
breakup.A.lm3 <- lm(Sunshine ~ Year * Alexander, data = breakup.wide)
model.sel(breakup.A.lm1, breakup.A.lm2, breakup.A.lm3)

# Model selection table 
# (Int)      Alx     Yer Alx:Yer df  logLik  AICc delta weight
# breakup.A.lm3  3662.00 -42.4000 -1.8120 0.02156  5  -9.395 -31.2  0.00      1
# breakup.A.lm1    42.09   0.6929                  3 -12.170  54.3 85.55      0
# breakup.A.lm2 -1275.00   0.7365  0.6561          4  -9.711   Inf   Inf      0
# Models ranked by AICc(x) 

# The third model was best based on AICc.
summary(breakup.A.lm3)
# Call:
#   lm(formula = Sunshine ~ Year * Alexander, data = breakup.wide)
# 
# Residuals:
#   24      25      26      30      33 
# -1.1931  2.8758 -1.6671 -0.1996  0.1840 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)     3.662e+03  1.347e+04   0.272    0.831
# Year           -1.812e+00  6.735e+00  -0.269    0.833
# Alexander      -4.240e+01  1.174e+02  -0.361    0.779
# Year:Alexander  2.156e-02  5.870e-02   0.367    0.776
# 
# Residual standard error: 3.542 on 1 degrees of freedom
# (41 observations deleted due to missingness)
# Multiple R-squared:  0.9471,	Adjusted R-squared:  0.7884 
# F-statistic: 5.968 on 3 and 1 DF,  p-value: 0.2902

# The best Alexander Creek model had excellent predictive power (r2 = 0.95).  Used this to predict breakup
# dates during 1995, 1999, 2001, 2003.  Still missing data for 2000 and 2011.

# Matanuska River at Palmer has observations for 2000 and 2011.  See if it is a good predictor of breakup at
# Sunshine.
breakup.P.lm1 <- lm(Sunshine ~ Palmer, data = breakup.wide)
breakup.P.lm2 <- lm(Sunshine ~ Year + Palmer, data = breakup.wide)
breakup.P.lm3 <- lm(Sunshine ~ Year * Palmer, data = breakup.wide)
model.sel(breakup.P.lm1, breakup.P.lm2, breakup.P.lm3)
# Model selection table 
#                  (Int)      Plm    Yer Plm:Yer df  logLik  AICc delta weight
# breakup.P.lm1   78.65   0.3814                 3 -61.685 131.2  0.00  0.740
# breakup.P.lm2 -330.50   0.4309  0.202          4 -61.220 133.8  2.56  0.206
# breakup.P.lm3 2678.00 -25.2700 -1.305 0.01288  5 -60.505 136.5  5.25  0.054
# Models ranked by AICc(x) 

# Model 1 had most support from the data
summary(breakup.P.lm1)
# Call:
#   lm(formula = Sunshine ~ Palmer, data = breakup.wide)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -27.2697  -1.4981   0.8323   4.6882  16.0096 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  78.6488    17.8500   4.406 0.000511 ***
#   Palmer        0.3814     0.1538   2.480 0.025495 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 9.701 on 15 degrees of freedom
# (29 observations deleted due to missingness)
# Multiple R-squared:  0.2908,	Adjusted R-squared:  0.2435 
# F-statistic:  6.15 on 1 and 15 DF,  p-value: 0.02549

# The best Palmer model had poor predictive power (r2 = 0.29).  Did not use.

# Yentna River at Lake Creek has observations for 2000 and 2011.  See if it is a good predictor of breakup at
# Sunshine.
breakup.Y.lm1 <- lm(Sunshine ~ Yentna, data = breakup.wide)
breakup.Y.lm2 <- lm(Sunshine ~ Year + Yentna, data = breakup.wide)
breakup.Y.lm3 <- lm(Sunshine ~ Year * Yentna, data = breakup.wide)
model.sel(breakup.Y.lm1, breakup.Y.lm2, breakup.Y.lm3)
# Model selection table 
#                   (Int)       Ynt      Yer Yer:Ynt df  logLik  AICc delta weight
# breakup.Y.lm3 24410.00 -203.7000 -12.1400  0.1018  5 -73.824 161.2  0.00  0.900
# breakup.Y.lm1    23.03    0.8078                   3 -79.540 166.3  5.17  0.068
# breakup.Y.lm2  -358.90    0.8725   0.1868          4 -78.809 167.8  6.66  0.032
# Models ranked by AICc(x) 
summary(breakup.Y.lm3)
# Call:
#   lm(formula = Sunshine ~ Year * Yentna, data = breakup.wide)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -18.1248  -2.2067   0.3844   4.0397  10.6923 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept) 24407.2841  7718.3564   3.162  0.00513 **
#   Year          -12.1356     3.8401  -3.160  0.00515 **
#   Yentna       -203.6751    63.7066  -3.197  0.00475 **
#   Year:Yentna     0.1018     0.0317   3.211  0.00460 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.595 on 19 degrees of freedom
# (23 observations deleted due to missingness)
# Multiple R-squared:  0.5498,	Adjusted R-squared:  0.4787 
# F-statistic: 7.735 on 3 and 19 DF,  p-value: 0.001417

# The best Yentna model had reasonable predictive power (r2 = 0.55).  I used this to predict breakup dates
# during 2000 and 2011.

breakup.wide <- breakup.wide %>%
  # Estimate the breakup date at Sunshine based on the best linear models using Alexander and Yentna data
  mutate(SunshineEstA = coef(breakup.A.lm3)[1] + coef(breakup.A.lm3)[2]*Year + coef(breakup.A.lm3)[3]*Alexander +
           coef(breakup.A.lm3)[4]*Year*Alexander,
         SunshineEstY = coef(breakup.Y.lm3)[1] + coef(breakup.Y.lm3)[2]*Year + coef(breakup.Y.lm3)[3]*Yentna +
           coef(breakup.Y.lm3)[4]*Year*Yentna,
    BreakupDOY = round(ifelse(!is.na(Sunshine), Sunshine,
                             ifelse(!is.na(Alexander), SunshineEstA, SunshineEstY))))
# Plot the estimated vs observed breakup dates at Sunshine
pairs.est.obs <- ggpairs(data = breakup.wide, columns = c("Sunshine", "SunshineEstA", "SunshineEstY"))
pairs.est.obs
# Looks good: the linear models do a pretty good job of estimating the observed data

breakup <- breakup.wide %>%
  select(Year, BreakupDOY) %>%
  filter(Year > 1979 & Year < 2017)
write_csv(breakup, "./data/Breakup.csv")

# Lag the breakup dates by 2 years to align with brood year
breakup.lag2 <- breakup %>%
  mutate(BreakupDOY.lag2 = lead(BreakupDOY, 2)) %>%
  filter(Year < 2010) %>%
  # Standardize breakup date across the 30-year timeseries
  mutate(zBreakupDOY.lag2 = scale(BreakupDOY.lag2))

# Plot the index of breakup timing (lagged by 2 years to correspond to brood years) over time for summary in 
# the results
breakup.plot <- ggplot(data = breakup.lag2, aes(x = Year, y = zBreakupDOY.lag2)) +
  geom_col() +
  scale_x_continuous(name = "Brood year") +
  scale_y_continuous(name = "Breakup timing in year\nof smolt outmigration (std)")
breakup.plot
ggsave("./figs/Breakup timeseries.png", width = 4, height = 3)
ggsave("./figs/Productivity timeseries.png", width = 6, height = 6)
