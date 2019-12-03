
# Environmental covariates for Cook Inlet Chinook productivity model
# Rebecca Shaftel
# rsshaftel@alaska.edu
# October 2018

#Note: this script includes data for 36 stream temperature sites.
# All 36 sites were used in models to predict stream temperature
# for May-September 1980 - 2016 and the predictions were used to 
# create stream temperature covariates.
# Precipitation covariates were generated in ArcGIS for 15 sites 
# with Chinook salmon data. These are the same 15 sites
# used in the Chinook Salmon productivity model in the manuscript. 

library(XLConnect)
library(tidyverse)
library(lubridate)
library(ggpmisc)
library(broom)
library(magrittr)
library(tools)
library(readxl)

options(tibble.print_max = 100, tibble.print_min = 10)

# 1. Read in stream temperature data  ------------------------------------------------

data.d <- paste(getwd(), "/data/Stream_temperature", sep = "")
data.f <- list.files(data.d, pattern = ".xlsx")

str.dat<-data.frame()
for(i in 1:length(data.f)) {
  wb <- loadWorkbook(paste(data.d, data.f[i], sep = "/"))
  sheets <- getSheets(wb)
  site <- strsplit(data.f[i], "water temp")[[1]][1]
  for(j in 1:length(sheets)){
    dat <- XLConnect::readWorksheet(wb, sheets[j])
    names(dat) <- c("Date", "Temperature")
    dat$Site <- site
    str.dat <- rbind(str.dat, dat)
  }
}

#Adding 3 sites on Kenai to str.dat df.
wb <- loadWorkbook(paste(getwd(), "/data/Stream_temperature/Kenai/Kenai water temp.xlsx", sep = ""))
sheets <- getSheets(wb)
for(j in 1:length(sheets)){
  site <- sheets[j]
  dat <- readWorksheet(wb,sheets[j])
  names(dat) <- c("Station", "Date", "Min.Temperature", "Temperature", "Max.Temperature")
  dat$Site <- site
  str.dat <- rbind(str.dat, dat[,c("Date", "Temperature", "Site")])
}  

#Add in one site on Little Susitna
wb <- loadWorkbook(paste(getwd(), "/data/Stream_temperature/Little_Su/Little_Susitna.xlsx", sep = ""))
ls <- readWorksheet(wb, sheet = "LS Millers Reach")
names(ls)[1] <- "DateTime"
ls$Date <- as.Date(ls$DateTime)
ls$Temperature <- ls$Ave
ls$Site <- "Little Susitna"

str.dat <- rbind(str.dat, ls[,c("Date", "Temperature", "Site")])
str.dat$Temperature <- as.numeric(str.dat$Temperature)

#check for NAs
#lots of dates (esp kenai) which are confirmed in excel sheets
str.dat[which(is.na(str.dat$Temperature)),][,3] %>% table

#trim white space off of site names
str.dat$Site <- str_trim(str.dat$Site, side = "right")

#add additional fields for plotting time series and summarizing data
str.dat$Day <- format(str.dat$Date, "%m-%d")
str.dat$Year <- year(str.dat$Date)
str.dat$Week <- week(str.dat$Date)

#remove NAs first because in long form and causing problems for week counts.
str.dat2 <- str.dat[!is.na(str.dat$Temperature),]

# 2. Plot of empirical stream temperature data -------------------------------

# What weeks are available for different sites?
#mostly 22 - 40, which is June 1 through early October.
str.dat2 %>%
  group_by(Site) %>%
  summarize(min(Week),
            max(Week),
            min(Year), 
            max(Year),
            max(Year) - (min(Year) - 1),
            length(unique(Year))) %>% print(n=40)

str.dat2 %>%
  filter(Week %in% 22:35) %>%
  ggplot(aes(x = as.Date(Day, "%m-%d"), y = Temperature, color = as.factor(Year))) +
  geom_line() +
  facet_wrap(~Site) +
  labs(color = "Year") +
  theme(axis.title.x = element_blank(), legend.position = "bottom")

# 3. Read in air temperature data fromairports -----------------------------------------

#GHCN Data from Sue Mauger - Cook Inletkeeper.  See email from 11-4-16 for links to documentation.
# Sue selected standard over metric units, so temps are in F and precip
# and snow are in inches.

air.dat <- read.csv("data/Air_temperature/airport_data_v1.csv", header = TRUE, stringsAsFactors = FALSE)
air.dat$Date <- as.Date(as.character(air.dat$DATE), format = "%Y%m%d")
air.dat[air.dat==-9999] <- NA
air.dat$temp.C <- apply(air.dat[,c("TMAX","TMIN")], 1, function(x) (mean(x, na.rm=TRUE)-32)*(5/9))
air.dat$precip.mm <- air.dat$PRCP*25.4

#add fields for plotting
air.dat$Day <- format(air.dat$Date, "%m-%d")
air.dat$Year <- year(air.dat$Date)
air.dat$Week <- week(air.dat$Date)

#plot of data
ggplot(air.dat, aes(as.Date(air.dat$Day, "%m-%d"), temp.C, color = Year)) +
  geom_line() +
  facet_wrap(~STATION_NAME)

#max temp. record high in talkeetna, 96 on 6/13/17, found article that confirms this measurement.
# http://www.climatecentral.org/blogs/all-time-heat-records-broken-in-alaska-heat-wave-to-continue-16131
aggregate(air.dat$TMAX, by = list(air.dat$STATION_NAME), function(x) max(x,na.rm=TRUE))

#get rid of NAs because otherwise they are included in week count.
air.dat2 <- air.dat[!is.na(air.dat$temp.C),]

#Create mean weekly air temperatures for weeks with 5 or more days of data.
weekCountAir <- air.dat2 %>% 
  group_by(STATION_NAME,Year,Week) %>% 
  summarize(weekCt = length(Day))

#create names of weeks with 5 or more days for calculating mean
air.dat3 <- merge(air.dat2, weekCountAir)
air.wk <- air.dat3 %>% 
  filter(weekCt > 4) %>% 
  group_by(STATION_NAME, Week, Year) %>% 
  summarize(meanAirTemp = mean(temp.C, na.rm = TRUE))

# 4. Crosswalk between stream and air temperature sites --------

#Remove Hidden Creek, no Chinook.
#Remove Russian River, only 1 year.
remove <- c("Hidden","Russian")

str.dat3 <- str.dat2[!str.dat2$Site %in% remove,]

#number of sites with different years of data
str.dat3 %>% 
  group_by(Site) %>% 
  summarize(yrct = length(unique(Year))) %>% 
  group_by(yrct) %>% 
  summarize(length(Site))

stream.sites <- unique(str.dat3$Site)
site.cw <- data.frame(Site = stream.sites, Air_Site = rep(NA, length(stream.sites)))
air.sites <- unique(air.dat$STATION_NAME)

site.cw[site.cw$Site %in% c("NF Campbell","Rabbit","Resurrection","Wasilla",
                            "Ship","Fish","Theodore","Chuitna"),"Air_Site"] <- air.sites[1]
site.cw[site.cw$Site %in% c("Anchor","Deep","Ninilchik","Stariski","McNeil"),"Air_Site"] <- air.sites[2]
site.cw[site.cw$Site %in% c("Alexander","Deshka","EF Chulitna","Little Willow",
                            "Montana","Willow","Byers","Cache","Chijuk",
                            "Deception","Moose (Mat)","Moose (Su)",
                            "Trapper","Troublesome","Little Susitna"),"Air_Site"] <- air.sites[3]
site.cw[site.cw$Site %in% c("Beaver","Crooked","Funny","Hidden","Moose","Quartz","Slikok","Soldotna"),"Air_Site"] <- air.sites[4]
site.cw[site.cw$Site %in% c("Kenai R at Soldotna","Kenai R at Skilak","Kenai R at Cooper Landing"),"Air_Site"] <- air.sites[4]


#Note regarding temperature data for fish models:
# Three sites for Kenai river to be used for late run. Only keep site at Soldotna, best fit.
site.cw$fish.model <- ifelse(site.cw$Site %in% c('Alexander', 'Anchor', 'Chuitna', 'Crooked', 'Deep', 'Deshka', 'EF Chulitna',
                                                 'Little Susitna', 'Little Willow', 'Montana', 'NF Campbell', 'Ninilchik',
                                                 'Theodore', 'Willow', 'Kenai R at Soldotna'), 1, 0)

site.cw[order(site.cw$Air_Site),]

#add region to site.cw
site.cw <- site.cw %>% 
  mutate(region = ifelse(Site %in% c('NF Campbell', 'Rabbit', 'Ship'), "Anchorage",
                         ifelse(Site %in% c('Alexander', 'Chuitna', 'McNeil', 'Theodore'), "Westside",
                                ifelse(Site %in% c('Anchor', 'Beaver', 'Crooked', 'Deep', 'Funny',
                                                   'Kenai R at Soldotna', 'Moose', 'Ninilchik', 'Quartz', 'Resurrection',
                                                   'Russian', 'Stariski', 'Slikok', 'Soldotna'), "Kenai", "Mat-Su"))))

# 5. Weekly data frames  -----------------------------------------------------

weekCount <- str.dat3 %>% 
  group_by(Site,Year,Week) %>% 
  summarize(weekCt = length(Day))

#create names of weeks with 5 or more days for calculating mean
str.dat4 <- merge(str.dat3, weekCount)
str.wk <- str.dat4 %>% 
  filter(weekCt > 4) %>% 
  group_by(Site, Week, Year) %>% 
  summarize(meanStrTemp = mean(Temperature), maxDailyTemp = max(Temperature))

#examining difference between maximum daily temperature and mean weekly temperature for each week (based on daily means).
#About a 1 degree difference in maximum daily temps vs. mean weekly temps.
with(str.wk, plot(meanStrTemp,maxDailyTemp))
with(str.wk, hist(maxDailyTemp - meanStrTemp, nclass = 30))
ggplot(str.wk, aes(maxDailyTemp - meanStrTemp)) +
  geom_histogram() +
  facet_wrap(~Site, scales = "free_y")

ggplot(str.wk, aes(Week, meanStrTemp, color = Year)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site)

#relationship between mean daily temps and mean weekly temps.
left_join(str.wk, str.dat4) %>%
  ggplot(aes(x = Temperature, y = meanStrTemp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE, size = 3, label.x.npc = 0,
               label.y.npc = 0.9, hjust = 0) +
  labs(x = "Mean daily temperatures", y = "Mean weekly temperatures") +
  facet_wrap(~Site)

#Note most are very closely related, except for Little Susitna.
left_join(str.wk, str.dat4) %>%
  group_by(Site) %>%
  do(tidy(lm(data = ., meanStrTemp ~ Temperature))) %>%
  filter(term == "Temperature") %>%
  arrange(term, desc(estimate)) %>%
  print(n = 76)

left_join(str.wk, str.dat4) %>%
  group_by(Site) %>%
  do(glance(lm(data = ., meanStrTemp ~ Temperature))) %>%
  print(n = 38)

# 6. Predictions -------------------------------------------------------------

#Need to trim to May-Sept so not modeling non-linear winter temps for Kenai sites.
week(as.Date("05-01-2015", format = "%m-%d-%Y")) #18, starts on 4/30 on non-leap years
week(as.Date("09-30-2015", format = "%m-%d-%Y")) #39, ends on 9/30 exactly on non-leap years

#added in observed v predicted lm fits, but r2 are exactly the 
# same as for the original model. Not totally sure why, except that 
# when plotting predictions, they are simple offsets from observations 
# based on linear adjustments so r2 will always be the same.

predictions <- data.frame()
model.list <- list()

#Removed code that creates a plot of weekly air and stream temperatures for each site.
# Can uncomment those lines for a multi-page pdf of data for each site.

# pdf("Temperature plots.pdf")
for(i in stream.sites){
  dat <- str.wk[str.wk$Site == i & str.wk$Week %in% 18:40,]   # captures May-Sept
  weeks <- paste(dat$Year, dat$Week)
  air.st <- site.cw[site.cw$Site == i, "Air_Site"]
  air <- air.wk[air.wk$STATION_NAME==air.st & paste(air.wk$Year,air.wk$Week) %in% weeks,]
  dat<-merge(dat,air)
  
  lm1<-lm(meanStrTemp ~ meanAirTemp, data = dat)
  model.list[[i]]<-lm1
  
  # plot(meanStrTemp ~ meanAirTemp, data = dat, main = i, xlab = paste(unique(STATION_NAME),"Air Temperature"),
  #      ylab = "Stream Temperature")
  # r2label = bquote(italic(R)^2 == .(format(summary(lm1)$r.squared, digits = 3)))
  # legend("topleft", bty="n", legend=r2label)
  # abline(lm1)
  
  preds <- ungroup(air.wk[air.wk$STATION_NAME == air.st & air.wk$Week %in% 18:40,])
  preds$predStrTemp <- predict(lm1, preds %>% select(meanAirTemp))
  preds <- preds %>%
    mutate(Site = i,
           yr.wk = paste(Year, Week))
  predictions <- rbind(predictions, preds)
  
  #observed v. predicted
  # predStreamTemps <- pull(preds[preds$yr.wk %in% weeks, "predStrTemp"])
  # lm2 <- lm(dat$meanStrTemp ~ predStreamTemps)
  # model.OP[[i]] <- lm2
  # 
}
# dev.off()

#Drop Kenai sites that we aren't using.
predictions <- predictions %>% 
  filter(!Site %in% c("Kenai R at Skilak", "Kenai R at Cooper Landing")) 

#merge model output for all sites
temp.models <- lapply(model.list, tidy) %>% plyr::ldply(rbind)
temp.models.r2 <- lapply(model.list, glance) %>% plyr::ldply(rbind)

#arrange model output by worst to best fit.
temp.models.r2 %>% 
  left_join(site.cw %>% select(fish.model, Site), by = c(".id" = "Site")) %>% 
  filter(fish.model == 1) %>% 
  arrange(r.squared)

tempModSum <- temp.models %>% 
  select(.id, term, estimate) %>% 
  spread(key = term, value = estimate) %>%
  left_join(temp.models.r2 %>% select(.id, r.squared, p.value))

tempModSum <- left_join(tempModSum, site.cw %>% select(Site, fish.model), by = c(".id" = "Site")) 

tempModSum %>% filter(fish.model == 1) %>% arrange(desc(r.squared))

#Some max weekly temps seem to be from years without much summer data.  Only use
# weeks from June July August. Weeks 22:35.
week(as.Date("06-01-2015", format = "%m-%d-%Y")) #22, starts on 5/28 on non-leap years
week(as.Date("08-31-2015", format = "%m-%d-%Y")) #35, ends on 9/3 on non-leap years

max.weekly.temps <- predictions %>% 
  filter(Week %in% 22:35) %>% 
  group_by(Site,Year) %>% 
  summarize(maxTemp = max(predStrTemp)) 

ggplot(max.weekly.temps,aes(Year,maxTemp)) +
  # geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  labs(title="Modeled Maximum Weekly Temperatures")

#about 0.3 deg/decade, matches Isaak reconstructed trends
nlme::lme(maxTemp ~ Year, random = ~1|Site, data = max.weekly.temps) %>% 
  summary

#different trends per site
nlme::lmList(maxTemp ~ Year|Site, data = max.weekly.temps, pool=TRUE) %>% plot
with(max.weekly.temps[max.weekly.temps$Site == "Montana",], 
     lm(maxTemp ~ I(Year-1998))) %>% 
  summary

#about 0.28 deg/decade, matches isaak reconstructed trends
nlme::lme(maxTemp~Year,random=~1|Site,data=max.weekly.temps) %>% summary


# 7. Trends in average summer temp - for comparison with Isaak (2011) --------

#Isaak et al. 2011 modeled changes in average seasonal temps and also 
# min WAT and max WAT, which were based on 7d rolling averages.
# Since we hindcasted weekly temps, can only directly compare
# to average summer temp. They used a 75 d min for each season.

predictions %>% 
  group_by(Site, Year) %>% 
  filter (Week %in% 22:35) %>%
  summarize(aveSummT = mean(predStrTemp)) %>%
  ggplot(aes(x = Year, y = aveSummT)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Site) +
  theme_bw()

summTrEq <- predictions %>% 
  group_by(Site, Year) %>% 
  filter (Week %in% 22:35) %>%
  summarize(aveSummT = mean(predStrTemp)) %>%
  ungroup() %>%
  group_by(Site) %>%
  do(tidy(lm(aveSummT ~ Year, data = .)))

summTrR2 <- predictions %>% 
  group_by(Site, Year) %>% 
  filter (Week %in% 22:35) %>%
  summarize(aveSummT = mean(predStrTemp)) %>%
  ungroup() %>%
  group_by(Site) %>%
  do(glance(lm(aveSummT ~ Year, data = .)))

summTrEq <- summTrEq %>%
  filter(term == "Year") 
summTrR2 <- summTrR2 %>%
  select(r.squared)

summTr <- left_join(summTrEq, summTrR2)
summTr <- left_join(summTr, site.cw[,c("Site", "region")])


# Trends in max weekly temp 

maxTrEq <- predictions %>% 
  group_by(Site, Year) %>% 
  filter (Week %in% 22:35, !Site %in% c("Kenai R at Skilak", "Kenai R at Cooper Landing")) %>%
  summarize(maxSummT = max(predStrTemp)) %>%
  ungroup() %>%
  group_by(Site) %>%
  do(tidy(lm(maxSummT ~ Year, data = .)))

maxTrR2 <- predictions %>% 
  group_by(Site, Year) %>% 
  filter (Week %in% 22:35, !Site %in% c("Kenai R at Skilak", "Kenai R at Cooper Landing")) %>%
  summarize(maxSummT = max(predStrTemp)) %>%
  ungroup() %>%
  group_by(Site) %>%
  do(glance(lm(maxSummT ~ Year, data = .)))

maxTrEq <- maxTrEq %>%
  filter(term == "Year") 

maxTrR2 <- maxTrR2 %>%
  select(r.squared)

maxTr <- left_join(maxTrEq, maxTrR2)
maxTr <- left_join(maxTr, site.cw[,c("Site", "region")])

maxTr %>% 
  group_by(region) %>% 
  summarize(mean(estimate),
            sd(estimate))


# 8. Stream temperature covariates  ---------------------------------------------------------

#comparing week numbers for leap and non-leap years.

tibble(dates.leap = seq.Date(as.Date("05-01-2000", format = "%m-%d-%Y"), 
                             as.Date("09-30-2000", format = "%m-%d-%Y"), by = "1 day"),
       dates = seq.Date(as.Date("05-01-2001", format = "%m-%d-%Y"), 
                        as.Date("09-30-2001", format = "%m-%d-%Y"), by = "1 day")) %>% 
  mutate(weeks.leap = week(dates.leap),
         weeks = week(dates)) %>% print(n = 200)

#max weekly temps during initiation of spawning (JA only).
max.spawn.temps <- predictions %>% 
  filter(Week %in% 27:35) %>% 
  group_by(Site,Year) %>% 
  summarize(maxWkJA = max(predStrTemp),
            meanWkJA = mean(predStrTemp)) 

#weekly temps during juvenile rearing (JJA only).
max.juve.temps <- predictions %>% 
  filter(Week %in% 22:35) %>% 
  group_by(Site,Year) %>% 
  summarize(maxWkJJA = max(predStrTemp),
            meanWkJJA = mean(predStrTemp)) 

#all spring and summer, no filter on week.
juveCovar <- predictions %>%
  group_by(Site, Year) %>%
  summarize(wksGT15 = length(which(predStrTemp > 15)),
            wksGT18 = length(which(predStrTemp > 18)),
            cumDD = sum(predStrTemp * 7))

#migration is May - August
migrCovar <- predictions %>%
  group_by(Site, Year) %>%
  filter(Week %in% 18:35) %>%
  summarize(wksGT16 = length(which(predStrTemp > 16)),
            wksGT21 = length(which(predStrTemp > 21)))

#spawning is July through September.
spawnCovar <- predictions %>%
  group_by(Site, Year) %>%
  filter(Week %in% 27:39) %>%
  summarize(wksGT13 = length(which(predStrTemp > 13)),
            wksGT10 = length(which(predStrTemp > 10)))

#merge all covariates into one table.
allCovars <- left_join(max.spawn.temps, max.juve.temps)
allCovars <- left_join(allCovars, juveCovar)
allCovars <- left_join(allCovars, migrCovar)
allCovars <- left_join(allCovars, spawnCovar)

#add the perName from the site.cw so it is present in both precip and temp data frames.
allCovars <- left_join(allCovars, site.cw %>% select(Site, fish.model)) 


panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}

pairs(allCovars[, 3:13], lower.panel=panel.smooth, upper.panel=panel.cor)

# 9. Read in precipitation data ----------------------------------------------

# These metrics were calculated in GIS by Leslie Jones, ACCS.

ason <- read.csv("data/Precipitation_and_discharge/ASON_metrics.csv", stringsAsFactors = FALSE) 
mjja <- read.csv("data/Precipitation_and_discharge/MJJA_metrics.csv", stringsAsFactors = FALSE) 

precip <- left_join(ason, mjja) %>% gather(key = "Covariate", value = "Value", -year, -siteID)

prSN <- precip %>%
  distinct(siteID) %>%
  mutate(capsiteID = toTitleCase(siteID),
         Site = ifelse(capsiteID == "EF_chulit", "EF Chulitna",
                       ifelse(capsiteID == "Campbell", "NF Campbell",
                              ifelse(capsiteID == "Kenai_lat", "Kenai R",
                                     ifelse(capsiteID == "Little_su", "Little Susitna",
                                            ifelse(capsiteID == "Little_wi", "Little Willow", capsiteID)))))) 

precip <- left_join(precip, prSN %>% select(siteID, Site))


# 10. Read in discharge data ---------------------------------------------------------------

#Data were calculated by Leslie Jones, ACCS from Little Susitna gage data.
rb <- read_excel("data/Precipitation_and_discharge/RBIndex_LSusitna_metrics.xlsx")

#convert to wide form and add in little su flow metrics for pairplot
precip %>%
  spread(key = Covariate, value = Value) %>%
  left_join(rb) %>% 
  pairs(~ASON_avg + ASON_max + MJJA_avg + MJJA_max + RB_ASON + RB_MJ + MDIS_MJJA, ., lower.panel=panel.smooth, upper.panel=panel.cor)

# 11. Combined all covariates and save output for modeling--------

#convert precip to wide form and and merge with little su flow metrics.
prpWd <- precip %>%
  select(-siteID) %>% 
  spread(key = Covariate, value = Value) %>%
  left_join(rb) %>% 
  rename("Year" = "year") %>% 
  mutate(Site = case_when(Site == "Kenai R" ~ "Kenai R at Soldotna",
                          TRUE ~ Site))

#Filtering stream temperature covariates in allCovars to just 15 sites
# that are in the productivity model.
covars <- allCovars %>%
  ungroup() %>% 
  filter(fish.model == 1) %>% 
  select(-fish.model) %>% 
  left_join(prpWd) %>% 
  mutate(Population = case_when(Site == "EF Chulitna" ~ "Chulitna",
                                Site == "Kenai R at Soldotna" ~ "Kenai late run",
                                Site == "NF Campbell" ~ "Campbell",
                                TRUE ~ Site)) 

#Check that site names match what Erik has.
sr <- read_csv("data/SpawnersRecruits.csv")
unique(covars$Population)[unique(covars$Population) %in% unique(sr$Population)]

write.csv(covars, "data/covars.csv")


# 12. Manuscript figures --------------------------------------------------

# Figure S4. Mean summer temperature trends 

summer.temps <- predictions %>% 
  filter(Week %in% 22:35, !Site %in% c("Kenai R at Skilak", "Kenai R at Cooper Landing")) %>% 
  group_by(Site, Year) %>% 
  summarize(meanTemp = mean(predStrTemp)) %>%
  left_join(site.cw[,c("Site", "region", "fish.model")]) %>%
  transform(Site = as.factor(plyr::revalue(Site, c("Kenai R at Soldotna" = "Kenai R")))) %>%
  mutate(reg.site = paste(region, Site, sep = ": "))

#add population field so names match ms.
summer.temps <- summer.temps %>% 
  mutate(Population = case_when(Site == "EF Chulitna" ~ "Chulitna",
                                Site == "Kenai R" ~ "Kenai late run",
                                Site == "NF Campbell" ~ "Campbell",
                                TRUE ~ as.character(Site))) 

formula <- y ~ x

ggplot(summer.temps %>% filter(fish.model == 1), aes(x = Year, y = meanTemp)) +
  geom_line() +
  geom_smooth(method = "lm") +
  stat_fit_tidy(method = "lm",
                method.args = list(formula = formula),
                mapping = aes(label = paste0("slope = ", signif(..x_estimate.., digits = 2))),
                label.x = 'left',
                label.y = 0.1,
                size = 3) +
  facet_wrap(~Population) +
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010), labels = c("'80", "'90", "'00", "'10")) +
  theme_bw() +
  theme(strip.text = element_text(size = 7)) +
  ylim(4, 18) +
  labs(y = expression("Modeled Mean Summer Temperature (˚C)"), x = "Year")

ggsave("figs/Fig S4. Modeled summer temps.pdf", width = 7, height = 7, units = "in")

# Figure S5. Maximum temperature during spawning 

#Identify years where majority of streams had higher than normal temperatures
# during spawning (> 1 sd from long-term mean).
covars %>% 
  group_by(Site) %>% 
  mutate(stdMaxWkJA = scale(maxWkJA),
         mean2 = mean(maxWkJA),
         sd2 = sd(maxWkJA)) %>% 
  group_by(Year) %>% 
  summarize(ct1sd = sum(stdMaxWkJA > 1)) %>% 
  print(n=37)

#which sites are below 1 sd from mean for 1997, 2003, 2004, and 2013?
covars %>% 
  group_by(Site) %>% 
  mutate(stdMaxWkJA = scale(maxWkJA)) %>% 
  filter(Year %in% c(1997, 2003, 2004, 2013), stdMaxWkJA <= 1) %>% 
  select(Site, Year, stdMaxWkJA) %>% arrange(Year)


ggplot() +
  geom_line(aes(x = Year, y = maxWkJA), data = covars) +
  geom_point(aes(x = 1997, y = maxWkJA), 
             data = covars %>% filter(Year == 1997,
                                      !Site %in% c("Anchor", "Chuitna", "Deep", "NF Campbell", "Ninilchik", "Theodore")), 
             shape = 21) +
  geom_point(aes(x = 2003, y = maxWkJA), 
             data = covars %>% filter(Year == 2003), shape = 21) +
  geom_point(aes(x = 2004, y = maxWkJA), 
             data = covars %>% filter(Year == 2004), shape = 21) +
  geom_point(aes(x = 2013, y = maxWkJA), 
             data = covars %>% filter(Year == 2013, !Site %in% c("Kenai R at Soldotna", "Crooked")), shape = 21) +
  facet_wrap(~ Population) +
  labs(y = "Stream Temperature (˚C)") +
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010), labels = c("'80", "'90", "'00", "'10")) +
  theme_bw() + 
  geom_hline(yintercept = 13, linetype="dashed",color="red") +
  theme(legend.position = "bottom") 

ggsave("figs/Fig S5.maxT_spawn.pdf", width = 7, height = 7, units = "in")

# Figure S6. Mean summer temperatures during rearing 

#Identify years where majority of streams had higher than normal temperatures
# during rearing (> 1 sd from long-term mean).
covars %>% 
  group_by(Site) %>% 
  mutate(stdMeanWkJJA = scale(meanWkJJA)) %>% 
  group_by(Year) %>% 
  summarize(ct1sd = sum(stdMeanWkJJA > 1)) %>% 
  print(n=37)

#which sites are below 1 sd from mean for 1997, 2004, 2005, 2013, and 2016?
covars %>% 
  group_by(Site) %>% 
  mutate(stdMeanWkJJA = scale(meanWkJJA)) %>% 
  filter(Year %in% c(1997, 2004, 2005, 2013, 2016), stdMeanWkJJA <= 1) %>% 
  select(Site, Year, stdMeanWkJJA) %>% arrange(Year)

ggplot() +
  geom_line(aes(x = Year, y = meanWkJJA), data = covars) +
  geom_point(aes(x = 1997, y = meanWkJJA), 
             data = covars %>% filter(Year == 1997,
                                         !Site %in% c("Chuitna", "NF Campbell", "Theodore")), shape = 21) +
  geom_point(aes(x = 2004, y = meanWkJJA), 
             data = covars %>% filter(Year == 2004), shape = 21) +
  geom_point(aes(x = 2005, y = meanWkJJA), 
             data = covars %>% filter(Year == 2005,
                                         !Site %in% c("Kenai R at Soldotna", "Crooked", "Chuitna", "NF Campbell", "Theodore")), shape = 21) +
  geom_point(aes(x = 2013, y = meanWkJJA), 
             data = covars %>% filter(Year == 2013, 
                                         !Site %in% c("Kenai R at Soldotna", "Crooked")), shape = 21) +
  geom_point(aes(x = 2016, y = meanWkJJA), 
             data = covars %>% filter(Year == 2016), shape = 21) +
  geom_hline(yintercept=15, linetype="dashed",color="red") +
  facet_wrap(~Population) +
  labs(y = "Stream Temperature (˚C)") +
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010), labels = c("'80", "'90", "'00", "'10")) +
  scale_y_continuous(breaks = c(5,10,15, 20), labels = c("5", "10", "15", "20")) +
  theme_bw() + 
  theme(legend.position = "bottom") 

ggsave("figs/Fig S6.avgT_grow.pdf", width = 7, height = 7, units = "in")

# Figure S7. Maximum monthly precipitation during spawning 

#Identify years where majority of streams had higher than normal monthly precipitation
# during spawning and early incubation (> 1 sd from long-term mean).
covars %>% 
  group_by(Site) %>% 
  filter(!is.na(ASON_max)) %>% 
  mutate(stdASON_max = scale(ASON_max)) %>% 
  group_by(Year) %>% 
  summarize(ct1sd = sum(stdASON_max > 1)) %>% 
  print(n=37)

#which sites are below 1 sd from mean for 1982, 1990, 1993, 2004, and 2006?
covars %>% 
  group_by(Site) %>% 
  filter(!is.na(ASON_max)) %>% 
  mutate(stdASON_max = scale(ASON_max)) %>% 
  filter(Year %in% c(1982, 1990, 1993, 2004, 2005, 2006), stdASON_max <= 1) %>% 
  select(Site, Year, stdASON_max) %>% arrange(Year)

ason_max_means <- covars %>% 
  group_by(Site, Population) %>% 
  summarize(meanP = mean(ASON_max, na.rm = TRUE))

ggplot() +
  geom_line(aes(x = Year, y = ASON_max), data = covars) +
  geom_point(aes(x = 1982, y = ASON_max), 
             data = covars %>% filter(Year == 1982, !Site %in% c("EF Chulitna", "Little Susitna", "Little Willow", "NF Campbell", "Willow")), shape = 21) +
  geom_point(aes(x = 1990, y = ASON_max), 
             data = covars %>% filter(Year == 1990), shape = 21) +
  geom_point(aes(x = 1993, y = ASON_max), 
             data = covars %>% filter(Year == 1993, !Site %in% c("Anchor", "Deep", "Kenai R at Soldotna")), shape = 21) +
  geom_point(aes(x = 2004, y = ASON_max), 
             data = covars %>% filter(Year == 2004, !Site == "EF Chulitna"), shape = 21) +
  geom_point(aes(x = 2005, y = ASON_max), 
             data = covars %>% filter(Year == 2005, !Site == "Little Susitna"), shape = 21) +
  geom_point(aes(x = 2006, y = ASON_max), 
             data = covars %>% filter(Year == 2006,
                                      !Site %in% c("Anchor", "Deep", "Kenai R at Soldotna", "Ninilchik")), shape = 21) +
  geom_hline(aes(yintercept = meanP), data = ason_max_means, linetype = "dashed", color = "red") +
  facet_wrap(~ Population) +
  labs(y = "Total Monthly Precipitation (mm)") +
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010), labels = c("'80", "'90", "'00", "'10"),
                     limits = c(1980, 2010)) +
  theme_bw() 

ggsave("figs/Fig S7.maxP_spawn.pdf", width = 7, height = 7, units = "in")

