# Pete Nelson, PhD
# Department of Water Resources
# vertebral counts study

# purpose: compare vertebral counts for juvenile salmon from the San Joaquin River versus Sacramento River

# see also: https://cvpia-osc.github.io/DSMtemperature/index.html
# package that Sadie developed for CVPIA

# You don't already have the package 'EflowStats':
remotes::install_github("doi-usgs/EflowStats@v5.2.0")

# packages
library(readxl)
library(ggplot2)
library(tidyverse)
library(janitor)
library(dataRetrieval)
library(EflowStats)
library(zoo)

# water temps -----
parm_cd <- "00010" # parameter=temp C; documentation uses "parameterCd"

site_sjr <- "11251000" # San Joaquin R below Friant Dam
site_amr <- "11446220" # American R below Folsom Dam
site_but <- "11390000" # Butte Creek near Chico
site_der <- "11383500" # Deer Creek near Vina
site_fth <- "11407000" # Feather R near Oroville

## san joaquin ----

# what site data are available
whatNWISdata(
  siteNumber = site_sjr
) %>% 
  filter(parm_cd == "00010") %>% 
  head()
# some temp begins 1984-09-06; other series much later

# Raw daily data:
raw_sjr <- readNWISdv(
  site_sjr, parm_cd,
  "2016-10-01", "2023-09-30"
) # first available 2016-10-01

sjr <- 
  raw_sjr %>% 
  mutate(date = Date,
         t_daily = X_00010_00003,
         .keep = "unused") %>% 
  select(site_no, date, t_daily)

head(sjr)

## american ----

# what site data are available
whatNWISdata(
  siteNumber = site_amr
) %>% 
  filter(parm_cd == "00010") %>% 
  head()

# Raw daily data:
raw_amr <- readNWISdv(
  site_amr, parm_cd,
  "2016-10-01", "2023-09-30"
)

amr <- 
  raw_amr %>% 
  mutate(date = Date,
         t_daily = X_00010_00003,
         .keep = "unused") %>% 
  select(site_no, date, t_daily)

## butte ----

# what site data are available
whatNWISdata(
  siteNumber = site_but
) %>% 
  filter(parm_cd == "00010") %>% 
  head()

# Raw daily data:
raw_but <- readNWISdv(
  site_but, parm_cd,
  "2016-10-01", "2023-09-30"
) 

but <- 
  raw_but %>% 
  mutate(date = Date,
         t_daily = X_00010_00003,
         .keep = "unused") %>% 
  select(site_no, date, t_daily)

## deer ----

# what site data are available
whatNWISdata(
  siteNumber = site_der
) %>% 
  filter(parm_cd == "00010") %>% 
  head()
# some temp begins 1984-09-06; other series much later

# Raw daily data:
raw_der <- readNWISdv(
  site_der, parm_cd,
  "2016-10-01", "2023-09-30"
) # first available 2016-10-01

der <- 
  raw_der %>% 
  mutate(date = Date,
         t_daily = X_00010_00003,
         .keep = "unused") %>% 
  select(site_no, date, t_daily)

## feather ----

# what site data are available
whatNWISdata(
  siteNumber = site_fth
) %>% 
  filter(parm_cd == "00010") %>% 
  head()
# USGS apparently useless for this site...

# Feather R water temperature data obtained from https://cdec.water.ca.gov/queryTools.html
# CDEC Station FWQ
# downloaded as "FWQ_25.xlsx" with some meta data added
# "2016-10-01 00:00"

# Raw daily data:
raw_fth <- 
  read_excel("data/FWQ_25.xlsx", sheet = "data") %>% 
  clean_names() %>% 
  mutate(date = as_date(force_tz(ymd_hm(date_time), "US/Pacific")),
         t_hourly = (as.numeric(value) - 32) * 5/9) %>% 
  print()

fth <- 
  raw_fth %>% 
  group_by(date) %>% 
  reframe(t_daily = mean(t_hourly, na.rm = T)) %>% 
  mutate(station_no = "FWQ") %>% 
  select(station_no, date, t_daily) %>% 
  print()

## combo -----
# stitch ts data from all 4 together

combo <-
  list(sjr, amr, but, der, fth) %>% 
  reduce(full_join, by = 'date') %>% 
  mutate(date = date,
         sjr = t_daily.x,
         amr = t_daily.y,
         but = t_daily.x.x,
         der = t_daily.y.y,
         fth = t_daily,
         .keep = "none") %>% 
  pivot_longer(.,
               cols = sjr:fth,
               names_to = "site",
               values_to = "t_daily",
               values_drop_na = FALSE)

# graphics -----
## tribs -----
ggplot(sjr, aes(x = date, y = t_daily)) +
  geom_line() +
  geom_smooth() +
  ggtitle("Water Temperature, San Joaquin River") +
  labs(x = "", y = "Degrees C") +
  theme_bw()

ggplot(amr, aes(x = date, y = t_daily)) +
  geom_line() +
  geom_smooth() +
  ggtitle("Water Temperature, American River") +
  labs(x = "", y = "Degrees C") +
  theme_bw()

ggplot(but, aes(x = date, y = t_daily)) +
  geom_line() +
  geom_smooth() +
  ggtitle("Water Temperature, Butte Creek") +
  labs(x = "", y = "Degrees C") +
  theme_bw()

ggplot(der, aes(x = date, y = t_daily)) +
  geom_line() +
  geom_smooth() +
  ggtitle("Water Temperature, Deer Creek") +
  labs(x = "", y = "Degrees C") +
  theme_bw()

ggplot(fth, aes(x = date, y = t_daily)) +
  geom_line() +
  geom_smooth() +
  ggtitle("Water Temperature, Feather River") +
  labs(x = "", y = "Degrees C") +
  theme_bw()

## combos ----
ggplot(combo,
       aes(x = date, y = t_daily, color = site)) +
  geom_smooth() +
  ggtitle("Water Temperature") +
  labs(x = "", y = "Degrees C") +
  theme_bw()

# not very useful
ggplot(combo,
       aes(x = date, y = t_daily, color = site)) +
  # geom_point(position = position_jitter(1, 3), pch = 21) +
  geom_line(aes(y = rollmean(t_daily, 14, na.pad = T))) + # 14-day rolling average
  ggtitle("Water Temperature") +
  labs(x = "", y = "14-day running average, deg C") +
  theme_bw() 

combo %>% 
  filter(site == "fth" | site == "sjr") %>% 
  ggplot(.,
         aes(x = date, y = t_daily, color = site)) +
  geom_smooth() +
  ggtitle("Feather River & San Joaquin River",
          subtitle = "Water Temperature") +
  labs(x = "", y = "Degrees C") +
  theme_bw()

combo %>% 
  filter(site == "fth" | site == "sjr") %>% 
  ggplot(.,
         aes(x = date, y = t_daily, color = site)) +
  geom_line() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 8), se = TRUE) +
  ggtitle("Feather River & San Joaquin River",
          subtitle = "Water Temperature") +
  labs(x = "", y = "Degrees C") +
  theme_bw()

combo %>% 
  filter(site == "fth" | site == "sjr") %>% 
  ggplot(.,
         aes(x = date, y = t_daily, color = site)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 8), se = TRUE) +
  ggtitle("Feather River & San Joaquin River",
          subtitle = "Water Temperature") +
  labs(x = "", y = "Degrees C") +
  theme_bw()

## use this -----
# filtered out some high temp outliers; 
# limited start/finish to put model close to middle
combo %>% 
  filter(site == "fth" | site == "sjr",
         t_daily < 15.5,
         date > "2016-11-01" & date < "2023-07-01") %>% 
  ggplot(.,
         aes(x = date, y = t_daily, color = site)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 8), se = TRUE) +
  ggtitle("Feather River & San Joaquin River",
          subtitle = "Water Temperature") +
  labs(x = "", y = "Degrees C") +
  theme_bw()

# pkg usage -----
vignette("dataRetrieval", package = "dataRetrieval")

readNWISsite(site_amr) # lat lon for site, not much else?

## find sites -----
# useful: https://maps.waterdata.usgs.gov/mapper/index.html

## common parameters -----
#pCode  shortName
#00060  Discharge[ft3/s]
#00065  Gage height[ft]
#00010  Temperature[C]
#00045  Precipitation[in]
#00400  pH

# pkg citation ----
citation(package = "dataRetrieval")