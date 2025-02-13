# Pete Nelson, PhD
# Department of Water Resources
# Predictability of Tributary Flows

# purpose: calculate measures of environmental predictability

# created 22 July 2024

# set up -----

# https://github.com/dbarneche/envPred

# devtools::install_github("dbarneche/envPred")

library(envPred)
library(tidyverse)
library(tsibble)
library(openxlsx)

# test regularity ----
butte_forks <- read_rds("data/butte_forks.rds")
temp <- tsibble(butte_forks,
        key = "site_no",
        index = "date") %>%  
  fill_gaps(.full = TRUE) # regularize ts

# check that ts is regular
temp %>% filter(is.na(flow))

# env predictability ----
## butte ----
butte_env_pred <-
  env_stats(butte$flow,
          butte$date,
          n_states = 11,
          delta = 1,
          is_uneven = TRUE,
          noise_method = "lomb_scargle") %>% 
  as.vector()

# view other related results
names(butte_env_pred)
dput(butte_env_pred)
str(butte_env_pred)
attr(butte_env_pred, "detrended_data")
attr(butte_env_pred, "noise_data")
attr(butte_env_pred, "noise_model")
attr(butte_env_pred, "class")

?envpreddata # see also Marshall & Burgess 2015 and Vasseur & Yodiz 2004

## NO butte_forks ----
# super short ts!

## clear ----
clear_env_pred <-
  env_stats(clear$flow,
            clear$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## colem ----
colem_env_pred <-
  env_stats(colem$flow,
            colem$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## colusa ----
colusa_env_pred <-
  env_stats(colusa$flow,
            colusa$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## deer ----
deer_env_pred <-
  env_stats(deer$flow,
            deer$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## forbes ----
forbes_env_pred <-
  env_stats(forbes$flow,
            forbes$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## freep ----
freep_env_pred <-
  env_stats(freep$flow,
            freep$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## grand_cyn ----
grand_cyn_env_pred <-
  env_stats(grand_cyn$flow,
            grand_cyn$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## grass ----
grass_env_pred <-
  env_stats(grass$flow,
            grass$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## happy_isles ----
happy_isles_env_pred <-
  env_stats(happy_isles$flow,
            happy_isles$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## hetchy ----
hetchy_env_pred <-
  env_stats(hetchy$flow,
            hetchy$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## inskip ----
inskip_env_pred <-
  env_stats(inskip$flow,
            inskip$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## manzanita ----
manzanita_env_pred <-
  env_stats(manzanita$flow,
            manzanita$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## marysv ----
marysv_env_pred <-
  env_stats(marysv$flow,
            marysv$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## mather ----
mather_env_pred <-
  env_stats(mather$flow,
            mather$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## mccld ----
mccld_env_pred <-
  env_stats(mccld$flow,
            mccld$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## merced ----
merced_env_pred <-
  env_stats(merced$flow,
            merced$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## mill ----
mill_env_pred <-
  env_stats(mill$flow,
            mill$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## orov ----
orov_env_pred <-
  env_stats(orov$flow,
            orov$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## pahono ----
pahono_env_pred <-
  env_stats(pahono$flow,
            pahono$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## putah_n ----
putah_n_env_pred <-
  env_stats(putah_n$flow,
            putah_n$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## putah_s ----
putah_s_env_pred <-
  env_stats(putah_s$flow,
            putah_s$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## rbdd ----
rbdd_env_pred <-
  env_stats(rbdd$flow,
            rbdd$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## shasta ----
shasta_env_pred <-
  env_stats(shasta$flow,
            shasta$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## smith ----
smith_env_pred <-
  env_stats(smith$flow,
            smith$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## strawb ----
strawb_env_pred <-
  env_stats(strawb$flow,
            strawb$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## verona ----
verona_env_pred <-
  env_stats(verona$flow,
            verona$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## wildcat ----
wildcat_env_pred <-
  env_stats(wildcat$flow,
            wildcat$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

## xcountry ----
xcountry_env_pred <-
  env_stats(xcountry$flow,
            xcountry$date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle") %>% 
  as.vector()

# combined results -----

trib_flow_env_pred <- 
  bind_rows(butte_env_pred, 
            clear_env_pred,
            colem_env_pred,
            colusa_env_pred,
            deer_env_pred,
            forbes_env_pred,
            freep_env_pred,
            grand_cyn_env_pred,
            grass_env_pred,
            happy_isles_env_pred,
            hetchy_env_pred,
            inskip_env_pred,
            manzanita_env_pred,
            marysv_env_pred,
            mather_env_pred,
            mccld_env_pred,
            merced_env_pred,
            mill_env_pred,
            orov_env_pred,
            pahono_env_pred,
            putah_n_env_pred,
            putah_s_env_pred,
            rbdd_env_pred,
            shasta_env_pred,
            smith_env_pred,
            strawb_env_pred,
            verona_env_pred,
            wildcat_env_pred,
            xcountry_env_pred
            ) %>% 
  add_column(site = c("butte", "clear", "colem", "colusa", "deer", "forbes",
                      "freep", "grand_cyn", "grass", "happy_isles", "hetchy",
                      "inskip", "manzanita", "marysv", "mather", "mccld",
                      "merced", "mill", "orov", "pahono", "putah_n", "putah_s",
                      "rbdd", "shasta", "smith", "strawb", "verona", "wildcat",
                      "xcountry"), 
             .before = "series_n")

# show data from all sites (except for butte_forks & harvest)
trib_flow_env_pred %>% 
  select(site, n_yrs, unbounded_seasonality:colwell_m) %>% 
  print(., n = Inf)

write_csv(trib_flow_env_pred, "data/trib_flow_env_pred.csv")
write_rds(trib_flow_env_pred, "data/trib_flow_env_pred.rds")

# analysis set -----
## data table ----
env_pred <- 
  left_join(trib_flow_env_pred[,c(1,5,13:20)], 
            flow_meta[,c(1,3:8,12,14)],
            join_by(site == site_cd)) %>% 
  mutate(dam = as.factor(dam),
         system_nm = as.factor(system_nm)) %>% 
  arrange(-dec_lat_va) %>% 
  print(n=Inf)

write_rds(env_pred, "data/env_pred.rds")

print(env_pred[,c(1,6:10,12,15:18)], n=Inf)

## drainage area ----
# is drainage area correlated w predictability?
plot(colwell_p ~ drain_area_va, data = env_pred)
mod1p <-
  lm(colwell_p ~ drain_area_va, data = env_pred)
summary(mod1p)

# is drainage area correlated w constancy?
plot(colwell_c ~ drain_area_va, data = env_pred)
mod1c <-
  lm(colwell_c ~ drain_area_va, data = env_pred)
summary(mod1c)

# is drainage area correlated w contingency?
plot(colwell_m ~ drain_area_va, data = env_pred)
mod1m <-
  lm(colwell_m ~ drain_area_va, data = env_pred)
summary(mod1m)

## ANOVAs -----
boxplot(colwell_p ~ dam, data = env_pred)
mod2p <- aov(colwell_p ~ dam, data = env_pred)
summary(mod2p)

boxplot(colwell_c ~ dam, data = env_pred)
mod2c <- aov(colwell_c ~ dam, data = env_pred)
summary(mod2c)

boxplot(colwell_m ~ dam, data = env_pred)
mod2m <- aov(colwell_m ~ dam, data = env_pred)
summary(mod2m)


 # do next -----
# add: river or creek, stream order (river continuum)
# remove: grand_cyn bc only 5 years of data?

# SCRATCH ##############################
temp1 <-
  tsibble(
  date = rep(as.Date("2017-10-01") + 0:4, 2),
  value = rnorm(10),
  site = c(rep("shasta", 5), rep("butte", 5)),
  key = "site",
  index = "date",
  regular =  TRUE) %>% 
  print()

temp2 <- temp1[-c(3,9),] %>% 
  print()

temp2 %>% 
  fill_gaps(.full = TRUE)

temp2 %>% 
  fill_gaps(.start = 2017-09-30,
            .end = 2017-10-07) # why?!

temp3 <-
  temp2 %>% 
  fill_gaps(.full = TRUE)

temp3 %>% filter(is.na(value))

data(sst)
head(sst)
tail(sst)
range(sst$dates) # start 1997-01-01, end 2007-12-31
env_stats(sst$time_series, sst$dates,
          n_states = 11, delta = 1,
          is_uneven = F, interpolate = F,
          show_warns = T,
          noise_method = "spectrum")
temp <-
  sst %>% filter(dates >= "1997-10-01" & dates <= "2007-09-30")
env_stats(temp$time_series, temp$dates,
          n_states = 11, delta = 1,
          is_uneven = F, interpolate = F,
          show_warns = T,
          noise_method = "spectrum")

harvest <- tsibble(
  year = c(2010, 2011, 2013, 2011, 2012, 2014),
  fruit = rep(c("kiwi", "cherry"), each = 3),
  kilo = sample(1:10, size = 6),
  key = fruit, index = year
)
fill_gaps(harvest, .full = TRUE)