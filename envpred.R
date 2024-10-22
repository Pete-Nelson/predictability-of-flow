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

# test regularity ----

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

## NO harvest ----
# data error?
harvest_env_pred <-
  env_stats(harvest$flow,
            harvest$date,
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

flow_env_pred <- 
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
flow_env_pred %>% 
  select(site, n_yrs, unbounded_seasonality:colwell_m) %>% 
  print(., n = Inf)

write_csv(flow_env_pred, "results/flow_env_pred.csv")
write_rds(flow_env_pred, "results/flow_env_pred.rds")

# do next -----
# add: USGS site code, river or creek, lat/lng, dammed vs undammed, elevation
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