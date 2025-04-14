# Pete Nelson, PhD
# Department of Water Resources
# Predictability of Tributary Flows

# purpose: acquire flow data for Sacramento River and tributaries

# created 22 July 2024

# set up -----

library(dataRetrieval) # USGS pkg for retrieving flow data
library(tidyverse) # data handling...
library(zoo) # working w time series

# finding sites -----
# https://maps.waterdata.usgs.gov/mapper/index.html

## site/data availability -----
# general example
whatNWISdata(siteNumber = "11342000")
# specific example
whatNWISdata(siteNumber = "11342000",
             service = "dv",
             parameterCd = "00060")

## obtain site info ----
# how to get site details, example: 'shasta' Sac R above Shasta Lake, '11342000'
readNWISsite(11342000)
# all data available; selected useful columns
# what are the data_type_cd=qw data?! getting phased out, but possibly instantaneous data...
whatNWISdata(siteNumber = 11342000) %>% 
  select(c(2:3,5:6,13:15,19,22:24))
whatNWISdata(siteNumber = "11342000",
             service = "dv",
             parameterCd = "00010") # water temperature
whatNWISdata(siteNumber = "11342000",
             service = "dv",        # daily value
             parameterCd = "00060") # flow
readNWISdata(siteNumber = "11342000", parameterCd = "00010")
head(readNWISdv(siteNumber = "11342000", parameterCd = "00060"))

## parameters -----
names(parameterCdFile)
# ?parameterCdFile 
glimpse(parameterCdFile)
parameterCdFile[1:9, c(1, 3, 6)]
pcode_to_name(parameterCd = "all") %>% 
  select(1,3:4,8) # column 2 has the detailed description

site <- "11342000"
readNWISsite(site)$station_nm # site name

### statCD ------
readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") # pulls daily mean only

# statCd = c("00001","00003") gets you mean and max...IF there is a mean and max!?!

# precipitation, flow, and temperature in one go...
pft <- c("00045", # precip, total inches
         "00060", # discharge ft3/s
         "00010") # water temp, deg C

pcode_to_name(parameterCd = pft)

# "00010" Temperature, water, degrees Celsius
# "00011" Temperature, water, degrees Fahrenheit
# "00020" Temperature, air, degrees Celsius
# "00021" Temperature, air, degrees Fahrenheit
# "00025" Barometric pressure, millimeters of mercury
# "00030" Incident solar radiation intensity, calories per square centimeter per day
# "00042" Altitude, feet above mean sea level [value?]
# "00045" Precipitation, total, inches [annual cumulative?]
# "00060" Discharge, cubic feet per second
# "00200" Incident light intensity, 400-700 nanometers, microeinsteins per square meter per second
# "00201" Incident light, daily total, 400-700 nanometers, microeinsteins per square meter
# "00434" pH adjusted to 25 degrees Celsius, water, laboratory, standard units
# "00480" Salinity, water, unfiltered, parts per thousand

## calculations -----
# degree-days: calculate the monthly cumulative degree-days or the mean monthly degree days to get at the possible thermal stress experienced by fishes

## data availability ----
# as of 22 October 2024, these sites didn't appear to have data
# Dunsmuir, DWSC Freeport, DWSC Clarksburg, Hood, Toland


# Sacramento River mainstem -----

## Dunsmuir ----
# Sacramento R below Little Castle C near Dunsmuir, California
# site has only 11 data points, total
site <- "11341450"

## Delta ----
# Sacramento R at Delta, California
# 
site <- "11342000"

delta_meta <-
  readNWISsite(site) %>% 
  bind_cols(site_cd = "delta",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

delta <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow) %>% 
  relocate(date, flow)

## Red Bluff -----
# above Bend Bridge above Red Bluff (upstream of Red Bluff & RBDD)
site <- "11377100"

rbdd_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "rbdd",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no)

rbdd <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

# save Red Bluff flow data for visualization
write_rds(rbdd, "data/rbdd.rds")

## Colusa ----
# Sacramento River at Colusa CA

site <- "11389500"

colusa_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "colusa",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no)

colusa <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## Verona ----
# Sacramento River at Verona CA

site <- "11425500"

verona_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "verona",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no)

verona <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## DWSC Freeport ----
# Sacramento River-Deepwater Ship Channel nr Freeport, CA
# as of 22 Oct 2024, site doesn't seem to have data--gets error message

site <- "11455095"

dwfreep_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "dwfreep",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

# no flow data?!

# dwfreep <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## DWSC Clarksburg ----
# Sacramento River-Deepwater Ship Channel nr Clarksburg, CA
# as of 22 Oct 2024, site doesn't seem to have data--gets error message

site <- "11455136"

dwclarks_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "dwclarks",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

# dwclarks <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## Freeport ----
# Sacramento River at Freeport, CA

site <- "11447650"

freep_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "freep",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

freep <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## Hood ----
# Sacramento River at Hood, CA
# as of 22 Oct 2024, site doesn't seem to have data--gets error message

site <- "382205121311300"

hood_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "hood",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no)

hood <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## Toland ----
# Sacramento River at Toland Landing near Rio Vista
# as of 22 Oct 2024, site doesn't seem to have data--gets error message

site <- "11455485"

toland_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "toland",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no)

# no flow data!

# toland <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)
  
## !DCC ----
# Sacramento River just above Delta Cross Channel
  
site <- "11455485"
  
dcc_meta <- readNWISsite(site) %>% 
    bind_cols(site_cd = "dcc",
              system_nm = "Sacramento R",
              dam = "yes") %>% 
    relocate(site_cd, 
             dam,
             system_nm,
             .after = site_no)
  
  dcc <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
    mutate(site_no = site_no,
           date = Date,
           flow = X_00060_00003,
           .keep = "none") %>% relocate(date, flow)

# Sac R tribs ----

## McCloud River -----
# above Shasta Lake
# site number: 11368000

site <- "11368000"

mccld_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "mccld",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no)

mccld <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## Clear Creek -----
# near Igo, California
# site number: 11372000

site <- "11372000"

clear_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "clear",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

clear <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## Battle Creek ----
### Coleman hatchery -----

site <- "11376550"

colem_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "colem",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

colem <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### Wildcat Canyon -----
# N Fork of Battle Creek below Wildcat Canyon, elev=1206'

site <- "11376160"

wildcat_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "wildcat",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

wildcat <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### Inskip -----
# S Fork Battle Creek below S Power House, elev=1435'

site <- "11376440"

inskip_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "inskip",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

inskip <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### Xcountry Cyn -----
# N Fork Battle Creek below diversion to Xcountry Canyon, near Manton, CA, elev 2212'

site <- "11376140"

xcountry_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "xcountry",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

xcountry <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### Smith Cyn -----
# N Fork Battle below div to Al Smith Canyon near Manton, elev 3800'

site <- "11376040"

smith_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "smith",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

smith <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### Manzanita Lake -----
# N Fork Battle Creek below the dam (N Battle Crk Reservoir), elev 5545'

site <- "11376015"

manzanita_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "manzanita",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

manzanita <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## Mill Creek -----
# site number: 11381500

site <- "11381500"

mill_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "mill",
            system_nm = "Sacramento R",
            dam = "no") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

mill <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## Deer Creek -----
# site number: 11383500

site <- "11383500"

deer_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "deer",
            system_nm = "Sacramento R",
            dam = "no") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

deer <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## Butte Creek ----
# find via https://maps.waterdata.usgs.gov/mapper/index.html

### near Sac R conflu & Chico -----

site <- "11390000"

butte_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "butte",
            system_nm = "Sacramento R",
            dam = "no") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

butte <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### below Forks of Butte ----
# near De Sabla C
# site number: 11389740
# severely deficient dataset

site <- "11389740"

butte_forks_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "butte_forks",
            system_nm = "Sacramento R",
            dam = "no") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

butte_forks <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)
write_rds(butte_forks, "data/butte_forks.rds")

## Putah Creek -----
### Putah N ----

site <- "11454000"

putah_n_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "putah_n",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

putah_n <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

# save Putah Crk N flow data for visualization
write_rds(putah_n, "data/putah_n.rds")

### Putah S -----

site <- "11454210"

putah_s_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "putah_s",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

putah_s <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

# save Putah Crk S flow data for visualization
write_rds(putah_s, "data/putah_s.rds")

## Feather River -----

### Oroville -----

site <- "11407000"

orov_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "orov",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

orov <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### Forbestown Dam -----
# SF Feather R below Forbestown Dam

site <- "11396200"

forbes_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "forbes",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

forbes <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### Strawberry Valley -----
# SF Feather R below div dam near Strawberry Valley California

site <- "11395200"

strawb_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "strawb",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

strawb <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### Little Grass Valley dam -----
# SF Feather R below Little Grass Valley Dam, California

site <- "11395030"

grass_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "grass",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

grass <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## Yuba River -----

### Marysville -----

site <- "11421000"

marysv_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "marysv",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

marysv <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## Consumnes R -----
### !Consumnes -----
# at Michigan Bar, no "major" dams, but diversions thought to limit salmon
# to the river below Rancho Murieta

site <- "11335000"

consumn_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "consumn",
            system_nm = "Sacramento R",
            dam = "no") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

consumn <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

# Mokelumne R -----
## !Mokelumne NF -----
# just below Salt Springs Dam

site <- "11314500"

mokel_nf_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "mokel_nf",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

mokel_nf <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## !Mokelumne NF2 -----
# near West Point

site <- "11316700"

mokel_nf2_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "mokel_nf2",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

mokel_nf2 <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## !Mokelumne MF -----
# at West Point; doesn't appear to be any major dams upstream (some diversions)

site <- "11317000"

mokel_mf_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "mokel_mf",
            system_nm = "Sacramento R",
            dam = "no") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

mokel_mf <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## !Mokelumne SF -----
# near West Point; doesn't appear to be any major dams upstream--
# possibly one of the less affected systems

site <- "11318500"

mokel_sf_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "mokel_sf",
            system_nm = "Sacramento R",
            dam = "no") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

mokel_sf <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## !Mokelumne -----
# mainstem above Comanche & Pardee reservoirs, near Mokelumne Hill (town)
#? near West Point (comparatively high elevation)

site <- "11319500"

mokel_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "mokel",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

mokel <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## !Mokelumne2 -----
# mainstem at Lodi/Woodbridge below Pardee & Comanchee reservoirs

site <- "11325500"

mokel2_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "mokel2",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

mokel2 <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

mokelumne_meta <-  
  bind_rows(mokel_nf_meta,
            mokel_nf2_meta,
            mokel_mf_meta,
            mokel_sf_meta,
            mokel_meta,
            mokel2_meta
  ) %>% 
  relocate(site_cd) %>% 
  select(site_cd:station_nm, dec_lat_va, dec_long_va, coord_datum_cd, county_cd,
         land_net_ds, alt_va, alt_datum_cd, drain_area_va, local_time_fg)

write_rds(mokelumne_meta, "data/mokelumne_meta.rds")

# merge all mokelumne flow ts & save as zoo object
mokelumne_flow

temp <- lst(mokel, mokel2, mokel_mf, mokel_nf, mokel_nf2, mokel_sf)

mokelumne_flow <-
  temp %>% 
  lapply(read.zoo) %>%        # create list of zoo objects
  do.call(what = "merge") %>% # merge objects
  fortify.zoo(name = "date")  # converts zoo to data frame

write_rds(mokelumne_flow, "data/mokelumne_flow.rds")

env_stats(butte$flow,
          butte$date,
          n_states = 11,
          delta = 1,
          is_uneven = TRUE,
          noise_method = "lomb_scargle")

mokelumne_flow[,1:2] %>% na.trim(.) %>% str()

mokelumne_flow[,1:2] %>% na.trim(.) %>% 
  env_stats(as.numeric(mokelumne_flow$flow.mokel),
            mokelumne_flow$date,
            n_states = 97,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle")

mokelumne_flow$flow.mokel_nf %>% 
  fortify.zoo(name = "date") %>% 
  na.trim(.) %>% 
  env_stats(., dates = "date")

mokelumne_flow$flow.mokel_nf %>% 
  na.trim(.) %>% 
  env_stats(.)

na.trim(mokelumne_flow[1:3650,1])

mok_envpred <- 
  mokelumne_flow[,1] %>% 
  fortify.zoo(name = "date") %>% 
  na.trim(.) %>% 
  env_stats(., date,
            n_states = 11,
            delta = 1,
            is_uneven = TRUE,
            noise_method = "lomb_scargle")



# San Joaquin R ----

### !Prisoners -----
# lower SJR

site <- "11313460"

prisoners_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "prisoners",
            system_nm = "San Joaquin R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

prisoners <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)


### !Buckley -----
# lower SJR near Buckley

site <- "375841121225601"

buckley_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "buckley",
            system_nm = "San Joaquin R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

buckley <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### !Stockton -----
# lower SJR near Stockton

site <- "11304810"

stockton_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "stockton",
            system_nm = "San Joaquin R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

stockton <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### !Vernalis -----
# lower SJR near Vernalis

site <- "11303500"

vernalis_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "vernalis",
            system_nm = "San Joaquin R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

vernalis <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)


# SJR tribs ----

## Stanislaus NF ----
### !utica ----
# north fork of the Stanislaus below Utica Reservoir

site <- "11293372"

utica_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "utica",
            system_nm = "San Joaquin R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

utica <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### !avery ----
# north fork of the Stanislaus near Avery

site <- "11294500"

avery_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "avery",
            system_nm = "San Joaquin R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

avery <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### !collierville ----
# Stanislaus R below confluence NF & MF

site <- "11295250"

collierville_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "collierville",
            system_nm = "San Joaquin R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

collierville <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### !black ----
# Black Creek, trib to Stanislaus R, possibly no dam upstream, near Copperopolis

site <- "11299600"

black_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "black",
            system_nm = "San Joaquin R",
            dam = "no") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

black <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### !knights ----
# Stanislaus R below Goodwin Dam, near Knights Ferry

site <- "11300500"

knights_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "knights",
            system_nm = "San Joaquin R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

knights <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### !oakdale ----
# Stanislaus R near Oakdale

site <- "11302500"

oakdale_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "knights",
            system_nm = "San Joaquin R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

oakdale <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## Stanislaus MF ----
### !kennedy ----
# middle fork of the Stanislaus near Kennedy Mdws; possibly no dam

site <- "11292000"

kennedy_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "kennedy",
            system_nm = "San Joaquin R",
            dam = "no") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

kennedy <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### !hellhalf ----
# middle fork of the Stanislaus near Hells Half Acre

site <- "11292700"

hellhalf_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "hellhalf",
            system_nm = "San Joaquin R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

hellhalf <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### !beardsley ----
# middle fork of the Stanislaus below Beardsley Dam

site <- "11292900"

beardsley_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "beardsley",
            system_nm = "San Joaquin R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

beardsley <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## Stanislaus SF -----
### !Stanislaus SF ----
# south fork of the Stanislaus below Strawberr Dam & Pinecrest Lake
# Strawberry (Pinecrest Lake, higher ele) and Lyons (Lyons Reservoir,
# lower ele) dams

site <- "11296500"

stanis_sf_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "stanis_sf",
            system_nm = "San Joaquin R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

stanis_sf <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)



## Tuolumne R -----
### Tuolumne ----
# just above Hetch Hetchy

site <- "11274790"

grand_cyn_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "grand_cyn",
            system_nm = "San Joaquin R",
            dam = "no") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

grand_cyn <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### Hetch Hetchy ----
# Tuolumne River just below O'Shaunnesy Dam

site <- "11276500"

hetchy_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "hetchy",
            system_nm = "San Joaquin R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

hetchy <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### Mather ----
# Tuolumne River below O'Shaunnesy near Mather

site <- "11276600"

mather_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "mather",
            system_nm = "San Joaquin R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

mather <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

## Merced River ----
### Valley -----
# Merced River at Happy Isles Bridge, Yosemite Valley

site <- "11264500"

happy_isles_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "happy_isles",
            system_nm = "San Joaquin R",
            dam = "no") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

happy_isles <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### Pohono ----
# Merced R at Pohono Bridge below Yosemite Valley

site <- "11266500"

pahono_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "pahono",
            system_nm = "San Joaquin R",
            dam = "no") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

pahono <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

### MFalls ----
# Merced R below Merced Falls Dam

site <- "11270900"

merced_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "merced",
            system_nm = "San Joaquin R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  print()

merced <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow)

# Klamath River -----
## Yreka ----
# Klamath R near Yreka, California
site <- "11517500"

yreka_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "yreka",
            system_nm = "Klamath R",
            dam = "yes") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no) %>% 
  mutate(map_scale_fc = as.numeric(map_scale_fc)) %>% 
  print()

yreka <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(date, flow) %>% relocate(date, flow)

# meta data -----
flow_meta <- 
  bind_rows(butte_meta,
            butte_forks_meta,
            clear_meta,
            colem_meta,
            colusa_meta,
            deer_meta,
            dunsmuir_meta,
            dwclarks_meta,
            dwfreep_meta,
            forbes_meta,
            freep_meta,
            grand_cyn_meta,
            grass_meta,
            happy_isles_meta,
            hetchy_meta,
            hood_meta,
            inskip_meta,
            manzanita_meta,
            marysv_meta,
            mather_meta,
            mccld_meta,
            merced_meta,
            mill_meta,
            orov_meta,
            pahono_meta,
            putah_n_meta,
            putah_s_meta,
            rbdd_meta,
            shasta_meta,
            smith_meta,
            strawb_meta,
            toland_meta,
            verona_meta,
            wildcat_meta,
            xcountry_meta,
            yreka_meta
  ) %>% 
  relocate(site_cd) %>% 
  select(site_no:station_nm, dec_lat_va, dec_long_va, coord_datum_cd, county_cd,
         land_net_ds, alt_va, alt_datum_cd, drain_area_va, local_time_fg)

# show data from all sites (except for butte_forks & harvest)
flow_meta

write_csv(flow_meta, "data/flow_meta.csv")
write_rds(flow_meta, "data/flow_meta.rds")
