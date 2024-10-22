# Pete Nelson, PhD
# Department of Water Resources
# Predictability of Tributary Flows

# purpose: acquire flow data for Sacramento River and tributaries

# created 22 July 2024

# set up -----

library(dataRetrieval) # USGS pkg for retrieving flow data
library(tidyverse) # data handling...

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




# Sacramento River mainstem -----

## Yreka ----
# Sacramento R near Yreka, California
site <- "11517500"

yreka_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "yreka",
            system_nm = "Sacramento R",
            dam = "no") %>% 
  relocate(site_cd, 
           dam,
           system_nm,
           .after = site_no)

yreka <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date) %>% relocate(flow, date)

## Dunsmuir ----
# Sacramento R below Little Castle C near Dunsmuir, California
site <- "11341450"

dunsmuir_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "dunsmuir",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, .after = site_no)

dunsmuir <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date) %>% relocate(flow, date)

## Shasta -----
# above Shasta Lake near Delta, California NOT anywhere close to the real Delta!
site <- "11342000"

shasta_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "shasta",
            system_nm = "Sacramento R",
            dam = "yes") %>% 
  relocate(site_cd, .after = site_no)

shasta <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date) %>% relocate(flow, date)

## Red Bluff -----
# above Bend Bridge above Red Bluff (upstream of Red Bluff & RBDD)
site <- "11377100"

rbdd_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "rbdd") %>% 
  relocate(site_cd, .after = site_no)

rbdd <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

## Colusa ----
# Sacramento River at Colusa CA

site <- "11389500"

colusa_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "rbdd") %>% 
  relocate(site_cd, .after = site_no)

colusa <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

## Verona ----
# Sacramento River at Verona CA

site <- "11425500"

verona_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "rbdd") %>% 
  relocate(site_cd, .after = site_no)

verona <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

## DWSC Freeport ----
# Sacramento River-Deepwater Ship Channel nr Freeport, CA

site <- "11455095"

dwfreep_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "rbdd") %>% 
  relocate(site_cd, .after = site_no)

# no flow data?!

# dwfreep <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

## DWSC Clarksburg ----
# Sacramento River-Deepwater Ship Channel nr Clarksburg, CA

site <- "11455136"

dwclarks_meta <- readNWISsite(site) %>% 
    bind_cols(site_cd = "rbdd") %>% 
    relocate(site_cd, .after = site_no)

# no flow data?!

# dwclarks <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

## Freeport ----
# Sacramento River at Freeport, CA

site <- "11447650"

freep_meta <- readNWISsite(site) %>% 
    bind_cols(site_cd = "rbdd") %>% 
    relocate(site_cd, .after = site_no)

freep <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

## Hood ----
# Sacramento River at Hood, CA

site <- "382205121311300"

hood_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "rbdd") %>% 
  relocate(site_cd, .after = site_no)

hood <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

## Toland ----
# Sacramento River at Toland Landing near Rio Vista

site <- "11455485"

toland_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "rbdd") %>% 
  relocate(site_cd, .after = site_no)

# no flow data!

# toland <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

# Sac R tribs ----

## McCloud River -----
# above Shasta Lake
# site number: 11368000

site <- "11368000"

mccld_meta <- readNWISsite(site) %>% 
    bind_cols(site_cd = "rbdd") %>% 
    relocate(site_cd, .after = site_no)

mccld <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

## Clear Creek -----
# near Igo, California
# site number: 11372000

site <- "11372000"

clear_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "rbdd") %>% 
  relocate(site_cd, .after = site_no)

clear <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

## Battle Creek ----
### Coleman hatchery -----

site <- "11376550"

colem_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "rbdd") %>% 
  relocate(site_cd, .after = site_no)

colem <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

### Wildcat Canyon -----
# N Fork of Battle Creek below Wildcat Canyon, elev=1206'

site <- "11376160"

wildcat_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "rbdd") %>% 
  relocate(site_cd, .after = site_no)

wildcat <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

### Inskip -----
# S Fork Battle Creek below S Power House, elev=1435'

site <- "11376440"

inskip_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "rbdd") %>% 
  relocate(site_cd, .after = site_no)

inskip <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

### Xcountry Cyn -----
# N Fork Battle Creek below diversion to Xcountry Canyon, near Manton, CA, elev 2212'

site <- "11376140"

xcountry_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "rbdd") %>% 
  relocate(site_cd, .after = site_no)

xcountry <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

### Smith Cyn -----
# N Fork Battle below div to Al Smith Canyon near Manton, elev 3800'

site <- "11376040"

smith_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "rbdd") %>% 
  relocate(site_cd, .after = site_no)

smith <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

### Manzanita Lake -----
# N Fork Battle Creek below the dam (N Battle Crk Reservoir), elev 5545'

site <- "11376015"

manzanita_meta <- readNWISsite(site) %>% 
  bind_cols(site_cd = "rbdd") %>% 
  relocate(site_cd, .after = site_no)

manzanita <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

## Mill Creek -----
# site number: 11381500

site <- "11381500"
readNWISsite(site)$station_nm # site meta data

mill <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

## Deer Creek -----
# site number: 11383500

site <- "11383500"
readNWISsite(site)$station_nm # site meta data

deer <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

## Butte Creek ----
# find via https://maps.waterdata.usgs.gov/mapper/index.html

### near Sac R conflu & Chico -----

site <- "11390000"
readNWISsite(site)$station_nm # site meta data

butte <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

### below Forks of Butte ----
# near De Sabla C
# site number: 11389740

site <- "11389740"
readNWISsite(site)$station_nm # site meta data

butte_forks <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

## Putah Creek -----
### Putah N ----

site <- "11454000"
readNWISsite(site)$station_nm # site meta data

putah_n <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

### Putah S -----

site <- "11454210"
readNWISsite(site)$station_nm # site meta data

putah_s <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

## Feather River -----

### Oroville -----

site <- "11407000"
readNWISsite(site)$station_nm # site meta data

orov <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

### Forbestown Dam -----
# SF Feather R below Forbestown Dam

site <- "11396200"
readNWISsite(site)$station_nm # site meta data

forbes <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

### Strawberry Valley -----
# SF Feather R below div dam near Strawberry Valley California

site <- "11395200"
readNWISsite(site)$station_nm # site meta data

strawb <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

### Little Grass Valley dam -----
# SF Feather R below Little Grass Valley Dam, California

site <- "11395030"
readNWISsite(site)$station_nm # site meta data

grass <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

## Yuba River -----

### Marysville -----

site <- "11421000"
readNWISsite(site)$station_nm # site meta data

marysv <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

## Tuolumne R -----
### Tuolumne ----
# just above Hetch Hetchy

site <- "11274790"
readNWISsite(site)$station_nm # site meta data

grand_cyn <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

### Hetch Hetchy ----
# Tuolumne River just below O'Shaunnesy Dam

site <- "11276500"
readNWISsite(site)$station_nm # site meta data

hetchy <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

### Mather ----
# Tuolumne River below O'Shaunnesy near Mather

site <- "11276600"
readNWISsite(site)$station_nm # site meta data

mather <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

## Merced River ----
### Valley -----
# Merced River at Happy Isles Bridge, Yosemite Valley

site <- "11264500"
readNWISsite(site)$station_nm # site meta data

happy_isles <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

### Pohono ----
# Merced R at Pohono Bridge below Yosemite Valley

site <- "11266500"
readNWISsite(site)$station_nm # site meta data

pahono <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)

### MFalls ----
# Merced R below Merced Falls Dam

site <- "11270900"
readNWISsite(site)$station_nm # site meta data

merced <- # raw daily data
  readNWISdv(siteNumber = site, 
             parameterCd = "00060", # discharge, ft3/s
             statCd = "00003") %>% # pulls daily mean only
  mutate(site_no = site_no,
         date = Date,
         flow = X_00060_00003,
         .keep = "none") %>% relocate(flow, date)