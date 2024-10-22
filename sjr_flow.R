# Pete Nelson, PhD
# Department of Water Resources
# Predictability of Tributary Flows

# purpose: acquire flow data for San Joaquin River and tributaries

# created 22 July 2024

# set up -----

library(dataRetrieval) # USGS pkg for retrieving flow data

# finding sites -----
# https://maps.waterdata.usgs.gov/mapper/index.html

# San Joaquin River mainstem -----
## Mammoth ----
# SJR near Mammoth Lakes, above any dam
# 11224000

## Florence -----
# SJR above Hooper Creek, near Florence Lake
# 11230070

## Hooper -----
# SJR below Hooper Creek, near Florence Lake
# 11230215

## Shakeflat ----
# SJR above Shakeflat Creek
# 11234760

## Willow ----
# SJR above Willow Creek
# 11242000

## Auberry -----
# SJR near Auberry, California
# 11246700

## FDam ----
# SJR release site at Friant Dam
# 11250110

## Friant ----
# SJR below Friant Dam
# 11251000

## Hwy41 ----
# SJR below Highway 41
# 11252275

# SJR tribs -----

## Kings ----
### Kings ----
# N Fork Kings River near Cliff Camp
# 11215000

### Balch ----
# N Fork Kings River below Balch Dam
# 11216200

### Dinkey ----
# N Fork Kings River below Dinkey Creek
# 11218400

## Kern -----

### Enos ----
# Kern River at Enos Park Foot Bridge near Bakersfield

enos <- "351813119150601"
(enos_info <- readNWISsite(enos)) # site meta data
whatNWISdata(siteNumber = enos,
             service = "dv",
             statCd = "00003") # determine what data are available
param <- "00060" # specifies desired parameters to retrieve

enos_rawdv <- # raw daily data
  readNWISdv(enos, param)
readNWISpCode(param) # mean daily flow in ft3/s
head(enos_rawdv)

### Democrat ----
# Kern River near Democrat Springs 

demo <- "11192000"
(demo_info <- readNWISsite(demo)) # site meta data
whatNWISdata(siteNumber = demo,
             service = "dv",
             statCd = "00003") # determine what data are available
param <- "00060" # specifies desired parameters to retrieve

demo_rawdv <- # raw daily data
  readNWISdv(demo, param)
readNWISpCode(param) # mean daily flow in ft3/s
head(demo_rawdv)

### Onyx ----
# S Fork of the Kern River near Onyx 
# above Lake Isabella

onyx <- "11189500"
(onyx_info <- readNWISsite(onyx)) # site meta data
whatNWISdata(siteNumber = onyx,
             service = "dv",
             statCd = "00003") # determine what data are available
param <- "00060" # specifies desired parameters to retrieve

onyx_rawdv <- # raw daily data
  readNWISdv(onyx, param)
readNWISpCode(param) # mean daily flow in ft3/s
head(onyx_rawdv)

### cond3 ----
# Kern River Conduit? No 3 Canyon near Kernville 
# above Lake Isabella, may be agricul diversion

cond3 <- "11185500"
(cond3_info <- readNWISsite(cond3)) # site meta data
whatNWISdata(siteNumber = cond3,
             service = "dv",
             statCd = "00003") # determine what data are available
param <- "00060" # specifies desired parameters to retrieve

cond3_rawdv <- # raw daily data
  readNWISdv(cond3, param)
readNWISpCode(param) # mean daily flow in ft3/s
head(cond3_rawdv)

### Kernv ----
# Kern River near Kernville 
# above Lake Isabella

kern <- "11186000"
(kern_info <- readNWISsite(kern)) # site meta data
whatNWISdata(siteNumber = kern,
             service = "dv",
             statCd = "00003") # determine what data are available
param <- "00060" # specifies desired parameters to retrieve

kern_rawdv <- # raw daily data
  readNWISdv(kern, param)
readNWISpCode(param) # mean daily flow in ft3/s
head(kern_rawdv)