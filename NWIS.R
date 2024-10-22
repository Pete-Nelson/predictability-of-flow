# Pete Nelson, PhD
# Department of Water Resources
# Explore USBS data with library dataRetrieval

# created 29 July 2024

# set up -----

library(dataRetrieval) # USGS pkg for retrieving flow data
library(tidyverse) # data handling...

# Provides access to a wide variety of data, from several sources (not just NWIS data).

# Help for parameter codes: https://help.waterdata.usgs.gov/codes-and-parameters/

# Sacramento River near Delta, CA, above Shasta Lake
siteNumber <- "11342000"
deltaInfo <- readNWISsite(siteNumber)
parameterCd <- "00060" # discharge ft3/s

# raw daily data
rawDaily <- readNWISdv(            # didn't work w readNWISqw()
  siteNumber, parameterCd,
  "1980-01-01", "2010-01-01")      # start/end dates
head(rawDaily)
tail(rawDaily)

# show text description of parameter as well as the units used:
pCode <- readNWISpCode(parameterCd) 
