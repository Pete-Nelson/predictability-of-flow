# Pete Nelson, PhD
# Department of Water Resources
# Predictability of Tributary Flows

# purpose: set up for fep work (fep=flow environmental predictability)

# created 20 December 2023

# set up for work ####

library(tidyverse)
library(readxl)
library(dataRetrieval) # USGS pkg for retrieving flow data

# The dataRetrieval package was created to simplify the process of loading hydrologic data into the R environment. It is designed to retrieve the major data types of U.S. Geological Survey (USGS) hydrology data that are available on the Web, as well as data from the Water Quality Portal (WQP), which currently houses water quality data from the Environmental Protection Agency (EPA), U.S. Department of Agriculture (USDA), and USGS. Direct USGS data is obtained from a service called the National Water Information System (NWIS). From https://doi-usgs.github.io/dataRetrieval/

# tutorial info ----------
# For complete tutorial information, see:
# https://rconnect.usgs.gov/dataRetrieval/
# https://waterdata.usgs.gov/blog/dataretrieval/

# basic tutorial -----
# https://doi-usgs.github.io/dataRetrieval/articles/tutorial.html

vignette("dataRetrieval", package = "dataRetrieval")

## workflow ex ----
# Choptank River near Greensboro, MD
# find via https://maps.waterdata.usgs.gov/mapper/index.html
siteNumber <- "01491000"
ChoptankInfo <- readNWISsite(siteNumber) # site meta data, here Choptank River
parameterCd <- "00060" # specifies desired parameters to retrieve

# raw daily data:
rawDailyData <- readNWISdv(siteNumber, parameterCd, "1980-01-01", "2010-01-01")

pCode <- readNWISpCode(parameterCd)
  
# dataRetrieval Intro 1 -----
# https://rconnect.usgs.gov/NMC_dataRetrieval_1

# dataRetrieval Intro 2 ------
# https://rconnect.usgs.gov/NMC_dataRetrieval_2

# still need help? -----
# post an issue on https://github.com/USGS-R/dataRetrieval/issues