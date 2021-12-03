# load packages 
source("preamble.R")

# Global vars
########################################################

statesList <- c("09","23","25","33","44","50","34","36","42","17","18","26","39","55","19","20","27","29","31","38","46","10","11","12","13","24","37","45","51","54","01","21","28","47","05","22","40","48","04","08","16","30","32","35","49","56","02","06","15","41","53"
)

Sys.setenv(CENSUS_KEY="6ae2ddad490223ade51e50449164c5f1dc628bc1")
QWIName <- "timeseries/qwi/rh"



# countyLevel <- readRDS("data/countyLevelClean.rds") #BW data cleaned up

# Pull census data if it doesn't exist
########################################################
if(!file.exists("data/censusDataByCounty.rds")){ source("censusAPIPull.R") }
# clean up BW data if not done - add FIPS code guesses etc
if(!file.exists("data/BWCountyLevel.rds")){ source("rCode/fipsCodeLabel.R")}
BWDataByCounty <- readRDS(here::here("data/BWCountyLevel.rds"))
# national aggregates for employment
if(!file.exists("data/natlResult.rds")){ source("natlCalcs.R")}
natlResult <- readRDS(here::here("data/natlResult.rds"))
# create our own bartik instrument
if(!file.exists("data/finalBartik.RDS")){ source("replicateBartikInstrument.R")}
bartikOurs <- readRDS("data/finalBartik.RDS")

########################################################