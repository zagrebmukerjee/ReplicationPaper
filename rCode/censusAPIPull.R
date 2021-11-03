library(censusapi)


Sys.setenv(CENSUS_KEY="6ae2ddad490223ade51e50449164c5f1dc628bc1")
readRenviron("~/.Renviron")


apis <- listCensusApis() %>%  tibble()


apis %>% filter(grepl('QWI', title))
QWIName <- "timeseries/qwi/rh"

listCensusMetadata(
  name = QWIName, 
  type = "geography")

varnames <- listCensusMetadata(
  name = QWIName, 
  type = "variables")$name

getCensus(name = QWIName, vars = c("SepSnx"), region = "state:25")

