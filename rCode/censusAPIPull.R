
source("rCode/preamble.R")

listCensusMetadata(
  name = QWIName, 
  type = "geography")

varnames <- listCensusMetadata(
  name = QWIName, 
  type = "variables")

endpointsClean <- listCensusApis() %>%  mutate(type = substr(name,0,3)) %>%  
  filter(type != "acs", type != "ase")


varnamesPop <- listCensusMetadata(name ="pep/population",vintage = 2015, type = "variables") 

# pulls for every county
tic()
totalData <- bind_rows(lapply(
  statesList,function(stateNum){
    getCensus(
      name = QWIName,
      vars = c("race", "ethnicity", "sex", "Emp"),
      region = "county:*",
      seasonadj = "U",
      regionin = paste0("state:", str_pad(stateNum, 2, pad = "0")), time = "from 2011 to 2015") %>% 
      rename(totalEmp = Emp) %>% tibble() }
))

saveRDS(totalData, "data/totalData.rds")
rm(list = c("totalData"))
toc()
tic()

mfgData <- bind_rows(lapply(
  statesList, function(stateNum){
    getCensus(
      name = QWIName,
      vars = c("race", "ethnicity", "sex", "Emp"),
      industry = "31-33",
      ownercode = "A05",
      region = "county:*",
      seasonadj = "U",
      regionin = paste0("state:", str_pad(stateNum, 2, pad = "0")), time = "from 2011 to 2015") %>% 
      rename(mfgEmp = Emp)  %>% tibble()}
))

saveRDS(mfgData, "data/mfgData.rds")
toc()





# population
popDataRaw <- getCensus(
      name = "pep/population",
      vintage = 2015, 
      vars = c("POP"),
      region = "county:*") %>% 
  rename(state_fips = state, county_fips = county, population = POP) %>% 
  mutate(state_fips = as.numeric(state_fips), county_fips = as.numeric(county_fips))

popData <- popDataRaw %>%
  mutate(county_fips = ifelse(paste0(state_fips, county_fips) == "46102", 113, county_fips)) # dealing with the renaming of Shannon County

saveRDS(popData, "data/popData.rds")
