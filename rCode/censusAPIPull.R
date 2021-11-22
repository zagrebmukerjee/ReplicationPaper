

# some scratch code to understand how Census vars work
#########################################################
# listCensusMetadata(
#   name = QWIName, 
#   type = "geography")
# 
# varnames <- listCensusMetadata(
#   name = QWIName,
#   type = "variables")
# 
# endpointsClean <- listCensusApis() %>%  mutate(type = substr(name,0,3)) %>%  
#   filter(type != "acs", type != "ase")
# 
# 
# varnamesPop <- listCensusMetadata(name ="pep/population",vintage = 2015, type = "variables") 


# Get aggregate & manufacturing employment, layoffs, job change by county

if(!file.exists("data/censusDataByCounty.rds")){
  
  totalData <- bind_rows(lapply(
    statesList,function(stateNum){
      getCensus(
        name = QWIName,
        vars = c("race", "ethnicity", "Emp"),
        region = "county:*",
        seasonadj = "U",
        regionin = paste0("state:", str_pad(stateNum, 2, pad = "0")), time = "from 2011 to 2016") %>% 
        rename(totalEmp = Emp) %>% tibble() }
  ))
  
  
  mfgData <- bind_rows(lapply(
    statesList, function(stateNum){
      getCensus(
        name = QWIName,
        vars = c("race", "ethnicity", "Emp", "FrmJbLs", "FrmJbLsS", "FrmJbC"),
        industry = "31-33",
        ownercode = "A05",
        region = "county:*",
        seasonadj = "U",
        regionin = paste0("state:", str_pad(stateNum, 2, pad = "0")), time = "from 2011 to 2016") %>% 
        rename(mfgEmp = Emp, mfgLayoffs = FrmJbLs, mfgLayoffsS = FrmJbLsS, mfgNetChange = FrmJbC)  %>% tibble()}
  ))
  
  # population
  popDataRaw <- getCensus(
    name = "pep/population",
    vintage = 2015, 
    vars = c("POP"),
    region = "county:*") %>% tibble()
  
  popData <- popDataRaw %>%
    mutate(county = ifelse(paste0(state, county) == "46102", 113, county)) # dealing with the renaming of Shannon County
  
  
  censusCountyDataRaw <- mfgData %>%  left_join(
    totalData, by = c("state", "county", "race", "ethnicity", "time", "industry", "code", "seasonadj" )) 
  
  
  saveRDS(mfgData, "data/censusDataByCounty.rds")
}




