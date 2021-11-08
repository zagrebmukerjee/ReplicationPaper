
source("rCode/preamble.R")

listCensusMetadata(
  name = QWIName, 
  type = "geography")

varnames <- listCensusMetadata(
  name = QWIName, 
  type = "variables")

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


