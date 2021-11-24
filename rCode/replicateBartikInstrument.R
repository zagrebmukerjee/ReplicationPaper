
################################################################
countyLevelData <- readRDS("data/censusDataByCounty.rds")
natlResult <- readRDS("data/natlResult.rds") 
################################################################

aggregate <- totalData %>%  left_join(mfgData) %>%
  mutate(
    year = as.integer(substr(time,start = 0, stop =4)),
    totalEmp = as.numeric(totalEmp),
    mfgEmp = as.numeric(mfgEmp)
  )  

dateStr <- c("2011-Q1","2011-Q2","2011-Q3","2011-Q4")

mfgTotal <- aggregate %>%  filter(time %in% dateStr) %>% 
  filter(race == "A0" & ethnicity == "A0") %>%
  group_by(state, county) %>% 
  summarize( mfgEmpTotal = mean(mfgEmp), totalEmp = mean(totalEmp)) %>% 
  mutate(mfgShareTotal = mfgEmpTotal/totalEmp)

mfgWhite <- aggregate %>%  filter(time %in%  dateStr) %>% 
  filter(race == "A1" & ethnicity == "A1") %>% 
  group_by(state, county) %>% 
  summarize( mfgEmpWhite = mean(mfgEmp), totalEmpWhite = mean(totalEmp)) %>% 
  mutate(mfgShareWhite = mfgEmpWhite/totalEmpWhite)

mfgNonwhite <- aggregate %>%  filter(time %in%  dateStr) %>% 
  filter(race != "A1" | ethnicity != "A1") %>% 
  filter(ethnicity != "A0" & race != "A0") %>%
  group_by(state, county) %>%
  summarise(totalEmpNonwhite = sum(totalEmp, na.rm = T)/length(dateStr), mfgEmpNonwhite = sum(mfgEmp, na.rm = T)/length(dateStr))  %>% 
  mutate(mfgShareNonwhite = mfgEmpNonwhite/totalEmpNonwhite)



natlBartik <- natlResult$totalLayoffs/natlResult$totalEmpl
natlBartikWhite <- natlResult$whiteLayoffs/natlResult$whiteEmpl
natlBartikNonwhite <- natlResult$nonwhiteLayoffs/natlResult$nonwhiteEmpl

finalBartikOurs <- mfgWhite %>%  left_join(mfgNonwhite) %>% left_join(mfgTotal) %>% 
  mutate(natlBartikWhite = natlBartikWhite,
         natlBartikNonwhite = natlBartikNonwhite,
         natlBartikTotal = natlBartik) %>% 
  mutate(bartikFinalWhite = natlBartikWhite * mfgShareWhite,
         bartikFinalNonwhite = natlBartikNonwhite * mfgShareNonwhite,
         bartikFinalTotal = natlBartikTotal * mfgShareTotal) %>% 
  dplyr::select(state, county, bartikFinalNonwhite, bartikFinalWhite, bartikFinalTotal) %>% 
  arrange(desc(bartikFinalWhite))  %>% 
  rename(state_fips = state, county_fips = county) %>% 
  mutate(state_fips = as.double(state_fips), county_fips = as.double(county_fips))

saveRDS(finalBartikOurs, "data/finalBartik.RDS")

# To check our Bartik against theirs - look at bartikComparison.R
