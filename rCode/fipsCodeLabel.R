###
# the counties do NOT match. eg. the FIPS list has 67 counties in SD, including Shannon County
# which no longer exists (renamed Oglala in 2015). Wiki says 66 counties. BW have 65. 
# FIPS list has one more county in VA. in VA, 'Bedford (independent) city, Virginia (51-515):
# Changed to town status and added to Bedford County (51-019) effective July 1, 2013
# my list still has bedford city and bedford county. 
# but why would theirs not have as many as  mine...
# maybe they omit bedford city because their list is post 2013


countyLevelRaw <- read_dta("OriginalMaterials/county_level.dta")


allFIPS <- tidycensus::fips_codes %>%  tibble() %>% 
  rename(state_fips = state_code, county_fips = county_code) %>% 
  mutate(state_fips = as.double(state_fips), county_fips = as.double(county_fips)) %>% 
  filter(paste0(state_fips, county_fips) != "46102") %>% # recently created Oglala county
  filter(paste0(state_fips, county_fips) != "51515") # recently deceased Bedford City

stateList <- allFIPS %>%  distinct(state, state_fips) 


countyLevel0 <-countyLevelRaw %>% rename(
  mfgLayoffs = msl_pc4y2,
  mfgLayoffsW = msl_w_pc4y2,
  mfgLayoffsNW = msl_nw_pc4y2
)  %>%  left_join(stateList) %>% 
  group_by(state_fips, year) %>% 
  mutate(id = row_number())

bwStates <- countyLevel0 %>%  distinct(id_state, state_name, state_fips)
bwStatesWithNames <- bwStates %>%  left_join(stateList) 
allFIPSBW <- allFIPS %>%  filter((state_fips %in% bwStatesWithNames$state_fips)) %>% 
  group_by(state, state_fips) %>% 
  mutate(id = row_number()) 


countyLevel <- countyLevel0 %>%  left_join(allFIPSBW %>%  dplyr::select(id, county_fips, county), by = c("id", "state", "state_fips")) %>% 
  dplyr::select(id, state_fips, state, state_name, county_fips, county, year, everything()) %>%
  filter(is.finite(mfgLayoffs)) # filters out 40-ish counties where mfgLayoffs is NA. TODO: missing data?
# these are missing in BW as well


saveRDS(countyLevel, "data/BWCountyLevel.rds")








