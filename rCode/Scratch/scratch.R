censusCountyData  <- readRDS("data/censusDataByCounty.rds")
BWDataByCounty <- readRDS("data/BWCountyLevel.rds")


ourDatasetBase <- censusCountyData %>%
  mutate(year = as.integer(substr(time, 0, 4))) %>% 
  mutate(wNw = ifelse(race == "A0" & ethnicity == "A0", "total", 
                      ifelse(race == "A1" & ethnicity == "A1", "white", "nonwhite"))) %>% 
  group_by(wNw, state_fips, county_fips, year, time) %>% 
  summarize(
    totalEmp = sum(totalEmp, na.rm = T),
    mfgEmp = sum(mfgEmp, na.rm = T), 
    mfgLayoffs = sum(mfgLayoffs, na.rm = T),
    mfgLayoffsS = sum(mfgLayoffsS, na.rm = T),
    mfgNetChange  = sum(mfgNetChange, na.rm = T),
    population = last(population)
  ) 

BWDataByCounty %>%  filter(year == 2016, state_fips == 25) %>% 
  dplyr::select(county_fips, county, mfgLayoffs) %>% 
  arrange(desc(mfgLayoffs))


a <- ourDatasetBase %>% 
  group_by(wNw, state_fips, county_fips) %>% 
  filter(year %in% c(2012, 2013, 2014, 2015)) %>%  
  arrange(time) %>% 
  summarize(
    totalEmp = mean(totalEmp), 
    mfgEmp = mean(mfgEmp), 
    mfgLayoffs = sum(mfgLayoffs),
    mfgLayoffsS = sum(mfgLayoffsS),
    mfgNetChange  = sum(mfgNetChange),
    population = mean(population)
  )  %>%  filter(wNw == "total") %>% 
  mutate(mfgLayoffs  = mfgLayoffs /totalEmp) %>% 
  dplyr::select(county_fips,mfgLayoffs) %>%  
  arrange(desc(mfgLayoffs))

b <- BWDataByCounty %>%  filter(year == 2016) %>% 
  dplyr::select(county_fips, county, mfgLayoffs) %>% 
  arrange(desc(mfgLayoffs))



plotData <- a %>%  rename(mfgLayoffsOurs = mfgLayoffs) %>% left_join(b)

ggplot(plotData) + geom_point(aes(mfgLayoffs, mfgLayoffsOurs))
