


#########################################
# Descriptive Statistics
#########################################


censusCountyData  <- readRDS("data/censusDataByCounty.rds")

descDatasetBase <- censusCountyData %>%
  mutate(year = as.integer(substr(time, 0, 4))) %>% 
  mutate(wNw = case_when(
    race == "A0" & ethnicity == "A0" ~ "Total",
    race == "A1" & ethnicity == "A1" ~ "White",
    race != "A0" & ethnicity != "A0" ~ "Non-white",
    TRUE ~ "na"
  )) %>% 
  filter(wNw != "na") %>% 
  group_by(wNw, state_fips, county_fips, year, time) %>% 
  summarize(
    totalEmp = sum(totalEmp, na.rm = T),
    mfgEmp = sum(mfgEmp, na.rm = T), 
    mfgLayoffs = sum(mfgLayoffs, na.rm = T),
    mfgLayoffsS = sum(mfgLayoffsS, na.rm = T),
    mfgNetChange  = sum(mfgNetChange, na.rm = T),
    population = last(population)
  )

descDatasetRaw <- descDatasetBase %>% 
              group_by(wNw, state_fips, county_fips) %>% 
              filter(year %in% 2012:2015) %>%  
              arrange(time) %>% 
              summarize(
                layoffCV = sd(mfgEmp, na.rm = T)/mean(mfgEmp, na.rm = T),
                totalEmp = mean(totalEmp, na.rm = T), 
                mfgEmp = mean(mfgEmp, na.rm = T),
                mfgLayoffs = sum(mfgLayoffs, na.rm = T),
                mfgLayoffsS = sum(mfgLayoffsS, na.rm = T),
                mfgNetChange  = -sum(mfgNetChange, na.rm = T),
                population = mean(population, na.rm = T)
              ) %>%
  filter(mfgEmp != 0)  %>% 
  rename(countyPop = population)


descDatasetRaw %>% 
  dplyr::select(wNw, state_fips, county_fips, totalEmp, mfgEmp, mfgNetChange) %>% 
  mutate(wNw = factor(x = wNw, levels = c("Total", "White", "Non-white"))) %>% 
  rename(
    `Total Employment` = totalEmp,
    `Manufacturing Employment` = mfgEmp,
    `Change in Mfg Employment` = mfgNetChange,
  )  %>%  
  group_by(state_fips, county_fips) %>% 
  mutate(`Change in Mfg Emp per Worker` = `Change in Mfg Employment`/max(`Total Employment`)) %>% 
  pivot_longer(cols = -c("wNw", "state_fips", "county_fips") ,names_to = "variable") %>%
  group_by(wNw, variable) %>% 
  summarize(
    Mean = mean(value, na.rm = TRUE), 
    `Std Dev` = sd(value, na.rm = TRUE),
    `5th Pctile` = quantile(value, probs = .05, na.rm = TRUE),
    Median = quantile(value, probs = .50, na.rm = TRUE),
    `95th Pctile` = quantile(value, probs = .95, na.rm = TRUE)
  ) %>%  arrange(desc(variable))
