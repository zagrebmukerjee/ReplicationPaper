


#########################################
# Descriptive Statistics
#########################################


datasetList <- readRDS("regData.rds")

tableOurs <- datasetList$datasetOurs %>% 
  ungroup() %>% 
  dplyr::select(ddem_votes_pct1, state_fips, county_fips, mfgShare_total, mfgShare_white, mfgShare_nonwhite,
                mfgNetChange_total, mfgNetChange_white, mfgNetChange_nonwhite) %>% 
  rename(
    `Change in Dem Vote Share` = ddem_votes_pct1,
    `Mfg Share of Emp` = mfgShare_total,
    `Mfg Share of Emp (White)` = mfgShare_white,
    `Mfg Share of Emp (Nonwhite)` = mfgShare_nonwhite,
    `Change in Mfg Jobs/ Worker` = mfgNetChange_total,
    `Change in Mfg Jobs/ Worker (W)` = mfgNetChange_white,
    `Change in Mfg Jobs/ Worker (NW)` = mfgNetChange_nonwhite
  ) %>% 
  pivot_longer(cols = -c( "state_fips", "county_fips") ,names_to = "variable") %>%
  mutate(
    variable = factor(variable, levels = c(
      "Change in Dem Vote Share",
      "Mfg Share of Emp",
      "Mfg Share of Emp (White)",
      "Mfg Share of Emp (Nonwhite)",
      "Change in Mfg Jobs/ Worker",
      "Change in Mfg Jobs/ Worker (W)",
      "Change in Mfg Jobs/ Worker (NW)"
      ))) %>% 
  group_by(variable) %>% 
  summarize(
    Mean = mean(value, na.rm = TRUE), 
    `Std Dev` = sd(value, na.rm = TRUE),
    `25th Pctile` = quantile(value, probs = .25, na.rm = TRUE),
    Median = quantile(value, probs = .50, na.rm = TRUE),
    `75th Pctile` = quantile(value, probs = .75, na.rm = TRUE)
  ) %>% rename(` ` = variable)




table04 <- datasetList$datasetOurs04 %>% 
  ungroup() %>% 
  dplyr::select(
    # ddem_votes_pct1,
    state_fips, 
    county_fips, 
    mfgShare_total, 
    mfgShare_white, 
    mfgShare_nonwhite,
    mfgNetChange_total, 
    mfgNetChange_white, 
    mfgNetChange_nonwhite) %>% 
  rename(
    # `Change in Dem Vote Share` = ddem_votes_pct1,
    `Mfg Share of Emp` = mfgShare_total,
    `Mfg Share of Emp (White)` = mfgShare_white,
    `Mfg Share of Emp (Nonwhite)` = mfgShare_nonwhite,
    `Change in Mfg Jobs/ Worker` = mfgNetChange_total,
    `Change in Mfg Jobs/ Worker (W)` = mfgNetChange_white,
    `Change in Mfg Jobs/ Worker (NW)` = mfgNetChange_nonwhite
  ) %>% 
  pivot_longer(cols = -c( "state_fips", "county_fips") ,names_to = "variable") %>%
  mutate(
    variable = factor(variable, levels = c(
      # "Change in Dem Vote Share",
      "Mfg Share of Emp",
      "Mfg Share of Emp (White)",
      "Mfg Share of Emp (Nonwhite)",
      "Change in Mfg Jobs/ Worker",
      "Change in Mfg Jobs/ Worker (W)",
      "Change in Mfg Jobs/ Worker (NW)"
    ))) %>% 
  group_by(variable) %>% 
  summarize(
    Mean = mean(value, na.rm = TRUE), 
    `Std Dev` = sd(value, na.rm = TRUE),
    `25th Pctile` = quantile(value, probs = .25, na.rm = TRUE),
    Median = quantile(value, probs = .50, na.rm = TRUE),
    `75th Pctile` = quantile(value, probs = .75, na.rm = TRUE)
  ) %>% rename(` ` = variable)

saveRDS(list(tableOurs = tableOurs, table04 = table04), file = "tableData.rds")
