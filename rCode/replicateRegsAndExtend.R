

source("rCode/preamble.R")
BWDataByCounty <- readRDS("data/BWCountyLevel.rds")

#################################################################################
# First we run our code with their data. Do we get the same result?
#################################################################################

# the BW results
dataset1 <- BWDataByCounty %>%  filter(year == 2016)

firstStageModel1 <- felm(
  data = dataset1,
  formula = mfgLayoffs ~ bartik_leo5 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y | id_state)

secondStageModel1 <- felm(
  data = dataset1,
  formula = ddem_votes_pct1 ~ firstStageModel1$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y | id_state)

rse1_1 <- coeftest(firstStageModel1, vcov = vcovHC(firstStageModel1, type = "HC0"))[,2]
rse1_2 <- coeftest(secondStageModel1, vcov = vcovHC(secondStageModel1, type = "HC0"))[,2]

firstStageModelW1 <- felm(
  data = dataset1 %>%  filter(is.finite(bartik_leo5_w2)),
  formula = mfgLayoffsW ~ bartik_leo5_w2 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y  + mfgLayoffsNW| id_state)

secondStageModelW1 <- felm(
  data = dataset1  %>%  filter(is.finite(bartik_leo5_w2)),
  formula = ddem_votes_pct1 ~ firstStageModelW1$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + mfgLayoffsNW| id_state)

rseW1_1 <- coeftest(firstStageModelW1, vcov = vcovHC(firstStageModelW1, type = "HC0"))[,2]
rseW1_2 <- coeftest(secondStageModelW1, vcov = vcovHC(secondStageModelW1, type = "HC0"))[,2]

stargazer(secondStageModel1, secondStageModelW1, se = list(rse1_2, rseW1_2), type = "text")

# note that we do get the same results. So on we go

#################################################################################
# Next we filter out the countries with 0 manufacturing share in 2011. 
#################################################################################

# no zero values
datasetNoZeros <- dataset1 %>%  filter(bartik_leo5 != 0, is.finite(bartik_leo5))


firstStageModelNoZeros <- felm(
  data = datasetNoZeros,
  formula = mfgLayoffs ~ bartik_leo5 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y | id_state)

secondStageModelNoZeros <- felm(
  data = datasetNoZeros,
  formula = ddem_votes_pct1 ~ firstStageModelNoZeros$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y | id_state)

rseNoZeros_1 <- coeftest(firstStageModelNoZeros, vcov = vcovHC(firstStageModelNoZeros, type = "HC0"))[,2]
rseNoZeros_2 <- coeftest(secondStageModelNoZeros, vcov = vcovHC(secondStageModelNoZeros, type = "HC0"))[,2]

firstStageModelWNoZeros <- felm(
  data = datasetNoZeros %>%  filter(is.finite(bartik_leo5_w2)),
  formula = mfgLayoffsW ~ bartik_leo5_w2 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y  + mfgLayoffsNW| id_state)

secondStageModelWNoZeros <- felm(
  data = datasetNoZeros  %>%  filter(is.finite(bartik_leo5_w2)),
  formula = ddem_votes_pct1 ~ firstStageModelWNoZeros$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + mfgLayoffsNW| id_state)

rseWNoZeros_1 <- coeftest(firstStageModelWNoZeros, vcov = vcovHC(firstStageModelWNoZeros, type = "HC0"))[,2]
rseWNoZeros_2 <- coeftest(secondStageModelWNoZeros, vcov = vcovHC(secondStageModelWNoZeros, type = "HC0"))[,2]


stargazer(secondStageModel1, secondStageModelNoZeros, se = list(rse1_2, rseNoZeros_2), type = "text")
stargazer(secondStageModelW1, secondStageModelWNoZeros, se = list(rse1_2, rseWNoZeros_2), type = "text")
# basically no change

#################################################################################
# Now, their model with all their controls:
#################################################################################

datasetallControls <- datasetNoZeros %>%  filter(is.finite(white_counties_4y), is.finite(msl_service_pc4y))


firstStageModelallControls <- felm(
  data = datasetallControls,
  formula = mfgLayoffs ~ bartik_leo5 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y | id_state)

secondStageModelallControls <- felm(
  data = datasetallControls,
  formula = ddem_votes_pct1 ~ firstStageModelallControls$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y  + white_counties_4y + msl_service_pc4y| id_state)

rseallControls_1 <- coeftest(firstStageModelallControls, vcov = vcovHC(firstStageModelallControls, type = "HC0"))[,2]
rseallControls_2 <- coeftest(secondStageModelallControls, vcov = vcovHC(secondStageModelallControls, type = "HC0"))[,2]

firstStageModelWallControls <- felm(
  data = datasetallControls %>%  filter(is.finite(bartik_leo5_w2)),
  formula = mfgLayoffsW ~ bartik_leo5_w2 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y  + mfgLayoffsNW| id_state)

secondStageModelWallControls <- felm(
  data = datasetallControls  %>%  filter(is.finite(bartik_leo5_w2)),
  formula = ddem_votes_pct1 ~ firstStageModelWallControls$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y  + mfgLayoffsNW| id_state)

rseWallControls_1 <- coeftest(firstStageModelWallControls, vcov = vcovHC(firstStageModelWallControls, type = "HC0"))[,2]
rseWallControls_2 <- coeftest(secondStageModelWallControls, vcov = vcovHC(secondStageModelWallControls, type = "HC0"))[,2]


stargazer(secondStageModel1, secondStageModelallControls, se = list(rse1_2, rseallControls_2), type = "text")
stargazer(secondStageModelW1, secondStageModelWallControls, se = list(rse1_2, rseWallControls_2), type = "text")

#################################################################################
# Let's try using our layoffs data instead of theirs
#################################################################################

censusCountyData  <- readRDS("data/censusDataByCounty.rds")
bartikOurs <-  readRDS("data/finalBartik.RDS")


# compute relevant statistics by race/eth
ourDatasetBase <- censusCountyData %>%
  mutate(year = as.integer(substr(time, 0, 4))) %>% 
  mutate(wNw = case_when(
    race == "A0" & ethnicity == "A0" ~ "total",
    race == "A1" & ethnicity == "A1" ~ "white",
    race != "A0" & ethnicity != "A0" ~ "nonwhite",
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

# TODO: take ratios of 2011 employment
# summarize across years
datasetOursRaw <- BWDataByCounty %>%  filter(year == 2016) %>%  dplyr::rename(BWLayoffs = mfgLayoffs) %>% 
  left_join(ourDatasetBase %>% 
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
              )) %>%
  filter(mfgEmp != 0)  %>% 
  mutate(mfgLayoffs = mfgLayoffs/totalEmp, 
         mfgLayoffsS = mfgLayoffsS/totalEmp,
         mfgNetChange = mfgNetChange/totalEmp) %>% 
  rename(countyPop = population)

datasetOursRaw04 <- BWDataByCounty %>%  filter(year == 2016) %>%  dplyr::rename(BWLayoffs = mfgLayoffs) %>% 
  left_join(ourDatasetBase %>% 
              group_by(wNw, state_fips, county_fips) %>% 
              filter(year %in% 2004:2015) %>%  
              arrange(time) %>% 
              summarize(
                layoffCV = sd(mfgEmp, na.rm = T)/mean(mfgEmp, na.rm = T),
                totalEmp = mean(totalEmp, na.rm = T), 
                mfgEmp = mean(mfgEmp, na.rm = T),
                mfgLayoffs = sum(mfgLayoffs, na.rm = T),
                mfgLayoffsS = sum(mfgLayoffsS, na.rm = T),
                mfgNetChange  = -sum(mfgNetChange, na.rm = T),
                population = mean(population, na.rm = T)
              )) %>%
  filter(mfgEmp != 0)  %>% 
  mutate(mfgLayoffs = mfgLayoffs/totalEmp,
         mfgLayoffsS = mfgLayoffsS/totalEmp,
         mfgNetChange = mfgNetChange/totalEmp) %>% 
  rename(countyPop = population)

datasetOursRaw12 <- BWDataByCounty %>%  filter(year == 2012) %>%  dplyr::rename(BWLayoffs = mfgLayoffs) %>% 
  left_join(ourDatasetBase %>% 
              group_by(wNw, state_fips, county_fips) %>% 
              filter(year %in% 2008:2011) %>%  
              arrange(time) %>% 
              summarize(
                layoffCV = sd(mfgEmp, na.rm = T)/mean(mfgEmp, na.rm = T),
                totalEmp = mean(totalEmp, na.rm = T), 
                mfgEmp = mean(mfgEmp, na.rm = T),
                mfgLayoffs = sum(mfgLayoffs, na.rm = T),
                mfgLayoffsS = sum(mfgLayoffsS, na.rm = T),
                mfgNetChange  = -sum(mfgNetChange, na.rm = T),
                population = mean(population, na.rm = T)
              )) %>%
  filter(mfgEmp != 0)  %>% 
  mutate(mfgLayoffs = mfgLayoffs/totalEmp,
         mfgLayoffsS = mfgLayoffsS/totalEmp,
         mfgNetChange = mfgNetChange/totalEmp) %>% 
  rename(countyPop = population)


datasetOursRaw1204 <- BWDataByCounty %>%  filter(year == 2012) %>%  dplyr::rename(BWLayoffs = mfgLayoffs) %>% 
  left_join(ourDatasetBase %>% 
              group_by(wNw, state_fips, county_fips) %>% 
              filter(year %in% 2004:2011) %>%  
              arrange(time) %>% 
              summarize(
                layoffCV = sd(mfgEmp, na.rm = T)/mean(mfgEmp, na.rm = T),
                totalEmp = mean(totalEmp, na.rm = T), 
                mfgEmp = mean(mfgEmp, na.rm = T),
                mfgLayoffs = sum(mfgLayoffs, na.rm = T),
                mfgLayoffsS = sum(mfgLayoffsS, na.rm = T),
                mfgNetChange  = -sum(mfgNetChange, na.rm = T),
                population = mean(population, na.rm = T)
              )) %>%
  filter(mfgEmp != 0)  %>% 
  mutate(mfgLayoffs = mfgLayoffs/totalEmp,
         mfgLayoffsS = mfgLayoffsS/totalEmp,
         mfgNetChange = mfgNetChange/totalEmp) %>%
  rename(countyPop = population)


datasetOurs <- datasetOursRaw %>%  filter(is.finite(white_counties_4y), is.finite(msl_service_pc4y))  %>%
  pivot_wider(names_from = wNw, values_from = c(totalEmp, mfgEmp, mfgLayoffs, mfgLayoffsS, mfgNetChange,layoffCV))  %>% left_join(bartikOurs)
datasetOurs04 <- datasetOursRaw04 %>%  filter(is.finite(white_counties_4y), is.finite(msl_service_pc4y)) %>%
  pivot_wider(names_from = wNw, values_from = c(totalEmp, mfgEmp, mfgLayoffs, mfgLayoffsS, mfgNetChange,layoffCV))  %>% left_join(bartikOurs)
datasetOurs12 <- datasetOursRaw12 %>%  filter(is.finite(white_counties_4y), is.finite(msl_service_pc4y)) %>%
  pivot_wider(names_from = wNw, values_from = c(totalEmp, mfgEmp, mfgLayoffs, mfgLayoffsS, mfgNetChange,layoffCV))  %>% left_join(bartikOurs)
datasetOurs1204 <- datasetOursRaw1204 %>%  filter(is.finite(white_counties_4y), is.finite(msl_service_pc4y)) %>%
  pivot_wider(names_from = wNw, values_from = c(totalEmp, mfgEmp, mfgLayoffs, mfgLayoffsS, mfgNetChange,layoffCV))  %>% left_join(bartikOurs)


# check to see how much it matters how you normalize employment

ggplot(datasetOurs) + geom_point(aes(BWLayoffs, mfgLayoffs_total))

ggplot(datasetOurs) + geom_point(aes(mfgNetChange_white, mfgNetChange_nonwhite))
ggplot(datasetOurs) + geom_point(aes(mfgNetChange_white*totalEmp_white/totalEmp_total, mfgNetChange_nonwhite*totalEmp_nonwhite/totalEmp_total))




firstStageModelOurs <- felm(
  data = datasetOurs %>%  
    filter(is.finite(mfgLayoffs_total), is.finite(bartik_leo5 )),
  formula = mfgLayoffs_total ~ bartik_leo5  +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y  | id_state)

secondStageModelOurs <- felm(
  data = datasetOurs %>% 
    filter(is.finite(mfgLayoffs_total), is.finite(bartik_leo5 )),
  formula = ddem_votes_pct1 ~ firstStageModelOurs$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y | id_state)

rseOurs_1 <- coeftest(firstStageModelOurs, vcov = vcovHC(firstStageModelOurs, type = "HC0"))[,2]
rseOurs_2 <- coeftest(secondStageModelOurs, vcov = vcovHC(secondStageModelOurs, type = "HC0"))[,2]

firstStageModelWOurs <- felm(
  data = datasetOurs %>%
    filter(is.finite(bartik_leo5_w2  ), is.finite(mfgLayoffs_white)),
  formula = mfgLayoffs_white ~ bartik_leo5_w2  +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y  + mfgLayoffsNW| id_state)

secondStageModelWOurs <- felm(
  data = datasetOurs  %>%
    filter(is.finite(bartik_leo5_w2), is.finite(mfgLayoffs_white)),
  formula = ddem_votes_pct1 ~ firstStageModelWOurs$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y + mfgLayoffsNW| id_state)

rseWOurs_1 <- coeftest(firstStageModelWOurs, vcov = vcovHC(firstStageModelWOurs, type = "HC0"))[,2]
rseWOurs_2 <- coeftest(secondStageModelWOurs, vcov = vcovHC(secondStageModelWOurs, type = "HC0"))[,2]


stargazer(secondStageModel1, secondStageModelOurs, se = list(rse1_2, rseOurs_2), type = "text")
stargazer(secondStageModelW1, secondStageModelWOurs, se = list(rse1_2, rseWOurs_2), type = "text")
# not huge changes


#################################################################
# using net change 2012-2016

datasetNetC <- datasetOurs

firstStageModelNetC <- felm(
  data = datasetNetC  %>%
    filter(is.finite(bartik_leo5)),
  formula = mfgNetChange_total ~ bartik_leo5 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y | id_state)

secondStageModelNetC <- felm(
  data = datasetNetC %>%
    filter(is.finite(bartik_leo5)),
  formula = ddem_votes_pct1 ~ firstStageModelNetC$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y | id_state)

rseNetC_1 <- coeftest(firstStageModelNetC, vcov = vcovHC(firstStageModelNetC, type = "HC0"))[,2]
rseNetC_2 <- coeftest(secondStageModelNetC, vcov = vcovHC(secondStageModelNetC, type = "HC0"))[,2]

firstStageModelWNetC <- felm(
  data = datasetNetC %>%  filter(is.finite(bartik_leo5_w2), is.finite(mfgNetChange_white), is.finite(mfgNetChange_nonwhite)),
  formula = mfgNetChange_white ~ bartik_leo5_w2 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y + mfgNetChange_nonwhite| id_state)

secondStageModelWNetC <- felm(
  data = datasetNetC  %>%   filter(is.finite(bartik_leo5_w2), is.finite(mfgNetChange_white), is.finite(mfgNetChange_nonwhite)),
  formula = ddem_votes_pct1 ~ firstStageModelWNetC$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y + mfgNetChange_nonwhite| id_state)

rseWNetC_1 <- coeftest(firstStageModelWNetC, vcov = vcovHC(firstStageModelWNetC, type = "HC0"))[,2]
rseWNetC_2 <- coeftest(secondStageModelWNetC, vcov = vcovHC(secondStageModelWNetC, type = "HC0"))[,2]


stargazer(secondStageModel1, secondStageModelNetC, se = list(rse1_2, rseNetC_2), type = "text")
stargazer(secondStageModelW1, secondStageModelWNetC, se = list(rse1_2, rseWNetC_2), type = "text")


#################################################################
# using net change 2012-2016 - old white method


firstStageModelWNetCA <- felm(
  data = datasetNetC %>%  filter(is.finite(bartik_leo5_w2), is.finite(mfgNetChange_white)),
  formula = mfgNetChange_white ~ bartik_leo5_w2 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y  + mfgLayoffsNW| id_state)

secondStageModelWNetCA <- felm(
  data = datasetNetC  %>%   filter(is.finite(bartik_leo5_w2), is.finite(mfgNetChange_white)),
  formula = ddem_votes_pct1 ~ firstStageModelWNetCA$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y + mfgLayoffsNW| id_state)

rseWNetCA_1 <- coeftest(firstStageModelWNetCA, vcov = vcovHC(firstStageModelWNetCA, type = "HC0"))[,2]
rseWNetCA_2 <- coeftest(secondStageModelWNetCA, vcov = vcovHC(secondStageModelWNetCA, type = "HC0"))[,2]


stargazer(secondStageModelW1, secondStageModelWNetC, secondStageModelWNetCA, se = list(rse1_2, rseWNetC_2, rseWNetCA_2), type = "text")

#################################################################
# using net change 2004-2016


datasetNetC04 <- datasetOurs04

firstStageModelNetC04 <- felm(
  data = datasetNetC04  %>%  filter(is.finite(mfgNetChange_total)),
  formula = mfgNetChange_total ~ bartik_leo5 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y | id_state)

secondStageModelNetC04 <- felm(
  data = datasetNetC04  %>%  filter(is.finite(mfgNetChange_total)),
  formula = ddem_votes_pct1 ~ firstStageModelNetC04$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y | id_state)

rseNetC04_1 <- coeftest(firstStageModelNetC04, vcov = vcovHC(firstStageModelNetC04, type = "HC0"))[,2]
rseNetC04_2 <- coeftest(secondStageModelNetC04, vcov = vcovHC(secondStageModelNetC04, type = "HC0"))[,2]

firstStageModelWNetC04 <- felm(
  data = datasetNetC04 %>%  filter(is.finite(bartik_leo5_w2), is.finite(mfgNetChange_white)),
  formula = mfgNetChange_white ~ bartik_leo5_w2 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y  + mfgNetChange_nonwhite| id_state)

secondStageModelWNetC04 <- felm(
  data = datasetNetC04  %>%  filter(is.finite(bartik_leo5_w2), is.finite(mfgNetChange_white)),
  formula = ddem_votes_pct1 ~ firstStageModelWNetC04$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y + mfgNetChange_nonwhite| id_state)

rseWNetC04_1 <- coeftest(firstStageModelWNetC04, vcov = vcovHC(firstStageModelWNetC04, type = "HC0"))[,2]
rseWNetC04_2 <- coeftest(secondStageModelWNetC04, vcov = vcovHC(secondStageModelWNetC04, type = "HC0"))[,2]


stargazer(secondStageModel1, secondStageModelNetC04, se = list(rse1_2, rseNetC04_2), type = "text")
stargazer(secondStageModelW1, secondStageModelWNetC04, se = list(rse1_2, rseWNetC04_2), type = "text")


#################################################################
# using net change 2012 election


datasetNetC12 <- datasetOurs12

firstStageModelNetC12 <- felm(
  data = datasetNetC12 %>%  filter(is.finite(mfgNetChange_total)),
  formula = mfgNetChange_total ~ bartik_leo5 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y | id_state)

secondStageModelNetC12 <- felm(
  data = datasetNetC12%>%  filter(is.finite(mfgNetChange_total)),
  formula = ddem_votes_pct1 ~ firstStageModelNetC12$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y | id_state)

rseNetC12_1 <- coeftest(firstStageModelNetC12, vcov = vcovHC(firstStageModelNetC12, type = "HC0"))[,2]
rseNetC12_2 <- coeftest(secondStageModelNetC12, vcov = vcovHC(secondStageModelNetC12, type = "HC0"))[,2]

firstStageModelWNetC12 <- felm(
  data = datasetNetC12 %>%  filter(is.finite(bartik_leo5_w2), is.finite(mfgNetChange_white)),
  formula = mfgNetChange_white ~ bartik_leo5_w2 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y  + mfgNetChange_nonwhite| id_state)

secondStageModelWNetC12 <- felm(
  data = datasetNetC12  %>%  filter(is.finite(bartik_leo5_w2), is.finite(mfgNetChange_white)),
  formula = ddem_votes_pct1 ~ firstStageModelWNetC12$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y + mfgNetChange_nonwhite| id_state)

rseWNetC12_1 <- coeftest(firstStageModelWNetC12, vcov = vcovHC(firstStageModelWNetC12, type = "HC0"))[,2]
rseWNetC12_2 <- coeftest(secondStageModelWNetC12, vcov = vcovHC(secondStageModelWNetC12, type = "HC0"))[,2]


stargazer(secondStageModel1, secondStageModelNetC12, se = list(rse1_2, rseNetC12_2), type = "text")
stargazer(secondStageModelW1, secondStageModelWNetC12, se = list(rse1_2, rseWNetC12_2), type = "text")

#################################################################
# using net change 2004-2011 2012 election


datasetNetC1204 <- datasetOurs1204

firstStageModelNetC1204 <- felm(
  data = datasetNetC1204  %>%  filter(is.finite(mfgNetChange_total)),
  formula = mfgNetChange_total ~ bartik_leo5 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y | id_state)

secondStageModelNetC1204 <- felm(
  data = datasetNetC1204  %>%  filter(is.finite(mfgNetChange_total)),
  formula = ddem_votes_pct1 ~ firstStageModelNetC1204$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y | id_state)

rseNetC1204_1 <- coeftest(firstStageModelNetC1204, vcov = vcovHC(firstStageModelNetC1204, type = "HC0"))[,2]
rseNetC1204_2 <- coeftest(secondStageModelNetC1204, vcov = vcovHC(secondStageModelNetC1204, type = "HC0"))[,2]

firstStageModelWNetC1204 <- felm(
  data = datasetNetC1204 %>%  filter(is.finite(bartik_leo5_w2)),
  formula = mfgNetChange_white ~ bartik_leo5_w2 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y  + mfgNetChange_nonwhite| id_state)

secondStageModelWNetC1204 <- felm(
  data = datasetNetC1204  %>%  filter(is.finite(bartik_leo5_w2)),
  formula = ddem_votes_pct1 ~ firstStageModelWNetC1204$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y + mfgNetChange_nonwhite| id_state)

rseWNetC1204_1 <- coeftest(firstStageModelWNetC1204, vcov = vcovHC(firstStageModelWNetC1204, type = "HC0"))[,2]
rseWNetC1204_2 <- coeftest(secondStageModelWNetC1204, vcov = vcovHC(secondStageModelWNetC1204, type = "HC0"))[,2]


stargazer(secondStageModel1, secondStageModelNetC1204, se = list(rse1_2, rseNetC1204_2), type = "text")
stargazer(secondStageModelW1, secondStageModelWNetC1204, se = list(rse1_2, rseWNetC1204_2), type = "text")


saveRDS(list(
  secondStageModelNetC = secondStageModelNetC,
  secondStageModelWNetC = secondStageModelWNetC,
  secondStageModelWNetC = secondStageModelWNetC))
#################################################################
# using coeff of var



datasetCV <- datasetOurs

firstStageModelCV <- felm(
  data = datasetCV%>%  filter(is.finite(layoffCV_total)),
  formula = layoffCV_total ~ bartik_leo5 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y | id_state)

secondStageModelCV <- felm(
  data = datasetCV %>%  filter(is.finite(layoffCV_total)),
  formula = ddem_votes_pct1 ~ firstStageModelCV$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y | id_state)

rseCV_1 <- coeftest(firstStageModelCV, vcov = vcovHC(firstStageModelCV, type = "HC0"))[,2]
rseCV_2 <- coeftest(secondStageModelCV, vcov = vcovHC(secondStageModelCV, type = "HC0"))[,2]

firstStageModelWCV <- felm(
  data = datasetCV %>%  filter(is.finite(bartik_leo5_w2)),
  formula = layoffCV_white ~ bartik_leo5_w2 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y  + mfgNetChange_nonwhite | id_state)

secondStageModelWCV <- felm(
  data = datasetOurs %>%  filter(is.finite(bartik_leo5_w2)),
  formula = ddem_votes_pct1 ~ firstStageModelWCV$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y + mfgNetChange_nonwhite| id_state)

rseWCV_1 <- coeftest(firstStageModelWCV, vcov = vcovHC(firstStageModelWCV, type = "HC0"))[,2]
rseWCV_2 <- coeftest(secondStageModelWCV, vcov = vcovHC(secondStageModelWCV, type = "HC0"))[,2]


stargazer(secondStageModel1, secondStageModelCV, se = list(rse1_2, rseCV_2), type = "text")
stargazer(secondStageModelW1, secondStageModelWCV, se = list(rse1_2, rseWCV_2), type = "text")



