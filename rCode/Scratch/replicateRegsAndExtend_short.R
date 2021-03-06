

source("rCode/preamble.R")
BWDataByCounty <- readRDS("data/BWCountyLevel.rds")

#################################################################################
# First we run our code with their data to replicate 
# table 2 in Baccini and Weymouth's paper
#################################################################################

# load B&W data
BWDataByCounty_16 <- BWDataByCounty %>%  filter(year == 2016)

# model 1
BW_firstStageModel1 <- felm(
  data = BWDataByCounty_16,
  formula = mfgLayoffs ~ bartik_leo5 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y | id_state)

BW_secondStageModel1 <- felm(
  data = BWDataByCounty_16,
  formula = ddem_votes_pct1 ~ BW_firstStageModel1$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y | id_state)

rse1_1 <- coeftest(BW_firstStageModel1, vcov = vcovHC(BW_firstStageModel1, type = "HC0"))[,2]
rse1_2 <- coeftest(BW_secondStageModel1, vcov = vcovHC(BW_secondStageModel1, type = "HC0"))[,2]

# model 2
BW_firstStageModel2 <- felm(
  data = BWDataByCounty_16 %>% filter(!is.na(white_counties_4y)),
  formula = mfgLayoffs ~ bartik_leo5 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y | id_state)

BW_secondStageModel2 <- felm(
  data = BWDataByCounty_16 %>% filter(!is.na(white_counties_4y)),
  formula = ddem_votes_pct1 ~ BW_firstStageModel2$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y | id_state)

rse2_1 <- coeftest(BW_firstStageModel2, vcov = vcovHC(BW_firstStageModel2, type = "HC0"))[,2]
rse2_2 <- coeftest(BW_secondStageModel2, vcov = vcovHC(BW_secondStageModel2, type = "HC0"))[,2]

# model 3
BW_firstStageModel3 <- felm(
  data = BWDataByCounty_16 %>% filter(!is.na(msl_service_pc4y),
                                   !is.na(white_counties_4y)),
  formula = mfgLayoffs ~ bartik_leo5 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y| id_state)

BW_secondStageModel3 <- felm(
  data = BWDataByCounty_16  %>% filter(!is.na(msl_service_pc4y),
                                    !is.na(white_counties_4y)),
  formula = ddem_votes_pct1 ~ BW_firstStageModel3$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y| id_state)

rse3_1 <- coeftest(BW_firstStageModel3, vcov = vcovHC(BW_firstStageModel3, type = "HC0"))[,2]
rse3_2 <- coeftest(BW_secondStageModel3, vcov = vcovHC(BW_secondStageModel3, type = "HC0"))[,2]

# model 4
BW_firstStageModel4 <- felm(
  data = BWDataByCounty_16 %>%  filter(is.finite(bartik_leo5_w2)),
  formula = mfgLayoffsW ~ bartik_leo5_w2 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y  + mfgLayoffsNW| id_state)

BW_secondStageModel4 <- felm(
  data = BWDataByCounty_16  %>%  filter(is.finite(bartik_leo5_w2)),
  formula = ddem_votes_pct1 ~ BW_firstStageModel4$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + mfgLayoffsNW| id_state)

rse4_1 <- coeftest(BW_firstStageModel4, vcov = vcovHC(BW_firstStageModel4, type = "HC0"))[,2]
rse4_2 <- coeftest(BW_secondStageModel4, vcov = vcovHC(BW_secondStageModel4, type = "HC0"))[,2]

# model 5
BW_firstStageModel5 <- felm(
  data = BWDataByCounty_16 %>%  filter(is.finite(bartik_leo5_w2),
                                    !is.na(white_counties_4y)),
  formula = mfgLayoffsW ~ bartik_leo5_w2 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y  +  white_counties_4y + mfgLayoffsNW| id_state)

BW_secondStageModel5 <- felm(
  data = BWDataByCounty_16  %>%  filter(is.finite(bartik_leo5_w2),
                                     !is.na(white_counties_4y)),
  formula = ddem_votes_pct1 ~ BW_firstStageModel5$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + mfgLayoffsNW| id_state)

rse5_1 <- coeftest(BW_firstStageModel5, vcov = vcovHC(BW_firstStageModel5, type = "HC0"))[,2]
rse5_2 <- coeftest(BW_secondStageModel5, vcov = vcovHC(BW_secondStageModel5, type = "HC0"))[,2]


# model 6
BW_firstStageModel6 <- felm(
  data = BWDataByCounty_16 %>%  filter(is.finite(bartik_leo5_w2),
                                    !is.na(msl_service_pc4y),
                                    !is.na(white_counties_4y)),
  formula = mfgLayoffsW ~ bartik_leo5_w2 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y  +  white_counties_4y + msl_service_pc4y + mfgLayoffsNW| id_state)

BW_secondStageModel6 <- felm(
  data = BWDataByCounty_16  %>%  filter(is.finite(bartik_leo5_w2),
                                     !is.na(msl_service_pc4y),
                                     !is.na(white_counties_4y)),
  formula = ddem_votes_pct1 ~ BW_firstStageModel6$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y + mfgLayoffsNW| id_state)

rse6_1 <- coeftest(BW_firstStageModel6, vcov = vcovHC(BW_firstStageModel6, type = "HC0"))[,2]
rse6_2 <- coeftest(BW_secondStageModel6, vcov = vcovHC(BW_secondStageModel6, type = "HC0"))[,2]

# table 1, aggregate race/ethnicity
# note that "fitted values" refer to the predicted value of manufacturing layoffs per
# 2011 worker
stargazer(BW_secondStageModel1, BW_secondStageModel2, BW_secondStageModel3,
          se = list(rse1_2, rse2_2, rse3_2), type = "text")

# table 2, deaggregated race/ethnicity
stargazer(BW_secondStageModel4,
          BW_secondStageModel5, BW_secondStageModel6,
          se = list(rse4_2, rse5_2, rse6_2), type = "text")

# note that we do get the same results [exact same coefficent estimates,
# standard errors, and N]. So on we go

rm(BW_firstStageModel1,
   BW_firstStageModel2,
   BW_firstStageModel3,
   BW_firstStageModel4,
   BW_firstStageModel5,
   BW_firstStageModel6)
#################################################################################
# Using BW's dataset, we filter out counties with 0 manufacturing share in 2011. 
#################################################################################

# lets recreate models 2 and 5 using no zeroes

# model 2
firstStageModel2_NZ <- felm(
  data = BWDataByCounty_16 %>% filter(!is.na(white_counties_4y),
                               bartik_leo5 != 0, 
                               is.finite(bartik_leo5)),
  formula = mfgLayoffs ~ bartik_leo5 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y | id_state)

secondStageModel2_NZ <- felm(
  data = BWDataByCounty_16 %>% filter(!is.na(white_counties_4y),
                               bartik_leo5 != 0, 
                               is.finite(bartik_leo5)),
  formula = ddem_votes_pct1 ~ firstStageModel2_NZ$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y | id_state)

rse2_1_nz <- coeftest(firstStageModel2_NZ, vcov = vcovHC(firstStageModel2_NZ, type = "HC0"))[,2]
rse2_2_nz <- coeftest(secondStageModel2_NZ, vcov = vcovHC(secondStageModel2_NZ, type = "HC0"))[,2]

# model 5
firstStageModel5_NZ <- felm(
  data = BWDataByCounty_16 %>%  filter(is.finite(bartik_leo5_w2),
                                    !is.na(white_counties_4y),
                                    bartik_leo5 != 0),
  formula = mfgLayoffsW ~ bartik_leo5_w2 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y  +  white_counties_4y + mfgLayoffsNW| id_state)

secondStageModel5_NZ <- felm(
  data = BWDataByCounty_16  %>%  filter(is.finite(bartik_leo5_w2),
                                     !is.na(white_counties_4y),
                                     bartik_leo5 != 0),
  formula = ddem_votes_pct1 ~ firstStageModel5_NZ$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + mfgLayoffsNW| id_state)

rse5_1_nz <- coeftest(firstStageModel5_NZ, vcov = vcovHC(firstStageModel5_NZ, type = "HC0"))[,2]
rse5_2_nz <- coeftest(secondStageModel5_NZ, vcov = vcovHC(secondStageModel5_NZ, type = "HC0"))[,2]


stargazer(secondStageModel2_NZ, secondStageModel5_NZ, se = list(rse2_2_nz, rse5_2_nz), type = "text")
# basically no change


rm(firstStageModel5_NZ,
   secondStageModel2_NZ, 
   secondStageModel5_NZ,
   firstStageModel2_NZ,
   BWDataByCounty_16)
# if we exclude counties with no manufacturing share,


#################################################################################
# Let's try using our layoffs [net change in manufacturing layoffs] 
# data instead of theirs [manufacturing job loss]
# first, prepare the datasets
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


datasetClean <- function(electionYear, startYear, aggregateYears){
  
  tmp <- ourDatasetBase %>% 
    group_by(state_fips, county_fips) %>% 
    mutate(emp_base_total = mean(totalEmp[year == startYear]),
           emp_base_mfg = mean(mfgEmp[year == startYear])) %>% 
    filter(year %in% aggregateYears) %>%  
    group_by() %>% 
    group_by(wNw, state_fips, county_fips, emp_base_total, emp_base_mfg) %>% 
    arrange(time) %>% 
    summarize(
      #layoffCV = sd(mfgEmp, na.rm = T)/mean(mfgEmp, na.rm = T), #not using this variable
      totalEmp = mean(totalEmp, na.rm = T), 
      mfgEmp = mean(mfgEmp, na.rm = T),
      mfgLayoffs = sum(mfgLayoffs, na.rm = T),
      # mfgLayoffsS = sum(mfgLayoffsS, na.rm = T), #not using this variable
      mfgNetChange  = -sum(mfgNetChange, na.rm = T),
      population = mean(population, na.rm = T)
    )
  
  BWDataByCounty %>%  filter(year == electionYear) %>%  dplyr::rename(BWLayoffs = mfgLayoffs) %>% 
    left_join( tmp)  %>%
    #filter(mfgEmp != 0)  %>% 
    rename(countyPop = population) %>%  
    filter(is.finite(white_counties_4y), is.finite(msl_service_pc4y))  %>%
    pivot_wider(names_from = wNw, 
                values_from = c(totalEmp, mfgEmp, 
                                mfgLayoffs, 
                                # mfgLayoffsS, # not using this variable for now
                                mfgNetChange,
                                #layoffCV  # not using this variable
                                )) %>% 
    mutate(mfgLayoffs_total = mfgLayoffs_total/emp_base_total,
           mfgNetChange_total = mfgNetChange_total/emp_base_total,
           mfgLayoffs_white = mfgLayoffs_white/emp_base_total,
           mfgNetChange_white = mfgNetChange_white/emp_base_total,
           mfgLayoffs_nonwhite = mfgLayoffs_nonwhite/emp_base_total,
           mfgNetChange_nonwhite = mfgNetChange_nonwhite/emp_base_total,
           mfgShare_total = mfgEmp_total/emp_base_total,
           mfgShare_white = mfgEmp_white/emp_base_total,
           mfgShare_nonwhite = mfgEmp_nonwhite/emp_base_total)
  
  
}


datasetOurs <- datasetClean(electionYear = 2016, startYear = 2011, aggregateYears = 2012:2015)
datasetOurs04 <- datasetClean(electionYear = 2016, startYear = 2003, aggregateYears = 2004:2015)
datasetOurs12 <- datasetClean(electionYear = 2012, startYear = 2007, aggregateYears = 2008:2011)
datasetOurs1204 <- datasetClean(electionYear = 2012, startYear = 2003, aggregateYears = 2004:2011)


###############################################################
# Rerun 2016 election analysis (using 2012-2015 layoff data) using
# our layoff measure, for B&W's models 2, 3, 5 and 6
################################################################

# check to see how much it matters how you normalize employment

# model 2
firstStageModelOurs_2 <- felm(
  data = datasetOurs %>%  
    filter(is.finite(mfgNetChange_total), is.finite(bartik_leo5 )),
  formula = mfgNetChange_total ~ bartik_leo5  +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y   | id_state)

secondStageModelOurs_2 <- felm(
  data = datasetOurs %>% 
    filter(is.finite(mfgNetChange_total), is.finite(bartik_leo5 )),
  formula = ddem_votes_pct1 ~ firstStageModelOurs_2$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y  | id_state)

rseOurs2_1 <- coeftest(firstStageModelOurs_2, vcov = vcovHC(firstStageModelOurs_2, type = "HC0"))[,2]
rseOurs2_2 <- coeftest(secondStageModelOurs_2, vcov = vcovHC(secondStageModelOurs_2, type = "HC0"))[,2]

# model 3
firstStageModelOurs_3 <- felm(
  data = datasetOurs %>%  
    filter(is.finite(mfgNetChange_total), is.finite(bartik_leo5 )),
  formula = mfgNetChange_total ~ bartik_leo5  +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y  | id_state)

secondStageModelOurs_3 <- felm(
  data = datasetOurs %>% 
    filter(is.finite(mfgNetChange_total), is.finite(bartik_leo5 )),
  formula = ddem_votes_pct1 ~ firstStageModelOurs_3$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y | id_state)

rseOurs3_1 <- coeftest(firstStageModelOurs_3, vcov = vcovHC(firstStageModelOurs_3, type = "HC0"))[,2]
rseOurs3_2 <- coeftest(secondStageModelOurs_3, vcov = vcovHC(secondStageModelOurs_3, type = "HC0"))[,2]

# model 5
firstStageModelOurs_5 <- felm(
  data = datasetOurs %>%
    filter(is.finite(bartik_leo5_w2  ),
           is.finite(mfgNetChange_white), 
           is.finite(mfgNetChange_nonwhite)),
  formula = mfgNetChange_white ~ bartik_leo5_w2  +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y   + mfgNetChange_nonwhite| id_state)

secondStageModelOurs_5 <- felm(
  data = datasetOurs  %>%
    filter(is.finite(bartik_leo5_w2),
           is.finite(mfgNetChange_white), 
           is.finite(mfgNetChange_nonwhite)),
  formula = ddem_votes_pct1 ~ firstStageModelOurs_5$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y  + mfgNetChange_nonwhite| id_state)

rseOurs5_1 <- coeftest(firstStageModelOurs_5, vcov = vcovHC(firstStageModelOurs_5, type = "HC0"))[,2]
rseOurs5_2 <- coeftest(secondStageModelOurs_5, vcov = vcovHC(secondStageModelOurs_5, type = "HC0"))[,2]


# model 6
firstStageModelOurs_6 <- felm(
  data = datasetOurs %>%
    filter(is.finite(bartik_leo5_w2  ), 
           is.finite(mfgNetChange_white), 
           is.finite(mfgNetChange_nonwhite)),
  formula = mfgNetChange_white ~ bartik_leo5_w2  +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y  + mfgNetChange_nonwhite| id_state)

secondStageModelOurs_6 <- felm(
  data = datasetOurs  %>%
    filter(is.finite(bartik_leo5_w2), is.finite(mfgNetChange_white), 
           is.finite(mfgNetChange_nonwhite)),
  formula = ddem_votes_pct1 ~ firstStageModelOurs_6$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y + mfgNetChange_nonwhite| id_state)

rseOurs6_1 <- coeftest(firstStageModelOurs_6, vcov = vcovHC(firstStageModelOurs_6, type = "HC0"))[,2]
rseOurs6_2 <- coeftest(secondStageModelOurs_6, vcov = vcovHC(secondStageModelOurs_6, type = "HC0"))[,2]

# compare model 2
stargazer(BW_secondStageModel2, secondStageModelOurs_2, se = list(rse2_2, rseOurs2_2), type = "text")

# model 3
stargazer(BW_secondStageModel3, secondStageModelOurs_3, se = list(rse3_2, rseOurs3_2), type = "text")

# model 5
stargazer(BW_secondStageModel5, secondStageModelOurs_5, se = list(rse5_2, rseOurs5_2), type = "text")

# model 6
stargazer(BW_secondStageModel6, secondStageModelOurs_6, se = list(rse6_2, rseOurs6_2), type = "text")



###############################################################
# Rerun 2016 election analysis, using 2004-2015 layoff data using
# our layoff measure
################################################################

# model 2
firstStageModelOurs04_2 <- felm(
  data = datasetOurs04 %>%  
    filter(is.finite(mfgNetChange_total), is.finite(bartik_leo5 )),
  formula = mfgNetChange_total ~ bartik_leo5  +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y   | id_state)

secondStageModelOurs04_2 <- felm(
  data = datasetOurs04 %>% 
    filter(is.finite(mfgNetChange_total), is.finite(bartik_leo5 )),
  formula = ddem_votes_pct1 ~ firstStageModelOurs04_2$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y  | id_state)

rseOurs042_1 <- coeftest(firstStageModelOurs04_2, vcov = vcovHC(firstStageModelOurs04_2, type = "HC0"))[,2]
rseOurs042_2 <- coeftest(secondStageModelOurs04_2, vcov = vcovHC(secondStageModelOurs04_2, type = "HC0"))[,2]

# model 3
firstStageModelOurs04_3 <- felm(
  data = datasetOurs04 %>%  
    filter(is.finite(mfgNetChange_total), is.finite(bartik_leo5 )),
  formula = mfgNetChange_total ~ bartik_leo5  +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y  | id_state)

secondStageModelOurs04_3 <- felm(
  data = datasetOurs04 %>% 
    filter(is.finite(mfgNetChange_total), is.finite(bartik_leo5 )),
  formula = ddem_votes_pct1 ~ firstStageModelOurs04_3$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y | id_state)

rseOurs043_1 <- coeftest(firstStageModelOurs04_3, vcov = vcovHC(firstStageModelOurs04_3, type = "HC0"))[,2]
rseOurs043_2 <- coeftest(secondStageModelOurs04_3, vcov = vcovHC(secondStageModelOurs04_3, type = "HC0"))[,2]

# model 5
firstStageModelOurs04_5 <- felm(
  data = datasetOurs04 %>%
    filter(is.finite(bartik_leo5_w2  ),
           is.finite(mfgNetChange_white), 
           is.finite(mfgNetChange_nonwhite)),
  formula = mfgNetChange_white ~ bartik_leo5_w2  +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y   + mfgNetChange_nonwhite| id_state)

secondStageModelOurs04_5 <- felm(
  data = datasetOurs04  %>%
    filter(is.finite(bartik_leo5_w2),
           is.finite(mfgNetChange_white), 
           is.finite(mfgNetChange_nonwhite)),
  formula = ddem_votes_pct1 ~ firstStageModelOurs04_5$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y  + mfgNetChange_nonwhite| id_state)

rseOurs045_1 <- coeftest(firstStageModelOurs04_5, vcov = vcovHC(firstStageModelOurs04_5, type = "HC0"))[,2]
rseOurs045_2 <- coeftest(secondStageModelOurs04_5, vcov = vcovHC(secondStageModelOurs04_5, type = "HC0"))[,2]


# model 6
firstStageModelOurs04_6 <- felm(
  data = datasetOurs04 %>%
    filter(is.finite(bartik_leo5_w2  ), 
           is.finite(mfgNetChange_white), 
           is.finite(mfgNetChange_nonwhite)),
  formula = mfgNetChange_white ~ bartik_leo5_w2  +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y  + mfgNetChange_nonwhite| id_state)

secondStageModelOurs04_6 <- felm(
  data = datasetOurs04  %>%
    filter(is.finite(bartik_leo5_w2), is.finite(mfgNetChange_white), 
           is.finite(mfgNetChange_nonwhite)),
  formula = ddem_votes_pct1 ~ firstStageModelOurs04_6$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y + mfgNetChange_nonwhite| id_state)

rseOurs046_1 <- coeftest(firstStageModelOurs04_6, vcov = vcovHC(firstStageModelOurs04_6, type = "HC0"))[,2]
rseOurs046_2 <- coeftest(secondStageModelOurs04_6, vcov = vcovHC(secondStageModelOurs04_6, type = "HC0"))[,2]

# compare model 2
stargazer(BW_secondStageModel2, secondStageModelOurs_2,
          secondStageModelOurs04_2, se = list(rse2_2, rseOurs2_2,
                                              rseOurs042_2), type = "text")

# model 3
stargazer(BW_secondStageModel3, 
          secondStageModelOurs_3, 
          secondStageModelOurs04_3,
          se = list(rse3_2, rseOurs3_2, rseOurs043_2), type = "text")

# model 5
stargazer(BW_secondStageModel5, secondStageModelOurs_5, 
          secondStageModelOurs04_5, se = list(rse5_2, rseOurs5_2,
                                              rseOurs045_2), type = "text")

# model 6
stargazer(BW_secondStageModel6, secondStageModelOurs_6, 
          secondStageModelOurs04_6,
          se = list(rse6_2, rseOurs6_2,
                    rseOurs046_2), type = "text")


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
  data = datasetNetC12 %>%  filter(is.finite(bartik_leo5_w2), is.finite(mfgNetChange_white), is.finite(mfgNetChange_nonwhite)),
  formula = mfgNetChange_white ~ bartik_leo5_w2 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y  + mfgNetChange_nonwhite| id_state)

secondStageModelWNetC12 <- felm(
  data = datasetNetC12  %>%  filter(is.finite(bartik_leo5_w2), is.finite(mfgNetChange_white), is.finite(mfgNetChange_nonwhite)),
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
  data = datasetNetC1204 %>%  filter(is.finite(bartik_leo5_w2), is.finite(mfgNetChange_nonwhite)),
  formula = mfgNetChange_white ~ bartik_leo5_w2 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y  + mfgNetChange_nonwhite| id_state)

secondStageModelWNetC1204 <- felm(
  data = datasetNetC1204  %>%  filter(is.finite(bartik_leo5_w2), is.finite(mfgNetChange_nonwhite)),
  formula = ddem_votes_pct1 ~ firstStageModelWNetC1204$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y + mfgNetChange_nonwhite| id_state)

rseWNetC1204_1 <- coeftest(firstStageModelWNetC1204, vcov = vcovHC(firstStageModelWNetC1204, type = "HC0"))[,2]
rseWNetC1204_2 <- coeftest(secondStageModelWNetC1204, vcov = vcovHC(secondStageModelWNetC1204, type = "HC0"))[,2]


stargazer(secondStageModel1, secondStageModelNetC1204, se = list(rse1_2, rseNetC1204_2), type = "text")
stargazer(secondStageModelW1, secondStageModelWNetC1204, se = list(rse1_2, rseWNetC1204_2), type = "text")

saveRDS(list(datasetOurs = datasetOurs, datasetOurs04=datasetOurs04 ), file = "regData.rds")
  
saveRDS(list(
  rseNetCA_2 = rseNetCA_2, 
  rseWNetCA_2 = rseWNetCA_2, 
  rseNetC_2 = rseNetC_2, 
  rseWNetC_2 = rseWNetC_2, 
  rseNetC04A_2 = rseNetC04A_2, 
  rseWNetC04A_2 = rseWNetC04A_2, 
  rseNetC04_2 = rseNetC04_2, 
  rseWNetC04_2 = rseWNetC04_2, 
  secondStageModelNetCA = secondStageModelNetCA,
  secondStageModelWNetCA = secondStageModelWNetCA,
  secondStageModelNetC = secondStageModelNetC,
  secondStageModelWNetC = secondStageModelWNetC,
  secondStageModelNetC04A = secondStageModelNetC04A,
  secondStageModelWNetC04A = secondStageModelWNetC04A,
  secondStageModelNetC04 = secondStageModelNetC04,
  secondStageModelWNetC04 = secondStageModelWNetC04),
  file = "regresults.rds")
