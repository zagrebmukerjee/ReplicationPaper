

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

stargazer(secondStageModel1, secondStageModelW1, se = c(rse1_2, rseW1_2), type = "text")

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


stargazer(secondStageModel1, secondStageModelNoZeros, se = c(rse1_2, rseNoZeros_2), type = "text")
stargazer(secondStageModelW1, secondStageModelWNoZeros, se = c(rse1_2, rseWNoZeros_2), type = "text")
# basically no change


#################################################################################
# Let's try using our layoffs data instead of theirs
#################################################################################

censusCountyData  <- readRDS("data/censusDataByCounty.rds")
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


# no zero values
datasetOurs <- BWDataByCounty %>%  filter(year == 2016) %>%  dplyr::rename(BWLayoffs = mfgLayoffs) %>% 
  left_join(ourDatasetBase %>% 
              group_by(wNw, state_fips, county_fips) %>% 
              filter(year %in% c(2012, 2013, 2014, 2015)) %>%  
              arrange(time) %>% 
              summarize(
                totalEmp = mean(totalEmp, na.rm = T), #latest
                mfgEmp = mean(mfgEmp, na.rm = T), #latest
                mfgLayoffs = sum(mfgLayoffs, na.rm = T),
                mfgLayoffsS = sum(mfgLayoffsS, na.rm = T),
                mfgNetChange  = sum(mfgNetChange, na.rm = T),
                population = mean(population, na.rm = T)
              )) %>%
  filter(mfgEmp != 0)  %>% 
  mutate(mfgLayoffs = mfgLayoffs/totalEmp,
         mfgLayoffsS = mfgLayoffsS/totalEmp,
         mfgNetChange = mfgNetChange/totalEmp)

ggplot(datasetOurs) + geom_point(aes(BWLayoffs, mfgLayoffsS))


firstStageModelOurs <- felm(
  data = datasetOurs %>%  filter(wNw == "total") %>%  filter(is.finite(mfgLayoffs)),
  formula = mfgLayoffs ~ bartik_leo5 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y | id_state)

secondStageModelOurs <- felm(
  data = datasetOurs %>%  filter(wNw == "total"),
  formula = ddem_votes_pct1 ~ firstStageModelOurs$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y | id_state)

rseOurs_1 <- coeftest(firstStageModelOurs, vcov = vcovHC(firstStageModelOurs, type = "HC0"))[,2]
rseOurs_2 <- coeftest(secondStageModelOurs, vcov = vcovHC(secondStageModelOurs, type = "HC0"))[,2]

firstStageModelWOurs <- felm(
  data = datasetOurs %>%  filter(wNw == "white") %>%  filter(is.finite(bartik_leo5_w2)),
  formula = mfgLayoffsW ~ bartik_leo5_w2 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y  + mfgLayoffsNW| id_state)

secondStageModelWOurs <- felm(
  data = datasetOurs  %>%  filter(wNw == "white") %>%  filter(is.finite(bartik_leo5_w2)),
  formula = ddem_votes_pct1 ~ firstStageModelWOurs$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + mfgLayoffsNW| id_state)

rseWOurs_1 <- coeftest(firstStageModelWOurs, vcov = vcovHC(firstStageModelWOurs, type = "HC0"))[,2]
rseWOurs_2 <- coeftest(secondStageModelWOurs, vcov = vcovHC(secondStageModelWOurs, type = "HC0"))[,2]


stargazer(secondStageModel1, secondStageModelOurs, se = c(rse1_2, rseOurs_2), type = "text")
stargazer(secondStageModelW1, secondStageModelWOurs, se = c(rse1_2, rseWOurs_2), type = "text")
# basically no change

datasetNetC <- datasetOurs

firstStageModelNetC <- felm(
  data = datasetNetC %>%  filter(wNw == "total") %>%  filter(is.finite(mfgNetChange)),
  formula = mfgNetChange ~ bartik_leo5 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y | id_state)

secondStageModelNetC <- felm(
  data = datasetNetC %>%  filter(wNw == "total") %>%  filter(is.finite(mfgNetChange)),
  formula = ddem_votes_pct1 ~ firstStageModelNetC$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y | id_state)

rseNetC_1 <- coeftest(firstStageModelNetC, vcov = vcovHC(firstStageModelNetC, type = "HC0"))[,2]
rseNetC_2 <- coeftest(secondStageModelNetC, vcov = vcovHC(secondStageModelNetC, type = "HC0"))[,2]

firstStageModelWNetC <- felm(
  data = datasetNetC %>%  filter(wNw == "white") %>%  filter(is.finite(bartik_leo5_w2)),
  formula = mfgNetChange ~ bartik_leo5_w2 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y  + mfgLayoffsNW| id_state)

secondStageModelWNetC <- felm(
  data = datasetOurs  %>%  filter(wNw == "white") %>%  filter(is.finite(bartik_leo5_w2)),
  formula = ddem_votes_pct1 ~ firstStageModelWNetC$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + mfgLayoffsNW| id_state)

rseWNetC_1 <- coeftest(firstStageModelWNetC, vcov = vcovHC(firstStageModelWNetC, type = "HC0"))[,2]
rseWNetC_2 <- coeftest(secondStageModelWNetC, vcov = vcovHC(secondStageModelWNetC, type = "HC0"))[,2]


stargazer(secondStageModel1, secondStageModelNetC, se = c(rse1_2, rseNetC_2), type = "text")
stargazer(secondStageModelW1, secondStageModelWNetC, se = c(rse1_2, rseWNetC_2), type = "text")




stargazer(firstStageModel1, firstStageModelOurs, se = c(rse1_1, rseOurs_1), type = "text")

ggplot(datasetNetC) + geom_point(aes(bartik_leo5, mfgNetChange ) )








# population-weighted
dataPopWeighted <- datasetNoZeros %>%  left_join(popData) 
resultsPopWeighted <- evalFun(dataPopWeighted, weights = dataPopWeighted$population)
resultsPopWeightedW <- evalFunW(dataPopWeighted  %>%  filter(is.finite(bartik_leo5_w2)), weights = (dataPopWeighted %>%  filter(is.finite(bartik_leo5_w2)))$population)

stargazer(results1$model2, resultsNoZeros$model2, resultsPopWeighted$model2, type = "text")
stargazer(results1W$model2, resultsNoZerosW$model2, resultsPopWeightedW$model2, type = "text")
qqnorm(results1$model1$residuals)
qqnorm(resultsPopWeighted$model1$residuals)
# note: we still have heteroskedasticity in the first leg of the regression 



########################################################
# Using our own Bartiks
########################################################
finalBartik <- readRDS("data/finalBartik.RDS")

countyLevelForRegs <- countyLevel %>%  filter(year == 2016)
datasetOurs <- countyLevelForRegs %>%  left_join(finalBartik) %>% 
  mutate(bartik_leo5 = bartikFinalTotal, 
         bartik_leo5_w2 = bartikFinalWhite,
         bartik_leo5_nw2 = bartikFinalNonwhite)



evalFunWFixed <- function(dataset, ...){
  
  firstStageModel1 <- felm(
    data = dataset,
    formula = mfgLayoffsW ~ bartik_leo5_w2  +
      LAU_unemp_rate_4y + pers_m_total_share_4y +
      pers_coll_share_4y| id_state, ...)
  
  firstStageModel2 <- felm(
    data = dataset,
    formula = mfgLayoffsNW ~ bartik_leo5_nw2   +
      LAU_unemp_rate_4y + pers_m_total_share_4y +
      pers_coll_share_4y| id_state, ...)
  
  secondStageModel <- felm(
    data = dataset,
    formula = ddem_votes_pct1 ~ firstStageModel1$fitted.values +
      LAU_unemp_rate_4y + pers_m_total_share_4y +
      pers_coll_share_4y + firstStageModel2$fitted.values| id_state, ...)
  
  rse1 <- coeftest(firstStageModel, vcov = vcovHC(firstStageModel, type = "HC0"))[,2]
  rse2 <- coeftest(secondStageModel, vcov = vcovHC(secondStageModel, type = "HC0"))[,2]
  print(c(secondStageModel$coefficients[[1]], rse2[[1]]) %>%  round(3) )
  return(list(model1 = firstStageModel, rse1 = rse1, model2 = secondStageModel, rse2 = rse2))
}

resultsOurs <- evalFun(datasetOurs %>%  filter(is.finite(bartik_leo5)))
resultsOursW <- evalFunW(datasetOurs %>%  filter(is.finite(bartik_leo5_w2)))
resultsOursWFixed <- evalFunWFixed(datasetOurs %>%  filter(is.finite(bartik_leo5_w2), is.finite(bartik_leo5_nw2)))
stargazer(resultsOurs$model2, se = resultsOurs$rse2, type = "text")
stargazer(resultsOursW$model2, se = resultsOursW$rse2, type = "text")
stargazer(resultsOursWFixed$model2, se = resultsOursWFixed$rse2, type = "text")


qqnorm(resultsOurs$model1$residuals)
qqnorm(resultsOursWFixed$model1$residuals)


stargazer(results1$model2, resultsOurs$model2, type = "text")
