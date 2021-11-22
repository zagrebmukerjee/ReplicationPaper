source("rCode/preamble.R")
# before loading this RDS, you have to create it using the last block
# of censusAPIPull.R
popData <- readRDS("data/popData.rds")

countyLevelForRegs <- countyLevel %>%  filter(year == 2016)

evalFun <- function(dataset, ...){
  
  firstStageModel <- felm(
    data = dataset,
    formula = mfgLayoffs ~ bartik_leo5 +
      LAU_unemp_rate_4y + pers_m_total_share_4y +
      pers_coll_share_4y | id_state, ...)
  
  secondStageModel <- felm(
    data = dataset,
    formula = ddem_votes_pct1 ~ firstStageModel$fitted.values +
      LAU_unemp_rate_4y + pers_m_total_share_4y +
      pers_coll_share_4y | id_state, ...)
  
  rse1 <- coeftest(firstStageModel, vcov = vcovHC(firstStageModel, type = "HC0"))[,2]
  rse2 <- coeftest(secondStageModel, vcov = vcovHC(secondStageModel, type = "HC0"))[,2]
  print(c(secondStageModel$coefficients[[1]], rse2[[1]]) %>%  round(3) )
  return(list(model1 = firstStageModel, rse1 = rse1, model2 = secondStageModel, rse2 = rse2))
}


evalFunW <- function(dataset, ...){
  
  firstStageModel <- felm(
    data = dataset,
    formula = mfgLayoffsW ~ bartik_leo5_w2 +
      LAU_unemp_rate_4y + pers_m_total_share_4y +
      pers_coll_share_4y  + mfgLayoffsNW| id_state, ...)
  
  secondStageModel <- felm(
    data = dataset,
    formula = ddem_votes_pct1 ~ firstStageModel$fitted.values +
      LAU_unemp_rate_4y + pers_m_total_share_4y +
      pers_coll_share_4y + mfgLayoffsNW| id_state, ...)
  
  rse1 <- coeftest(firstStageModel, vcov = vcovHC(firstStageModel, type = "HC0"))[,2]
  rse2 <- coeftest(secondStageModel, vcov = vcovHC(secondStageModel, type = "HC0"))[,2]
  print(c(secondStageModel$coefficients[[1]], rse2[[1]]) %>%  round(3) )
  return(list(model1 = firstStageModel, rse1 = rse1, model2 = secondStageModel, rse2 = rse2))
}

plotFun <- function(dataset){ggplot(dataset, aes(x = bartik_leo5, y = mfgLayoffs)) + geom_point()}
plotFunW <- function(dataset){ggplot(dataset, aes(x = bartik_leo5_w2, y = mfgLayoffsW)) + geom_point()}


# the BW results
dataset1 <- countyLevelForRegs
results1 <- evalFun(dataset1)
results1W <- evalFunW(dataset1 %>%  filter(is.finite(bartik_leo5_w2)))
stargazer(results1$model2, se = results1$rse2, type = "text")
stargazer(results1W$model2, se = results1W$rse2, type = "text")
plotFun(dataset1)
plotFunW(dataset1)
qqnorm(results1$model1$residuals)
qqnorm(results1W$model1$residuals)


# no zero values
datasetNoZeros <- countyLevelForRegs %>%  filter(bartik_leo5 != 0, is.finite(bartik_leo5))
resultsNoZeros <- evalFun(datasetNoZeros)
resultsNoZerosW <- evalFunW(datasetNoZeros  %>%  filter(is.finite(bartik_leo5_w2)) )

stargazer(resultsNoZeros$model2, se = resultsNoZeros$rse2, type = "text")
stargazer(resultsNoZerosW$model2, se = resultsNoZerosW$rse2, type = "text")


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
