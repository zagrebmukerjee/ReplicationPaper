source("rCode/preamble.R")
# before loading this RDS, you have to create it using the last block
# of censusAPIPull.R
readRDS("data/popData.rds")


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

plotFun <- function(dataset){ggplot(dataset, aes(x = bartik_leo5, y = mfgLayoffs)) + geom_point()}


# the BW results
dataset1 <- countyLevel
results1 <- evalFun(dataset1)

# no zero values
datasetNoZeros <- countyLevel %>%  filter(bartik_leo5 != 0)
resultsNoZeros <- evalFun(datasetNoZeros)


# no zero values
dataPopWeighted <- datasetNoZeros %>%  left_join(popData) 
resultsPopWeighted <- evalFun(dataPopWeighted, weights = dataPopWeighted$population)





