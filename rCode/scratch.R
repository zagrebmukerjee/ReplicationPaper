
evalFunW2 <- function(dataset, ...){
  
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
