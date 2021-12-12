library(CBPS)
library(cobalt)


datasetOurs04Limited <- datasetOurs04 %>%  
  filter(is.finite(mfgNetChange_total), is.finite(mfgNetChange_white ), is.finite(mfgNetChange_nonwhite ), is.finite(bartik_leo5_w2))



plot(datasetOurs04Limited$ddem_votes_pct1, datasetOurs04Limited$mfgNetChange_total)

plot(datasetOurs04Limited$mfgNetChange_white, datasetOurs04Limited$msl_educ_prop)

fit04 <- CBPS(mfgNetChange_total ~ LAU_unemp_rate_4y + pers_m_total_share_4y +
                pers_coll_share_4y + white_counties_4y + msl_service_pc4y + mfgShare_total + totalEmp_total, data = datasetOurs04Limited, method = "exact")

fit04_white <- CBPS(mfgNetChange_white ~ LAU_unemp_rate_4y + pers_m_total_share_4y +
                pers_coll_share_4y + white_counties_4y + msl_service_pc4y + mfgShare_white + totalEmp_white, data = datasetOurs04Limited, method = "exact")

saveRDS(list(fit04 = fit04, fit04_white = fit04_white), "cbpsObjects.rds")

plot(fit04, main = "COV")
plot(fit04_white)

balance(fit04)
balance(fit04_white)


###############################################################
# Test to make sure my listwise deletion didn't do too much
################################################################

# model 2
firstStageModelOurs04L_2 <- felm(
  data = datasetOurs04Limited,
  formula = mfgNetChange_total ~ bartik_leo5  +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y   | id_state)

secondStageModelOurs04L_2 <- felm(
  data = datasetOurs04Limited,
  formula = ddem_votes_pct1 ~ firstStageModelOurs04L_2$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y  | id_state)

rseOurs04L2_1 <- coeftest(firstStageModelOurs04L_2, vcov = vcovHC(firstStageModelOurs04L_2, type = "HC0"))[,2]
rseOurs04L2_2 <- coeftest(secondStageModelOurs04L_2, vcov = vcovHC(secondStageModelOurs04L_2, type = "HC0"))[,2]

# model 3
firstStageModelOurs04L_3 <- felm(
  data =datasetOurs04Limited,
  formula = mfgNetChange_total ~ bartik_leo5  +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y  | id_state)

secondStageModelOurs04L_3 <- felm(
  data = datasetOurs04Limited,
  formula = ddem_votes_pct1 ~ firstStageModelOurs04L_3$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y | id_state)

rseOurs04L3_1 <- coeftest(firstStageModelOurs04L_3, vcov = vcovHC(firstStageModelOurs04L_3, type = "HC0"))[,2]
rseOurs04L3_2 <- coeftest(secondStageModelOurs04L_3, vcov = vcovHC(secondStageModelOurs04L_3, type = "HC0"))[,2]

# model 5
firstStageModelOurs04L_5 <- felm(
  data =datasetOurs04Limited,
  formula = mfgNetChange_white ~ bartik_leo5_w2  +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y   + mfgNetChange_nonwhite| id_state)

secondStageModelOurs04L_5 <- felm(
  data = datasetOurs04Limited,
  formula = ddem_votes_pct1 ~ firstStageModelOurs04L_5$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y  + mfgNetChange_nonwhite| id_state)

rseOurs04L5_1 <- coeftest(firstStageModelOurs04L_5, vcov = vcovHC(firstStageModelOurs04L_5, type = "HC0"))[,2]
rseOurs04L5_2 <- coeftest(secondStageModelOurs04L_5, vcov = vcovHC(secondStageModelOurs04L_5, type = "HC0"))[,2]


# model 6
firstStageModelOurs04L_6 <- felm(
  data = datasetOurs04Limited,
  formula = mfgNetChange_white ~ bartik_leo5_w2  +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y  + mfgNetChange_nonwhite| id_state)

secondStageModelOurs04L_6 <- felm(
  data = datasetOurs04Limited,
  formula = ddem_votes_pct1 ~ firstStageModelOurs04L_6$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y + mfgNetChange_nonwhite| id_state)

rseOurs04L6_1 <- coeftest(firstStageModelOurs04L_6, vcov = vcovHC(firstStageModelOurs04L_6, type = "HC0"))[,2]
rseOurs04L6_2 <- coeftest(secondStageModelOurs04L_6, vcov = vcovHC(secondStageModelOurs04L_6, type = "HC0"))[,2]

# compare model 2
stargazer(BW_secondStageModel2, secondStageModelOurs04_2,
          secondStageModelOurs04L_2, se = list(rse2_2, rseOurs042_2,
                                              rseOurs04L2_2), type = "text")

# model 3
stargazer(BW_secondStageModel3, 
          secondStageModelOurs04_3, 
          secondStageModelOurs04L_3,
          se = list(rse3_2, rseOurs043_2, rseOurs04L3_2), type = "text")

# model 5
stargazer(BW_secondStageModel5, secondStageModelOurs04_5, 
          secondStageModelOurs04L_5, se = list(rse5_2, rseOurs045_2,
                                              rseOurs04L5_2), type = "text")

# model 6
stargazer(BW_secondStageModel6, secondStageModelOurs04_6, 
          secondStageModelOurs04L_6,
          se = list(rse6_2, rseOurs046_2,
                    rseOurs04L6_2), type = "text")


###############################################################
# now with weights but no IV
###############################################################


# model 2
secondStageModelOurs04LW_2 <- felm(
  data = datasetOurs04Limited,
  formula = ddem_votes_pct1 ~ mfgNetChange_total +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y  | id_state, weights = fit04$weights)

rseOurs04LW2_2 <- coeftest(secondStageModelOurs04LW_2, vcov = vcovHC(secondStageModelOurs04LW_2, type = "HC0"))[,2]

# model 3
secondStageModelOurs04LW_3 <- felm(
  data = datasetOurs04Limited,
  formula = ddem_votes_pct1 ~ mfgNetChange_total +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y | id_state, weights = fit04$weights)

rseOurs04LW3_2 <- coeftest(secondStageModelOurs04LW_3, vcov = vcovHC(secondStageModelOurs04LW_3, type = "HC0"))[,2]

# model 5

secondStageModelOurs04LW_5 <- felm(
  data = datasetOurs04Limited,
  formula = ddem_votes_pct1 ~ mfgNetChange_white +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y  + mfgNetChange_nonwhite| id_state, weights = fit04_white$weights)

rseOurs04LW5_2 <- coeftest(secondStageModelOurs04LW_5, vcov = vcovHC(secondStageModelOurs04LW_5, type = "HC0"))[,2]


# model 6

secondStageModelOurs04LW_6 <- felm(
  data = datasetOurs04Limited,
  formula = ddem_votes_pct1 ~mfgNetChange_white +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y + mfgNetChange_nonwhite| id_state, weights = fit04_white$weights)

rseOurs04LW6_2 <- coeftest(secondStageModelOurs04LW_6, vcov = vcovHC(secondStageModelOurs04LW_6, type = "HC0"))[,2]

# compare model 2
stargazer(BW_secondStageModel2, secondStageModelOurs04L_2,
          secondStageModelOurs04LW_2, se = list(rse2_2, rseOurs04L2_2,
                                               rseOurs04LW2_2), type = "text")

# model 3
stargazer(BW_secondStageModel3, 
          secondStageModelOurs04L_3, 
          secondStageModelOurs04LW_3,
          se = list(rse3_2, rseOurs04L3_2, rseOurs04LW3_2), type = "text")

# model 5
stargazer(BW_secondStageModel5, secondStageModelOurs04L_5, 
          secondStageModelOurs04LW_5, se = list(rse5_2, rseOurs04L5_2,
                                               rseOurs04LW5_2), type = "text")

# model 6
stargazer(BW_secondStageModel6, secondStageModelOurs04L_6, 
          secondStageModelOurs04LW_6,
          se = list(rse6_2, rseOurs04L6_2,
                    rseOurs04LW6_2), type = "text")



# model 6
stargazer(BW_secondStageModel5, 
          secondStageModelOurs04LW_5,
          se = list(rse5_2,
                    rseOurs04LW5_2), type = "text")

stargazer(BW_secondStageModel6, 
          secondStageModelOurs04LW_6,
          se = list(rse6_2,
                    rseOurs04LW6_2), type = "text")

saveRDS(list(rseOurs04LW2_2 = rseOurs04LW2_2,
             rseOurs04LW3_2 = rseOurs04LW3_2,
             rseOurs04LW5_2 = rseOurs04LW5_2,
             rseOurs04LW6_2 = rseOurs04LW6_2,
             secondStageModelOurs04LW_2 = secondStageModelOurs04LW_2,
             secondStageModelOurs04LW_3 = secondStageModelOurs04LW_3,
             secondStageModelOurs04LW_5 = secondStageModelOurs04LW_5,
             secondStageModelOurs04LW_6 = secondStageModelOurs04LW_6),
        "cbpsResults.rds")

###############################################################
# now with weights and IV
###############################################################



# model 2
firstStageModelOurs04LWIV_2 <- felm(
  data = datasetOurs04Limited,
  formula = mfgNetChange_total ~ bartik_leo5  +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y  | id_state , weights = fit04$weights)

secondStageModelOurs04LWIV_2 <- felm(
  data = datasetOurs04Limited,
  formula = ddem_votes_pct1 ~ firstStageModelOurs04LWIV_2$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y | id_state , weights = fit04$weights)

rseOurs04LWIV2_1 <- coeftest(firstStageModelOurs04LWIV_2, vcov = vcovHC(firstStageModelOurs04LWIV_2, type = "HC0"))[,2]
rseOurs04LWIV2_2 <- coeftest(secondStageModelOurs04LWIV_2, vcov = vcovHC(secondStageModelOurs04LWIV_2, type = "HC0"))[,2]

# model 3
firstStageModelOurs04LWIV_3 <- felm(
  data =datasetOurs04Limited,
  formula = mfgNetChange_total ~ bartik_leo5  +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y | id_state , weights = fit04$weights)

secondStageModelOurs04LWIV_3 <- felm(
  data = datasetOurs04Limited,
  formula = ddem_votes_pct1 ~ firstStageModelOurs04LWIV_3$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y| id_state , weights = fit04$weights)

rseOurs04LWIV3_1 <- coeftest(firstStageModelOurs04LWIV_3, vcov = vcovHC(firstStageModelOurs04LWIV_3, type = "HC0"))[,2]
rseOurs04LWIV3_2 <- coeftest(secondStageModelOurs04LWIV_3, vcov = vcovHC(secondStageModelOurs04LWIV_3, type = "HC0"))[,2]

# model 5
firstStageModelOurs04LWIV_5 <- felm(
  data =datasetOurs04Limited,
  formula = mfgNetChange_white ~ bartik_leo5_w2  +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y   + mfgNetChange_nonwhite, weights = fit04_white$weights)

secondStageModelOurs04LWIV_5 <- felm(
  data = datasetOurs04Limited,
  formula = ddem_votes_pct1 ~ firstStageModelOurs04LWIV_5$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y  + mfgNetChange_nonwhite, weights = fit04_white$weights)

rseOurs04LWIV5_1 <- coeftest(firstStageModelOurs04LWIV_5, vcov = vcovHC(firstStageModelOurs04LWIV_5, type = "HC0"))[,2]
rseOurs04LWIV5_2 <- coeftest(secondStageModelOurs04LWIV_5, vcov = vcovHC(secondStageModelOurs04LWIV_5, type = "HC0"))[,2]


# model 6
firstStageModelOurs04LWIV_6 <- felm(
  data = datasetOurs04Limited,
  formula = mfgNetChange_white ~ bartik_leo5_w2  +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y  + mfgNetChange_nonwhite, weights = fit04_white$weights)

secondStageModelOurs04LWIV_6 <- felm(
  data = datasetOurs04Limited,
  formula = ddem_votes_pct1 ~ firstStageModelOurs04LWIV_6$fitted.values +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + white_counties_4y + msl_service_pc4y + mfgNetChange_nonwhite, weights = fit04_white$weights)

rseOurs04LWIV6_1 <- coeftest(firstStageModelOurs04LWIV_6, vcov = vcovHC(firstStageModelOurs04LWIV_6, type = "HC0"))[,2]
rseOurs04LWIV6_2 <- coeftest(secondStageModelOurs04LWIV_6, vcov = vcovHC(secondStageModelOurs04LWIV_6, type = "HC0"))[,2]

# compare model 2
stargazer(BW_secondStageModel2, secondStageModelOurs04_2,
          secondStageModelOurs04LWIV_2, se = list(rse2_2, rseOurs04L2_2,
                                               rseOurs04LWIV2_2), type = "text")

# model 3
stargazer(BW_secondStageModel3, 
          secondStageModelOurs04_3, 
          secondStageModelOurs04LWIV_3,
          se = list(rse3_2, rseOurs04L3_2, rseOurs04LWIV3_2), type = "text")

# model 5
stargazer(BW_secondStageModel5, secondStageModelOurs04L_5, 
          secondStageModelOurs04LWIV_5, se = list(rse5_2, rseOurs04L5_2,
                                               rseOurs04LWIV5_2), type = "text")

# model 6
stargazer(BW_secondStageModel6, secondStageModelOurs04_6, 
          secondStageModelOurs04LWIV_6,
          se = list(rse6_2, rseOurs04LWIV6_2,
                    rseOurs04L6_2), type = "text")

