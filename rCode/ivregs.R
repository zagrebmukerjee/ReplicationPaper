source("rCode/preamble.R")


countyLevelRaw <- read_dta("OriginalMaterials/county_level.dta")

countyLevel <-countyLevelRaw %>% rename(
  mfgLayoffs = msl_pc4y2,
  mfgLayoffsW = msl_w_pc4y2,
  mfgLayoffsNW = msl_nw_pc4y2
) %>%  filter(year == 2016)


# firstStageModel <- felm(
#   data = countyLevel,
#   formula = mfgLayoffs ~ bartik_leo5 +
#     LAU_unemp_rate_4y + pers_m_total_share_4y +
#     pers_coll_share_4y | id_state)

# TODO: why do FELM and GLM differ

firstStageModel <- glm(
  data = countyLevel,
  formula = mfgLayoffs ~ bartik_leo5 +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y + id_state)

rse1 <- coeftest(firstStageModel, sandwich)[,2]
stargazer(firstStageModel, type = "text")
sprintf( "%0.2f", rse1)

# GIM(firstStageModel, B = 75, B2 = 75) # this takes so damn long

ggplot(countyLevel, aes(x = bartik_leo5, y = mfgLayoffs)) + geom_point()





