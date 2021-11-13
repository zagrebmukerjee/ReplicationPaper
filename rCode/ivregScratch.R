source("rCode/preamble.R")


countyLevelRaw <- read_dta("OriginalMaterials/county_level.dta")

countyLevel <-countyLevelRaw %>% rename(
  mfgLayoffs = msl_pc4y2,
  mfgLayoffsW = msl_w_pc4y2,
  mfgLayoffsNW = msl_nw_pc4y2
) %>%  filter(year == 2016) %>% 
  mutate(logLayoffs = log(1+mfgLayoffs)) %>%
  mutate(logBartik = log(1+bartik_leo5)) 






firstStageModelLog <- felm(
  data = countyLevelSubset,
  formula = logLayoffs ~  logBartik +
    LAU_unemp_rate_4y + pers_m_total_share_4y +
    pers_coll_share_4y | id_state)


# TODO: why do FELM and GLM differ

# firstStageModel <- glm(
#   data = countyLevel,
#   formula = mfgLayoffs ~ bartik_leo5 +
#     LAU_unemp_rate_4y + pers_m_total_share_4y +
#     pers_coll_share_4y + id_state)

rse1 <- coeftest(firstStageModel, vcov = vcovHC(firstStageModel, type = "HC0"))[,2]
stargazer(firstStageModel, type = "text")
stargazer(firstStageModel, type = "text", se = rse1)
# sprintf( "%0.2f", rse1)

# GIM(firstStageModel, B = 75, B2 = 75) # this takes so damn long! 


ggplot(countyLevel, aes(x = bartik_leo5, y = mfgLayoffs)) + geom_point()
ggplot(countyLevel, aes(x = bartik_leo5, y = log(1+mfgLayoffs))) + geom_point()
ggplot(countyLevel, aes(x = log(1+ bartik_leo5), y = log(1+mfgLayoffs))) + geom_point()


rse2 <- coeftest(firstStageModelLog, sandwich)[,2]
stargazer(firstStageModelLog, type = "text")
stargazer(firstStageModelLog, type = "text", se = rse2)

# color ggplot by regio
# look at per capita question
# can weight by whatever is driving the heteroskedasticity
# want to bootstrap the whole procedure to get the correct standard errors
  # resample the data over and over - then get the dispersion of those coeffs
# censable
# GIM on other specifications
# box cox
# GIM on log-log
# talk to public health folks - they are also doing IV approach
# create QOIs would trump still have won
# wtf is going on with the counties that have 0 bartik and >0 mfg layoffs
