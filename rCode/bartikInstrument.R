# censusAPIPull.R gets the data and saves it

source("rcode/preamble.R")

################################################################
mfgData <- readRDS("data/mfgData.rds")
totalData <- readRDS("data/totalData.rds")

countyLevelRaw <- read_dta("OriginalMaterials/county_level.dta")

countyLevel <-countyLevelRaw %>% rename(
  mfgLayoffs = msl_pc4y2,
  mfgLayoffsW = msl_w_pc4y2,
  mfgLayoffsNW = msl_nw_pc4y2
)


natlResult <- readRDS("natlResult.rds")
################################################################

aggregate <- totalData %>%  left_join(mfgData) %>%
  mutate(
    year = as.integer(substr(time,start = 0, stop =4)),
    totalEmp = as.numeric(totalEmp),
    mfgEmp = as.numeric(mfgEmp)
  )  

dateStr <- "2011-Q4"

mfgTotal <- aggregate %>%  filter(time == dateStr) %>% 
  filter(race == "A0" & ethnicity == "A0") %>%
  select(state, county, mfgEmpTotal = mfgEmp, totalEmp = totalEmp) %>% 
  mutate(mfgShareTotal = mfgEmpTotal/totalEmp)

mfgWhite <- aggregate %>%  filter(time == dateStr) %>% 
  filter(race == "A1" & ethnicity == "A1") %>% 
  select(state, county, mfgEmpWhite = mfgEmp, totalEmpWhite = totalEmp) %>% 
  mutate(mfgShareWhite = mfgEmpWhite/totalEmpWhite)

mfgNonwhite <- aggregate %>%  filter(time == dateStr) %>% 
  filter(race != "A1" | ethnicity != "A1") %>% 
  filter(ethnicity != "A0" & race != "A0") %>%
  select(state, county, race, ethnicity, mfgEmp, totalEmp) %>% 
  group_by(state, county) %>%
  summarise(totalEmpNonwhite = sum(totalEmp, na.rm = T), mfgEmpNonwhite = sum(mfgEmp, na.rm = T))  %>% 
  mutate(mfgShareNonwhite = mfgEmpNonwhite/totalEmpNonwhite)



natlBartik <- natlResult$totalLayoffs/natlResult$totalEmpl
natlBartikWhite <- natlResult$whiteLayoffs/natlResult$whiteEmpl
natlBartikNonwhite <- NA

finalBartik <- mfgWhite %>%  left_join(mfgNonwhite) %>% left_join(mfgTotal) %>% 
  mutate(natlBartikWhite = natlBartikWhite,
         natlBartikNonwhite = natlBartikNonwhite,
         natlBartikTotal = natlBartik) %>% 
  mutate(bartikFinalWhite = natlBartikWhite * mfgShareWhite,
         bartikFinalNonwhite = natlBartikNonwhite * mfgShareNonwhite,
         bartikFinalTotal = natlBartikTotal * mfgShareTotal) %>% 
  select(state, county, bartikFinalNonwhite, bartikFinalWhite, bartikFinalTotal) %>% 
  arrange(desc(bartikFinalWhite)) %>% 
  filter(state == "01")



### Compare to B&W

# nationalEmp <- read.xlsx2("QWINationalByRE.xlsx",2) %>%  filter(date == dateStr)

MATest <- countyLevel %>% 
  filter(state_fips == stateNum) %>%  filter(year == 2016) %>% select(state_fips, pan_id, bartik_leo5, bartik_leo5_w2  )  %>% 
  arrange(desc(bartik_leo5_w2))

finalBartik %>%  arrange(county) %>% mutate(bartRank = rank(bartikFinalWhite)) %>% 
  arrange((bartikFinalWhite))

# MATest %>%  arrange(pan_id) %>% mutate(bartik_leo5_w2 = bartik_leo5_w2) %>% mutate(bartRank = rank(bartik_leo5_w2))  %>% 
#   arrange((bartRank)) %>%  mutate(whiteDiff = finalBartik$bartikFinalWhite/MATest$bartik_leo5_w2-1)

# diffs <- 
# finalBartik$bartikFinalWhite/MATest$bartik_leo5_w2-1
# finalBartik$bartikFinalTotal/MATest$bartik_leo5-1

# ggplot() + geom_point(mapping = aes(MATest$bartik_leo5_w2, finalBartik$bartikFinalWhite)) +
#   geom_line(mapping = aes(MATest$bartik_leo5_w2,MATest$bartik_leo5_w2))


ggplot() + geom_point(mapping = aes(MATest$bartik_leo5, finalBartik$bartikFinalTotal)) +
geom_line(mapping = aes(MATest$bartik_leo5,MATest$bartik_leo5))

