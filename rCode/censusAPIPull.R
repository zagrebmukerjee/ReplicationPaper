library(censusapi)
library(tidyverse)
library(stringr)
library(xlsx)
library(assertthat)
library(haven)

# source("rCode/nationalLevel.R")

Sys.setenv(CENSUS_KEY="6ae2ddad490223ade51e50449164c5f1dc628bc1")
readRenviron("~/.Renviron")


# apis <- listCensusApis() %>%  tibble()


# apis %>% filter(grepl('QWI', title))
QWIName <- "timeseries/qwi/rh"

listCensusMetadata(
  name = QWIName, 
  type = "geography")

varnames <- listCensusMetadata(
  name = QWIName, 
  type = "variables")


stateNum <- 25

totalData <- getCensus(
  name = QWIName,
  vars = c("race", "ethnicity", "sex", "Emp"),
  region = "county:*",
  seasonadj = "U",
  regionin = paste0("state:", str_pad(stateNum, 2, pad = "0")), time = "from 2011 to 2015") %>% 
  rename(totalEmp = Emp) %>% tibble() 

mfgData <-   getCensus(
  name = QWIName,
  vars = c("race", "ethnicity", "sex", "Emp"),
  industry = "31-33",
  ownercode = "A05",
  region = "county:*",
  seasonadj = "U",
  regionin = paste0("state:", str_pad(stateNum, 2, pad = "0")), time = "from 2011 to 2015") %>% 
  rename(mfgEmp = Emp)  %>% tibble()



################################################################

# mfgData <- mfgDataRaw %>%  group_by(race, ethnicity, sex, state, county, time)  %>%  
#   summarize(mfgEmp = sum(as.numeric(mfgEmp), na.rm = T)) 
statesList <- c("09","23","25","33","44","50","34","36","42","17","18","26","39","55","19","20","27","29","31","38","46","10","11","12","13","24","37","45","51","54","01","21","28","47","05","22","40","48","04","08","16","30","32","35","49","56","02","06","15","41","53"
)

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


### Compare to B&W

# nationalEmp <- read.xlsx2("QWINationalByRE.xlsx",2) %>%  filter(date == dateStr)

natlBartik <- totalLayoffs/totalEmpl
natlBartikWhite <- whiteLayoffs/whiteEmpl
natlBartikNonwhite <- NA

finalBartik <- mfgWhite %>%  left_join(mfgNonwhite) %>% left_join(mfgTotal) %>% 
  mutate(natlBartikWhite = natlBartikWhite,
         natlBartikNonwhite = natlBartikNonwhite,
         natlBartikTotal = natlBartikTotal) %>% 
  mutate(bartikFinalWhite = natlBartikWhite * mfgShareWhite,
         bartikFinalNonwhite = natlBartikNonwhite * mfgShareNonwhite,
         bartikFinalTotal = natlBartikTotal * mfgShareTotal) %>% 
  select(state, county, bartikFinalNonwhite, bartikFinalWhite, bartikFinalTotal) %>% 
  arrange(desc(bartikFinalWhite))

MATest <- countyLevel %>% 
  filter(state_fips == stateNum) %>%  filter(year == 2016) %>% select(state_fips, pan_id, bartik_leo5, bartik_leo5_w2  )  %>% 
  arrange(desc(bartik_leo5_w2))

finalBartik %>%  arrange(county) %>% mutate(bartRank = rank(bartikFinalWhite)) %>% 
  arrange((bartikFinalWhite))

MATest %>%  arrange(pan_id) %>% mutate(bartik_leo5_w2 = bartik_leo5_w2) %>% mutate(bartRank = rank(bartik_leo5_w2))  %>% 
  arrange((bartRank)) %>%  mutate(whiteDiff = finalBartik$bartikFinalWhite/MATest$bartik_leo5_w2-1)

# diffs <- 
  finalBartik$bartikFinalWhite/MATest$bartik_leo5_w2-1
  # finalBartik$bartikFinalTotal/MATest$bartik_leo5-1

ggplot() + geom_point(mapping = aes(MATest$bartik_leo5_w2, finalBartik$bartikFinalWhite)) +
  geom_line(mapping = aes(MATest$bartik_leo5_w2,MATest$bartik_leo5_w2))


# ggplot() + geom_point(mapping = aes(MATest$bartik_leo5, finalBartik$bartikFinalTotal)) +
  # geom_line(mapping = aes(MATest$bartik_leo5,MATest$bartik_leo5))

