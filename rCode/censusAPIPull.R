library(censusapi)


Sys.setenv(CENSUS_KEY="6ae2ddad490223ade51e50449164c5f1dc628bc1")
readRenviron("~/.Renviron")


apis <- listCensusApis() %>%  tibble()


apis %>% filter(grepl('QWI', title))
QWIName <- "timeseries/qwi/rh"

listCensusMetadata(
  name = QWIName, 
  type = "geography")

varnames <- listCensusMetadata(
  name = QWIName, 
  type = "variables")

# industry: NAICS codes, 31-33 is mfg

totalData <- getCensus(
  name = QWIName,
  vars = c("race", "ethnicity", "sex", "geography", "Emp"),
  region = "county:*",
  seasonadj = "U",
  regionin = "state:25", time = "from 2011 to 2015") %>% 
  rename(totalEmp = Emp) %>% tibble() 

mfgData <- getCensus(
  name = QWIName,
  vars = c("race", "ethnicity", "sex", "geography", "Emp"),
  industry = "31-33", 
  region = "county:*",
  seasonadj = "U",
  regionin = "state:25", time = "from 2011 to 2015") %>% 
  rename(mfgEmp = Emp)  %>% tibble()

aggregate <- totalData %>%  left_join(mfgData) %>%
  mutate(
    year = as.integer(substr(time,start = 0, stop =4)),
    totalEmp = as.numeric(totalEmp),
    mfgEmp = as.numeric(mfgEmp)
    )  

mfgWhite <- aggregate %>%  filter(time == "2011-Q1") %>% 
  filter(race == "A1" & ethnicity == "A1") %>% 
  select(state, county, mfgEmpWhite = mfgEmp, totalEmpWhite = totalEmp) %>% 
  mutate(mfgShareWhite = mfgEmpWhite/totalEmpWhite)

mfgNonwhite <- aggregate %>%  filter(time == "2011-Q1") %>% 
  filter(race != "A1" | ethnicity != "A1") %>% 
  filter(ethnicity != "A0" & race != "A0") %>%
  select(state, county, race, ethnicity, mfgEmp, totalEmp) %>% 
  group_by(state, county) %>%
  summarise(totalEmpNonwhite = sum(totalEmp, na.rm = T), mfgEmpNonwhite = sum(mfgEmp, na.rm = T))  %>% 
  mutate(mfgShareNonwhite = mfgEmpNonwhite/totalEmpNonwhite)


natlBartikWhite <- (natlBartik %>%  filter(race %in% c("white")))$bartikNatl
natlBartikNonwhite <- (natlBartik %>%  filter(race %in% c("nonwhite")))$bartikNatl

finalBartik <- mfgWhite %>%  left_join(mfgNonwhite) %>%
  mutate(natlBartikWhite = natlBartikWhite,
         natlBartikNonwhite = natlBartikNonwhite) %>% 
  mutate(bartikFinalWhite = natlBartikWhite * mfgShareWhite,
         bartikFinalNonwhite = natlBartikNonwhite * mfgShareNonwhite) %>% 
  select(state, county, bartikFinalWhite) %>% 
  arrange(desc(bartikFinalWhite))

MATest <- countyLevel %>%  filter(state_fips == 25) %>%  filter(year == 2016) %>% select(state_fips, pan_id, bartik_leo5_w2) %>% 
  arrange(desc(bartik_leo5_w2))

finalBartik$bartikFinalWhite * 100

MATest$bartik_leo5_w2*1000
