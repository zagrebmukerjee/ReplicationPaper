stateNum <- 25

totalData <- getCensus(
  name = QWIName,
  vars = c("race", "geography", "industry", "Emp"),
  region = "county:001",
  seasonadj = "U",
  regionin = paste0("state:25"), time = "from 2011 to 2015") %>% 
  rename(totalEmp = Emp) %>% tibble() 

totalData %>%  distinct(industry) %>%  View()



### bartik on mfg only

natlBartikWhite <- (natlBartik %>%  filter(race %in% c("white")))$bartikMfgNatl
natlBartikNonwhite <- (natlBartik %>%  filter(race %in% c("nonwhite")))$bartikMfgNatl

finalBartikMfgOnly <- mfgWhite %>%  left_join(mfgNonwhite) %>%
  mutate(natlBartikWhite = natlBartikWhite,
         natlBartikNonwhite = natlBartikNonwhite) %>% 
  mutate(bartikFinalWhite = natlBartikWhite * mfgShareWhite,
         bartikFinalNonwhite = natlBartikNonwhite * mfgShareNonwhite) %>% 
  select(state, county, bartikFinalWhite) %>% 
  arrange(desc(bartikFinalWhite))



MATestMfgOnly <- countyLevel %>%  filter(state_fips == 25) %>%  filter(year == 2016) %>% select(state_fips, pan_id, bartik_leo5_w2) %>% 
  arrange(desc(bartik_leo5_w2))

finalBartikMfgOnly %>%  arrange(county) %>% mutate(bartRank = rank(bartikFinalWhite))
MATestMfgOnly %>%  arrange(pan_id) %>% mutate(bartik_leo5_w2 = bartik_leo5_w2 * 10) %>% mutate(bartRank = rank(bartik_leo5_w2))

finalBartik$bartikFinalWhite/MATest$bartik_leo5_w2/10-1




mfgDataRaw %>%  filter((county == "009" & time == "2011-Q1")) %>% filter(race == "A1") %>% 
  filter(ethnicity == "A1") %>%
  mutate(mfgEmp = as.numeric(mfgEmp)) %>%  View()




# industry: NAICS codes, 31-33 is mfg
# allNaicsStr <- c("311", "312", "313", "314", "315", "316","321","322","323","324","325","326","327","331","332","333","334","335","336","337","339")


# allNaicsStr <- c("31")

sectorGet <- function(str){
  getCensus(
    name = QWIName,
    vars = c("race", "ethnicity", "sex", "Emp"),
    industry = str, 
    region = "county:*",
    seasonadj = "U",
    regionin = paste0("state:", str_pad(stateNum, 2, pad = "0")), time = "from 2011 to 2015") %>% 
    rename(mfgEmp = Emp)  %>% tibble()
  
}



test <-   getCensus(
  name = QWIName,
  vars = c("ownercode", "Emp"),
  industry = "31-33",
  # ownercode = "A05",
  region = "county:*",
  seasonadj = "U",
  regionin = paste0("state:", str_pad(stateNum, 2, pad = "0")), time = "from 2011 to 2015") %>% 
  rename(mfgEmp = Emp)  %>% tibble()


