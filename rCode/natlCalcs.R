totalEmpFun <- function(stateNum){ getCensus(
  name = QWIName,
  vars = c("race", "ethnicity", "Emp"),
  region = paste0("state:", str_pad(stateNum, 2, pad = "0")),
  seasonadj = "U",
  time = "from 2012 to 2015") %>% 
    rename(totalEmp = Emp) %>% tibble() 
}

mfgEmpFun <- function(stateNum){ getCensus(
  name = QWIName,
  vars = c("race", "ethnicity", "Emp"),
  industry = "31-33",
  region = paste0("state:", str_pad(stateNum, 2, pad = "0")),
  seasonadj = "U",
  time = "from 2012 to 2015") %>% 
    rename(mfgEmp = Emp) %>% tibble() 
}

mfgLayoffFun <- function(stateNum){ getCensus(
  name = QWIName,
  vars = c("race", "ethnicity", "FrmJbLsS", "FrmJbLs", "FrmJbC"),
  industry = "31-33",
  region = paste0("state:", str_pad(stateNum, 2, pad = "0")),
  seasonadj = "U",
  time = "from 2012 to 2015") %>% 
    rename(mfgLayoffs = FrmJbLs, 
           mfgLayoffsS = FrmJbLsS,
           mfgNetChange = FrmJbC) %>% tibble() 
}

# pull the stuff at state level
allEmpRaw <- lapply(statesList, function(s){
  totalEmpFun(s) %>%  full_join(mfgEmpFun(s)) %>% full_join(mfgLayoffFun(s))
})

#data at the state level
aggNatlState <- bind_rows(allEmpRaw) 

nQuarters <- length(unique(aggNatlState$time))

# aggregate over years and states - there's 16 quarters
aggNatl <- aggNatlState %>%  group_by(race, ethnicity) %>%  summarize(
  totalEmp = sum(as.numeric(totalEmp), na.rm = T)/nQuarters, 
  mfgEmp = sum(as.numeric(mfgEmp), na.rm = T)/nQuarters,
  mfgLayoffs = sum(as.numeric(mfgLayoffs), na.rm = T)/nQuarters,
  mfgLayoffsS = sum(as.numeric(mfgLayoffsS), na.rm = T)/nQuarters,
  mfgNetChange = sum(as.numeric(mfgNetChange), na.rm = T)/nQuarters
) %>%  filter(race != "A0", ethnicity != "A0") # filter out totals

totalLayoffs <- aggNatl %>%  ungroup() %>%  summarize(totalLayoffs = sum(mfgLayoffs))
totalNetChange <- aggNatl %>%  ungroup() %>%  summarize(totalNetChange = sum(mfgNetChange))
totalEmpl <- aggNatl %>%  ungroup() %>%  summarize(totalEmp = sum(totalEmp))

# NOTE: nonhispanic
whiteLayoffs <- aggNatl %>%  filter(race == "A1", ethnicity == "A1") %>%  summarize(whiteLayoffs = sum(mfgLayoffs))
whiteNetChange <- aggNatl %>%  filter(race == "A1", ethnicity == "A1") %>%  summarize(whiteNetChange = sum(mfgNetChange))
whiteEmpl <- aggNatl %>%  filter(race == "A1", ethnicity == "A1") %>%  summarize(whiteEmp = sum(totalEmp))

# nonwhite or white hispanic
nonwhiteLayoffs <- aggNatl %>%  filter(race != "A1"| ethnicity != "A1") %>%  ungroup() %>%   summarize(nonwhiteLayoffs = sum(mfgLayoffs))
nonwhiteNetChange <- aggNatl %>%  filter(race != "A1"| ethnicity != "A1") %>%  ungroup() %>%   summarize(nonwhiteNetChange = sum(mfgNetChange))
nonwhiteEmpl <- aggNatl %>%  filter(race != "A1"| ethnicity != "A1") %>%  ungroup() %>%  summarize(nonwhiteEmp = sum(totalEmp))



saveRDS(list(
  totalLayoffs = totalLayoffs$totalLayoffs,
  totalNetChange = totalNetChange$totalNetChange,
  totalEmpl=totalEmpl$totalEmp,
  whiteLayoffs = whiteLayoffs$whiteLayoffs,
  whiteNetChange = whiteNetChange$whiteNetChange,
  whiteEmpl = whiteEmpl$whiteEmp,
  nonwhiteLayoffs = nonwhiteLayoffs$nonwhiteLayoffs,
  nonwhiteNetChange = nonwhiteNetChange$nonwhiteNetChange,
  nonwhiteEmpl = nonwhiteEmpl$nonwhiteEmp), "data/natlResult.rds")



