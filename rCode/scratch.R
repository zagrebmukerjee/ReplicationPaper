library(tidyverse)
library(xlsx)
library(assertthat)
library(haven)

countyLevelRaw <- read_dta("OriginalMaterials/county_level.dta")
natlEmplRaw <- read.xlsx2("OriginalMaterials/Table 1.xlsx",1) %>%  tibble()
natlLayoffRaw <- read.xlsx2("OriginalMaterials/Table 1.xlsx",2) %>%  tibble()

# convert to #s and filter out totally empty rows
tmp <- bind_cols(lapply(natlEmplRaw, as.numeric)) %>%
  tibble() %>%  filter(if_any(everything(), ~ !is.na(.x))) 

tmp2 <- bind_cols(lapply(natlLayoffRaw, as.numeric)) %>%
  tibble() %>%  filter(if_any(everything(), ~ !is.na(.x))) 

natlEmpl <- tmp %>% 
  mutate(year = seq(to = 2019, by = 1, length.out = nrow(tmp))) %>% 
  select(year, everything()) %>% 
  mutate(white = White.Alone,
         nonwhite = rowSums(across(!c(starts_with("year"), starts_with("White"))))) # wrong order...

natlLayoff <- tmp2 %>% 
  mutate(year = seq(to = 2018, by = 1, length.out = nrow(tmp2))) %>% 
  select(year, everything())  %>% 
  mutate(white = White.Alone,
         nonwhite = rowSums(across(!c(starts_with("year"), starts_with("White")))))

stopifnot(ncol(natlEmpl)==ncol(natlLayoff))
nRaces <- ncol(natlEmpl%>%  select(-year))
raceNames <- colnames(natlEmpl %>%  select(-year))

natlLevelRaw <- bind_rows(
  natlEmpl %>% mutate(type = "empl") %>%  pivot_longer(cols = (!year & ! type)),
  natlLayoff %>% mutate(type = "layoff") %>%  pivot_longer(cols = (!year & ! type))) %>% 
  rename(race = name)


natlLevel <- natlLevelRaw %>%  filter(year > 2012 & year < 2019) %>% 
  group_by(race,type ) %>%  summarize(value = mean(value)) %>% 
  pivot_wider(id_cols = race, names_from = type,  values_from = value )

countyLevel <-countyLevelRaw %>% rename(
  mfgLayoffs = msl_pc4y2,
  mfgLayoffsW = msl_w_pc4y2,
  mfgLayoffsNW = msl_nw_pc4y2
)

# approximate nat'l component of bartik instrument by race
natlBartik <- natlLevel %>% mutate(bartikNatl = layoff/empl)



