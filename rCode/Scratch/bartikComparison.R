
source("rCode/preamble.R")

finalBartikOurs <- readRDS("data/finalBartik.RDS")
bwData <- readRDS("data/BWCountyLevel.rds")

### Compare to B&W

# nationalEmp <- read.xlsx2("QWINationalByRE.xlsx",2) %>%  filter(date == dateStr)

BWBartik <- bwData %>%  filter(year == 2016) %>% dplyr::select(state_fips, county_fips, bartik_leo5, bartik_leo5_w2  )  %>% 
  arrange(desc(bartik_leo5_w2))

finalBartikOurs %>% mutate(bartRank = rank(bartikFinalWhite)) %>% 
  arrange((bartikFinalWhite))

# MATest %>%  arrange(pan_id) %>% mutate(bartik_leo5_w2 = bartik_leo5_w2) %>% mutate(bartRank = rank(bartik_leo5_w2))  %>% 
#   arrange((bartRank)) %>%  mutate(whiteDiff = finalBartik$bartikFinalWhite/MATest$bartik_leo5_w2-1)

# diffs <- 
# finalBartik$bartikFinalWhite/MATest$bartik_leo5_w2-1
# finalBartik$bartikFinalTotal/MATest$bartik_leo5-1

# ggplot() + geom_point(mapping = aes(MATest$bartik_leo5_w2, finalBartik$bartikFinalWhite)) +
#   geom_line(mapping = aes(MATest$bartik_leo5_w2,MATest$bartik_leo5_w2))

plotData <- BWBartik %>%  left_join(finalBartikOurs)
# TODO: we are missing some that they have??

ggplot() + geom_point(mapping = aes(plotData$bartik_leo5, plotData$bartikFinalTotal)) +
  geom_line(mapping = aes(plotData$bartik_leo5,plotData$bartik_leo5))
