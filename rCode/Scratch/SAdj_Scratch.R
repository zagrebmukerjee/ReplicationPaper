mfgData <- getCensus(
      name = QWIName,
      vars = c("race", "ethnicity", "sex", "FrmJbLs"),
      industry = "31-33",
      ownercode = "A05",
      region = "county:133",
      seasonadj = "U",
      regionin = paste0("state:", str_pad(51, 2, pad = "0")), time = "from 2011 to 2015") %>% 
      rename(layoffs = FrmJbLs)  %>% tibble()


