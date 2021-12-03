packages <- c("censusapi", "tidycensus",
              "tidyverse", "stringr", 
              "xlsx", "assertthat",
              "tictoc", "haven",
              "stargazer", "AER",
              "lfe", "lmtest", 
              "CBPS", "here")

if(length(which(!packages %in% installed.packages())) > 0) {
  install.packages(packages[!packages %in% installed.packages()])
}

library(censusapi)
library(tidycensus)
library(tidyverse)
library(stringr)
library(xlsx)
library(assertthat)
library(tictoc)
library(haven)
library(stargazer)
library(AER)
library(lfe)
library(lmtest)
library(CBPS)
library(here)





