library(censusapi)
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
library(RobustSE)

statesList <- c("09","23","25","33","44","50","34","36","42","17","18","26","39","55","19","20","27","29","31","38","46","10","11","12","13","24","37","45","51","54","01","21","28","47","05","22","40","48","04","08","16","30","32","35","49","56","02","06","15","41","53"
)



Sys.setenv(CENSUS_KEY="6ae2ddad490223ade51e50449164c5f1dc628bc1")
readRenviron("~/.Renviron")


QWIName <- "timeseries/qwi/rh"
countyLevel <- readRDS("data/countyLevelClean.rds") #BW data cleaned up
