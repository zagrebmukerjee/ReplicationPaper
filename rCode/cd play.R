# load voting data
library(data.table)
library(tidyverse)
library(stargazer)
library(sandwich)
library(lmtest)
library(knitr)
library(kableExtra)
library(readxl)
library(scales)
library(tigris)

# clear screen
shell("cls")

# clear enviornment
rm(list = ls()) # clear enviornment

options(scien = 999)


write.excel <- function(x,
                        row.names = FALSE,
                        col.names = TRUE,
                        ...) {
  write.table(
    x,
    "clipboard",
    sep = "\t",
    row.names = row.names,
    col.names = col.names,
    ...
  )
} 


# load input
model_input <- readRDS("model_5_input.rds")
model_results <- readRDS("model_5_results.rds")

voting <- fread("voting data.csv")
electoral_votes <- fread("electoral votes.csv")

state_match <- unique(model_input %>%
                        group_by() %>% filter(state_name != "") %>%
                        select(state, state_name))

# filter model input based on our regression filtering
model_input <- model_input %>%
  filter(
    is.finite(bartik_leo5_w2),
    is.finite(mfgNetChange_white),
    is.finite(mfgNetChange_nonwhite)
  )

model_small <- model_input %>%
  dplyr::select(
    id,
    state_fips,
    state,
    state_name,
    county_fips,
    county,
    year,
    dem_votes_pct1,
    ddem_votes_pct1,
    mfgNetChange_white,
    mfgNetChange_total,
    mfgNetChange_nonwhite
  )

# clean voting data
voting <- voting %>%
  dplyr::select(year, state, county_name, county_fips,
                party, candidatevotes) %>%
  filter(party == "DEMOCRAT" | party == "REPUBLICAN") %>%
  group_by(year, county_fips) %>%
  summarize(
    total_dem_rep_votes = sum(candidatevotes),
    dem_votes = sum(candidatevotes[party == "DEMOCRAT"]),
    rep_votes = sum(candidatevotes[party == "REPUBLICAN"])
  ) %>%
  mutate(dem_vote_share = dem_votes / total_dem_rep_votes)


# prep model input data to be merged to voting data
model_small <- model_small %>% mutate(
  c_fips_long = case_when(
    str_length(as.character(county_fips)) == 1 ~
      paste0("00", as.character(county_fips)),
    str_length(as.character(county_fips)) == 2 ~
      paste0("0", as.character(county_fips)),
    TRUE ~ as.character(county_fips)
  ),
  s_fips_long  = case_when(
    str_length(as.character(state_fips)) == 1 ~
      paste0("0", as.character(state_fips)),
    TRUE ~ as.character(state_fips)
  ),
  fips_long = paste0(s_fips_long, c_fips_long),
  fips_numeric = as.numeric(fips_long)
)

# join model input to observed voting data
model_small <- left_join(model_small,
                         voting,
                         by = c("fips_numeric" = "county_fips",
                                "year" = "year"))

for (j in 1:100) {
 
  # find the xth percentile in manufacturing layoffs (our measure) across counties
  
  ptile <- j/100
  percentile <- quantile(model_input$mfgNetChange_white, (ptile))
  
  temp <- model_small %>%
    mutate(# counterfactual net change in manufacturing job losses
      CF_netchange = percentile,
      
      # difference between counterfactual and actual net manufacturing job losses
      CF_d_netchange = CF_netchange - mfgNetChange_white)
  
   for (i in 1:100) {
    coef_draw <-
      rnorm(1, model_results$coefficients[1], model_results$se[1])
  
    temp <- temp %>%
      mutate(
        effect_size = CF_d_netchange * coef_draw,
        CF_dem_votes = dem_votes + effect_size * total_dem_rep_votes,
        CF_rep_votes = rep_votes - effect_size * total_dem_rep_votes
      )
    
    state_summary <- temp %>%
      group_by(state) %>%
      summarize(
        actual_trump_votes = sum(rep_votes),
        actual_dem_votes = sum(dem_votes),
        CF_trump_votes = sum(CF_rep_votes),
        CF_dem_votes = sum(CF_dem_votes)
      )
    
    state_summary <-
      left_join(state_summary, state_match, by = "state") %>%
      rename(state_short = state) %>%
      mutate(
        flipped_election = case_when(
          CF_trump_votes < CF_dem_votes &
          actual_trump_votes > actual_dem_votes ~
          1,
          CF_dem_votes < CF_trump_votes &
            actual_dem_votes > actual_trump_votes ~ 1,
          TRUE ~ 0
          
          
        )
      )
  
    elec_votes_tmp <- left_join(electoral_votes,
                                state_summary,
                                by = c("state" = "state_name"))
    
    elec_votes_tmp <- elec_votes_tmp %>%
      mutate(
        trump_votes_cf  = case_when(
          is.na(CF_trump_votes) ~
          as.integer(trump),
          
          CF_trump_votes > CF_dem_votes  &
          flipped_election == 1 ~ as.integer(votes),
          
          CF_trump_votes < CF_dem_votes  &
          flipped_election == 1 ~ as.integer(0),
          TRUE ~ as.integer(trump)
        ),
        
        clinton_votes_cf  = case_when(
          is.na(CF_trump_votes) ~
          as.integer(clinton),
          
          CF_trump_votes < CF_dem_votes  &
          flipped_election == 1 ~ as.integer(votes),
          
          CF_trump_votes > CF_dem_votes  &
          flipped_election == 1 ~ as.integer(0),
          TRUE ~ as.integer(clinton)
        )
      )
    
    result_tmp <- elec_votes_tmp %>%
      summarize(
        trump_votes_cf  = sum(trump_votes_cf),
        clinton_votes_cf = sum(clinton_votes_cf)
      )
    
    result_tmp$draw <- i
    result_tmp$ptile <- ptile
    
    if (exists("combined")) {
      combined <- rbind(combined, result_tmp)
    } else {
      combined <- result_tmp
    }
  }
}


summary <- combined %>%
  group_by(ptile) %>%
  arrange(trump_votes_cf) %>%
  summarize(median_result = clinton_votes_cf[row_number() == 50],
            lower = clinton_votes_cf[row_number() == 3],
            upper = clinton_votes_cf[row_number() == 98])



# first plot CI
ggplot(summary, aes(ptile, median_result)) + 
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                color = "blue") +
  geom_hline(yintercept = 270, color = "red") +
  theme_bw() +
  ylab("Outcome: Clinton Electoral College Votes\n") +
  xlab("Input Assumption: Percentile of Manufacturing Layoffs\n") +
  ggtitle("Counterfactual Presidential Election Simulation Results")




summary2 <- combined %>%
  mutate(hil_wins = case_when(clinton_votes_cf > 270 ~ 1,
                              TRUE ~ 0)) %>%
  group_by(ptile) %>%
  summarize(perc_wins =  sum(hil_wins)/n())


ggplot(summary2, aes(ptile, perc_wins)) + 
  geom_col(fill = "#4287f5")  +
  theme_bw() +
  ylab("Outcome: Frequency of Clinton Winning\nIn 100 Simulations") +
  xlab("Input Assumption: Percentile of Manufacturing Layoffs\n") +
  ggtitle("Counterfactual Presidential Election Simulation Results")




# just to midpoint

for (j in 1:100) {
  
  # find the xth percentile in manufacturing layoffs (our measure) across counties
  
  ptile <- j/100
  percentile <- quantile(model_input$mfgNetChange_white, (ptile))
  
  temp <- model_small %>%
    mutate(# counterfactual net change in manufacturing job losses
      CF_netchange = percentile,
      
      # difference between counterfactual and actual net manufacturing job losses
      CF_d_netchange = CF_netchange - mfgNetChange_white)
  
    coef_draw <- model_results$coefficients[1]
    temp <- temp %>%
      mutate(
        effect_size = CF_d_netchange * coef_draw,
        CF_dem_votes = dem_votes + effect_size * total_dem_rep_votes,
        CF_rep_votes = rep_votes - effect_size * total_dem_rep_votes
      )
    
    state_summary <- temp %>%
      group_by(state) %>%
      summarize(
        actual_trump_votes = sum(rep_votes),
        actual_dem_votes = sum(dem_votes),
        CF_trump_votes = sum(CF_rep_votes),
        CF_dem_votes = sum(CF_dem_votes)
      )
    
    state_summary <-
      left_join(state_summary, state_match, by = "state") %>%
      rename(state_short = state) %>%
      mutate(
        flipped_election = case_when(
          CF_trump_votes < CF_dem_votes &
            actual_trump_votes > actual_dem_votes ~
            1,
          CF_dem_votes < CF_trump_votes &
            actual_dem_votes > actual_trump_votes ~ 1,
          TRUE ~ 0
          
          
        )
      )
    
    elec_votes_tmp <- left_join(electoral_votes,
                                state_summary,
                                by = c("state" = "state_name"))
    
    elec_votes_tmp <- elec_votes_tmp %>%
      mutate(
        trump_votes_cf  = case_when(
          is.na(CF_trump_votes) ~
            as.integer(trump),
          
          CF_trump_votes > CF_dem_votes  &
            flipped_election == 1 ~ as.integer(votes),
          
          CF_trump_votes < CF_dem_votes  &
            flipped_election == 1 ~ as.integer(0),
          TRUE ~ as.integer(trump)
        ),
        
        clinton_votes_cf  = case_when(
          is.na(CF_trump_votes) ~
            as.integer(clinton),
          
          CF_trump_votes < CF_dem_votes  &
            flipped_election == 1 ~ as.integer(votes),
          
          CF_trump_votes > CF_dem_votes  &
            flipped_election == 1 ~ as.integer(0),
          TRUE ~ as.integer(clinton)
        )
      )
    
    result_tmp <- elec_votes_tmp %>%
      summarize(
        trump_votes_cf  = sum(trump_votes_cf),
        clinton_votes_cf = sum(clinton_votes_cf)
      )
    
    result_tmp$ptile <- ptile
    
    if (exists("combined2")) {
      combined2 <- rbind(combined2, result_tmp)
    } else {
      combined2 <- result_tmp
    }
  }


ggplot(combined2, aes( x = ptile, y = clinton_votes_cf)) + 
  geom_col( fill = "#4287f5") +
  geom_hline(yintercept = 270, color = "red", size = 1) +
  theme_bw() +
  ylab("Outcome: Clinton Electoral College Votes\n") +
  xlab("Input Assumption: Percentile of Manufacturing Layoffs\n") +
  ggtitle("Counterfactual Presidential Election Simulation Results (Using Point Estimate of Coefficient)\n")





