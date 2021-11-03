*     ***************************************************************** *;
*     ***************************************************************** *;
*       File-Name:      county_level.do                                 *;
*       Date:           12/28/2020                                      *;
*       Authora:        Baccini & Weymouth                              *;
*       Purpose:        Replication of "Gone For Good:                  *;
*						Deindustrialization, White Voter Backlash,      *;
*						and US Presidential Voting"                     *;                                                                      
*       Input Files:    county_level.dta 						        *; 
*       Output Files:   county_logfile.log                              *;
*       Machine:        Office                                          *;
*       Program: 		Stata 15                                        *;
*     ****************************************************************  *;
*     ****************************************************************  *;

*Note: to run this dofile, you need to install the following stata comands: outreg2, ivreg2, and xtivreg2 (plus related commands).
* ZAG: you also need ranktest


* Setting initial commands
clear
clear matrix
clear mata
set maxvar 50000
set matsize 11000
set more off 

* Loading the dataset
* cd "C:\Users\lbacci\Dropbox\Projects\Baccini Weymouth 2017\April2018_Data_Legislators_Voting\Replication\.A_Replication"
use county_level, clear
* cd "C:\Users\lbacci\Dropbox\Projects\Baccini Weymouth 2017\April2018_Data_Legislators_Voting\Replication\.AA Check"
* ZAG: tweaked logging
log using logs\county_logfile.log, name(county) 

* Setting the county-year panel
xtset pan_id year


########## MAIN TEXT
* Table 2 (county-level analysis)
*1
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y (msl_pc4y2= bartik_leo5) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using table2.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, Service Layoffs, No, State fixed effects, Yes) keep(msl_pc4y2) label 
*2
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y (msl_pc4y2= bartik_leo5) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using table2.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, No, State fixed effects, Yes) keep(msl_pc4y2) label append
*3
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y msl_service_pc4y (msl_pc4y2= bartik_leo5) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using table2.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, Yes, State fixed effects, Yes) keep(msl_pc4y2) label append
*4
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using table2.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, Service Layoffs, No, State fixed effects, Yes) keep(msl_w_pc4y2 msl_nw_pc4y2) label append
*5
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using table2.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, No, State fixed effects, Yes) keep(msl_w_pc4y2 msl_nw_pc4y2) label append
*6
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y msl_service_pc4y msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using table2.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, Yes, State fixed effects, Yes) keep(msl_w_pc4y2 msl_nw_pc4y2 ) label append

*Table 6 (county Level, 2008-2016)
*1
xi: xtivreg2 ddem_votes_pct1 LAU_unemp_rate_4y it1 pers_m_total_share_4y_y1 pers_coll_share_4y_y1 white_counties_4y white_counties_4y_y1 (msl_pc4y2 it_new= bartik_leo5 it_instr2) i.id_state_y if year>=2008, cluster(pan_id) fe partial(i.id_state_y) first
outreg2 using table6.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, County fixed effects, Yes, State-year fixed effects, Yes) keep(msl_pc4y2 it_new) label 
*2
xi: xtivreg2 ddem_votes_pct1 LAU_unemp_rate_4y it1 pers_m_total_share_4y_y1 pers_coll_share_4y_y1 white_counties_4y white_counties_4y_y1 msl_nw_pc4y2 it_nw2 (msl_w_pc4y2 it_w2=bartik_leo5_w2 it_w2_instr_new) i.id_state_y if year>=2008, cluster(pan_id) fe partial(i.id_state_y) first
outreg2 using table6.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, County fixed effects, Yes, State-year fixed effects, Yes) keep(msl_w_pc4y2 it_w2 msl_nw_pc4y2 it_nw2) label  append
*3
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y msl_nw_pc4y2  (msl_w_pc4y2 =bartik_leo5_w2) i.id_state if year==2008, r partial(i.id_state) first
outreg2 using table6.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, County fixed effects, No, State-year fixed effects, Yes) keep(msl_nw_pc4y2 it_nw2 msl_w_pc4y2 it_w2) label append
*4
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y msl_nw_pc4y2  (msl_w_pc4y2 =bartik_leo5_w2 ) i.id_state if year==2012, r partial(i.id_state) first
outreg2 using table6.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, County fixed effects, No, State-year fixed effects, Yes) keep(msl_nw_pc4y2 it_nw2 msl_w_pc4y2 it_w2) label append


######## APPENDIX A

* Table A1
corr bartik_leo5 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y msl_service_pc4y if year==2016


########## APPENDIX B

* Table B1 (OLS)
*1
xi: reg ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y msl_pc4y2 i.id_state if year==2016, r 
outreg2 using tableB1.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, Service Layoffs, No, State fixed effects, Yes) keep(msl_pc4y2) label 
*2
xi: reg ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y msl_pc4y2 i.id_state if year==2016, r 
outreg2 using tableB1.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, No, State fixed effects, Yes) keep(msl_pc4y2) label append
*3
xi: reg ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y msl_service_pc4y msl_pc4y2 i.id_state if year==2016, r 
outreg2 using tableB1.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, Yes, State fixed effects, Yes) keep(msl_pc4y2) label append
*4
xi: reg ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y msl_nw_pc4y2 msl_w_pc4y2 i.id_state if year==2016, r 
outreg2 using tableB1.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, Service Layoffs, No, State fixed effects, Yes) keep(msl_w_pc4y2 msl_nw_pc4y2) label append
*5
xi: reg ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y msl_nw_pc4y2 msl_w_pc4y2 i.id_state if year==2016, r 
outreg2 using tableB1.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, No, State fixed effects, Yes) keep(msl_w_pc4y2 msl_nw_pc4y2) label append
*6
xi: reg ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y msl_service_pc4y msl_nw_pc4y2 msl_w_pc4y2 i.id_state if year==2016, r 
outreg2 using tableB1.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, Yes, State fixed effects, Yes) keep(msl_w_pc4y2 msl_nw_pc4y2 ) label append

* Table B2 (first stage)
*1
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y (msl_pc4y2= bartik_leo5) i.id_state if year==2016, r partial(i.id_state) first savefirst
est restore _ivreg2_msl_pc4y2
outreg2 using tableB2.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, Service Layoffs, No, State fixed effects, Yes) keep(bartik_leo5) label 
*4
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) i.id_state if year==2016, r partial(i.id_state) first savefirst
est restore _ivreg2_msl_w_pc4y2
outreg2 using tableB2.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, Service Layoffs, No, State fixed effects, Yes) keep(bartik_leo5_w2) label append

* Table B3
*Florida
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) if state_name=="Florida" & year==2016, first
gen dem_p=_b[LAU_unemp_rate_4y]*LAU_unemp_rate_4y+_b[msl_nw_pc4y2]*msl_nw_pc4y2+_b[msl_w_pc4y2]*msl_w_pc4y2 if state_name=="Florida" & year==2016
gen dem_p2=_b[LAU_unemp_rate_4y]*LAU_unemp_rate_4y+_b[msl_nw_pc4y2]*msl_nw_pc4y2+_b[msl_w_pc4y2]*0.01 if state_name=="Florida" & year==2016
egen totdem_p=mean(dem_p) if state_name=="Florida" & year==2016
egen totdem_p2=mean(dem_p2) if state_name=="Florida" & year==2016
gen dem_effect=(totdem_p-totdem_p2)/totdem_p2 if state_name=="Florida" & year==2016
sum dem_effect if state_name=="Florida" & year==2016
drop dem_p-dem_effect
*Michigan
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) if state_name=="Michigan" & year==2016, first
gen dem_p=_b[LAU_unemp_rate_4y]*LAU_unemp_rate_4y+_b[msl_nw_pc4y2]*msl_nw_pc4y2+_b[msl_w_pc4y2]*msl_w_pc4y2 if state_name=="Michigan" & year==2016
gen dem_p2=_b[LAU_unemp_rate_4y]*LAU_unemp_rate_4y+_b[msl_nw_pc4y2]*msl_nw_pc4y2+_b[msl_w_pc4y2]*0.05 if state_name=="Michigan" & year==2016
egen totdem_p=mean(dem_p) if state_name=="Michigan" & year==2016
egen totdem_p2=mean(dem_p2) if state_name=="Michigan" & year==2016
gen dem_effect=(totdem_p-totdem_p2)/totdem_p2 if state_name=="Michigan" & year==2016
sum dem_effect if state_name=="Michigan" & year==2016
drop dem_p-dem_effect
*Pennsylvania
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) if state_name=="Pennsylvania" & year==2016, first
gen dem_p=_b[LAU_unemp_rate_4y]*LAU_unemp_rate_4y+_b[msl_nw_pc4y2]*msl_nw_pc4y2+_b[msl_w_pc4y2]*msl_w_pc4y2 if state_name=="Pennsylvania" & year==2016
gen dem_p2=_b[LAU_unemp_rate_4y]*LAU_unemp_rate_4y+_b[msl_nw_pc4y2]*msl_nw_pc4y2+_b[msl_w_pc4y2]*0.04 if state_name=="Pennsylvania" & year==2016
egen totdem_p=mean(dem_p) if state_name=="Pennsylvania" & year==2016
egen totdem_p2=mean(dem_p2) if state_name=="Pennsylvania" & year==2016
gen dem_effect=(totdem_p-totdem_p2)/totdem_p2 if state_name=="Pennsylvania" & year==2016
sum dem_effect if state_name=="Pennsylvania" & year==2016
drop dem_p-dem_effect
*Wisconsin
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) if state_name=="Wisconsin" & year==2016, first
gen dem_p=_b[LAU_unemp_rate_4y]*LAU_unemp_rate_4y+_b[msl_nw_pc4y2]*msl_nw_pc4y2+_b[msl_w_pc4y2]*msl_w_pc4y2 if state_name=="Wisconsin" & year==2016
gen dem_p2=_b[LAU_unemp_rate_4y]*LAU_unemp_rate_4y+_b[msl_nw_pc4y2]*msl_nw_pc4y2+_b[msl_w_pc4y2]*0.05 if state_name=="Wisconsin" & year==2016
egen totdem_p=mean(dem_p) if state_name=="Wisconsin" & year==2016
egen totdem_p2=mean(dem_p2) if state_name=="Wisconsin" & year==2016
gen dem_effect=(totdem_p-totdem_p2)/totdem_p2 if state_name=="Wisconsin" & year==2016
sum dem_effect if state_name=="Wisconsin" & year==2016
drop dem_p-dem_effect

* Table B4 (other outcomes)
*1
xi: ivreg2 dem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y (msl_pc4y2= bartik_leo5) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB4.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, Sevice Layoffs, No, State fixed effects, Yes) keep(msl_pc4y2) label
*2
xi: ivreg2 dem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB4.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, Sevice Layoffs, No, State fixed effects, Yes) keep(msl_nw_pc4y2 msl_w_pc4y2) label append
*3
xi: ivreg2 ddem_votes_pct1_third LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y (msl_pc4y2= bartik_leo5) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB4.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, Sevice Layoffs, No, State fixed effects, Yes) keep(msl_pc4y2) label append
*4
xi: ivreg2 ddem_votes_pct1_third LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB4.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, Sevice Layoffs, No, State fixed effects, Yes) keep(msl_nw_pc4y2 msl_w_pc4y2) label append
*5
xi: ivreg2 dturn_pres1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y (msl_pc4y2= bartik_leo5) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB4.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, Sevice Layoffs, No, State fixed effects, Yes) keep(msl_pc4y2) label append
*6
xi: ivreg2 dturn_pres1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB4.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, Sevice Layoffs, No, State fixed effects, Yes) keep(msl_nw_pc4y2 msl_w_pc4y2) label append

* Table B5 (other confounders)
*1
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y  pers_m_total_share_4y pers_coll_share_4y msl_educ_prop msl_male_prop msl_age_prop white_counties_4y (msl_pc4y2= bartik_leo5) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB5.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Sevice Layoffs, No, Other Layoffs, Yes, State fixed effects, Yes) keep(msl_pc4y2) label
*2
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y  pers_m_total_share_4y pers_coll_share_4y msl_educ_prop msl_male_prop msl_age_prop white_counties_4y msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB5.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Sevice Layoffs, No, Other Layoffs, Yes, State fixed effects, Yes) keep(msl_w_pc4y2 msl_nw_pc4y2) label append
*3
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y  pers_m_total_share_4y pers_coll_share_4y white_counties_4y autor (msl_pc4y2= bartik_leo5) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB5.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Sevice Layoffs, No, Other Layoffs, No, State fixed effects, Yes) keep(msl_pc4y2 autor) label append
*4
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y  pers_m_total_share_4y pers_coll_share_4y white_counties_4y autor msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB5.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Sevice Layoffs, No, Other Layoffs, No, State fixed effects, Yes) keep(msl_w_pc4y2 msl_nw_pc4y2 autor) label append
*5
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y  pers_m_total_share_4y pers_coll_share_4y white_counties_4y (msl_pc4y2 autor= bartik_leo5 autor_instr) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB5.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Sevice Layoffs, No, Other Layoffs, No, State fixed effects, Yes) keep(msl_pc4y2 autor) label append
*6
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y  pers_m_total_share_4y pers_coll_share_4y white_counties_4y msl_nw_pc4y2 (msl_w_pc4y2 autor=bartik_leo5_w2 autor_instr) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB5.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Sevice Layoffs, No, Other Layoffs, No, State fixed effects, Yes) keep(msl_w_pc4y2 msl_nw_pc4y2 autor) label append

* Table B6 (district fixed effects)
*1
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y (msl_pc4y2= bartik_leo5) i.cd_id if year==2016, r partial(i.cd_id) first
outreg2 using tableB6.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, District fixed effects, Yes) keep(msl_pc4y2) label
*2
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y (msl_pc4y2= bartik_leo5) i.cd_id if year==2016, r partial(i.cd_id) first
outreg2 using tableB6.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, District fixed effects, Yes) keep(msl_pc4y2) label append
*3
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y msl_service_pc4y (msl_pc4y2= bartik_leo5) i.cd_id if year==2016, r partial(i.cd_id) first
outreg2 using tableB6.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, District fixed effects, Yes) keep(msl_pc4y2 msl_service_pc4y) label append
*4
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) i.cd_id if year==2016, r partial(i.cd_id) first
outreg2 using tableB6.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, District fixed effects, Yes) keep(msl_w_pc4y2 msl_nw_pc4y2) label append
*5
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) i.cd_id if year==2016, r partial(i.cd_id) first
outreg2 using tableB6.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, District fixed effects, Yes) keep(msl_w_pc4y2 msl_nw_pc4y2) label append
*6
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y msl_service_pc4y msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) i.cd_id if year==2016, r partial(i.cd_id) first
outreg2 using tableB6.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, District fixed effects, Yes) keep(msl_w_pc4y2 msl_nw_pc4y2) label append

* Table B7 (cumulative layoffs)
*1
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y (msl_cum_pc= bartik_leo5c) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB7.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, State fixed effects, Yes) keep(msl_cum_pc) label
*2
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y (msl_cum_pc= bartik_leo5c) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB7.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, State fixed effects, Yes) keep(msl_cum_pc) label append
*3
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y msl_service_pc4y (msl_cum_pc= bartik_leo5c) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB7.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, State fixed effects, Yes) keep(msl_cum_pc) label append
*4
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y msl_nw_cum_pc (msl_w_cum_pc=bartik_leo5_w_cum) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB7.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, State fixed effects, Yes) keep(msl_nw_cum_pc msl_w_cum_pc) label append
*5
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y msl_nw_cum_pc (msl_w_cum_pc=bartik_leo5_w_cum) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB7.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, State fixed effects, Yes) keep(msl_nw_cum_pc msl_w_cum_pc) label append
*6
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y msl_nw_cum_pc (msl_w_cum_pc=bartik_leo5_w_cum) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB7.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, State fixed effects, Yes) keep(msl_nw_cum_pc msl_w_cum_pc) label append

* Table B8 (CZ as unit of analysis)
collapse (first) state_fips (mean) ddem_votes_pct1 msl_pc4y2 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y bartik_leo5 bartik_leo5_w2 msl_w_pc4y2 msl_nw_pc4y2 msl_service_pc4y  white_counties_4y, by(id_cz year)
label variable msl_pc4y2 "Manufacturing Layoffs"
label variable msl_w_pc4y2 "White Manufacturing Layoffs"
label variable msl_nw_pc4y2 "Non-white Manufacturing Layoffs"
label variable ddem_votes_pct1 "Change of Democratic Vote Share"
label variable bartik_leo5 "Bartik instrument (total)"
label variable bartik_leo5_w2 "Bartik instrument (white)"
xtset id_cz year
egen id_state=group(state_fips)
*1
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y (msl_pc4y2= bartik_leo5) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB8.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, Service Layoffs, No, State fixed effects, Yes) keep(msl_pc4y2) label
*2
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y (msl_pc4y2= bartik_leo5) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB8.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, No, State fixed effects, Yes) keep(msl_pc4y2) label append
*3
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y msl_service_pc4y (msl_pc4y2= bartik_leo5) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB8.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, Yes, State fixed effects, Yes) keep(msl_pc4y2) label append
*4
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB8.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, Service Layoffs, No, State fixed effects, Yes) keep(msl_w_pc4y2 msl_nw_pc4y2) label append
*5
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB8.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, No, State fixed effects, Yes) keep(msl_w_pc4y2 msl_nw_pc4y2) label append
*6
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y white_counties_4y msl_service_pc4y msl_nw_pc4y2 (msl_w_pc4y2=bartik_leo5_w2) i.id_state if year==2016, r partial(i.id_state) first
outreg2 using tableB8.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, Yes, State fixed effects, Yes) keep(msl_w_pc4y2 msl_nw_pc4y2) label append


########## APPENDIX C

* Loading the dataset
*cd "C:\Users\lbacci\Dropbox\Projects\Baccini Weymouth 2017\April2018_Data_Legislators_Voting\Replication\.A_Replication"
use county_level, clear
*cd "C:\Users\lbacci\Dropbox\Projects\Baccini Weymouth 2017\April2018_Data_Legislators_Voting\Replication\.AA Check"

* Setting the county-year panel
xtset pan_id year

* Table C1 (first stage)
*1
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y it1 pers_m_total_share_4y_y1 pers_coll_share_4y_y1 white_counties_4y white_counties_4y_y1 (msl_pc4y2 it_new= bartik_leo5 it_instr2) i.id_state_y i.pan_id if year>=2008, cluster(pan_id) partial(i.id_state_y i.pan_id) first savefirst
est restore _ivreg2_msl_pc4y2
outreg2 using tableC1.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Service layoffs, No, County fixed effects, Yes, State-Year fixed effects, Yes) keep(bartik_leo5 it_instr2) label append
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y it1 pers_m_total_share_4y_y1 pers_coll_share_4y_y1 white_counties_4y white_counties_4y_y1 (msl_pc4y2 it_new= bartik_leo5 it_instr2) i.id_state_y i.pan_id if year>=2008, cluster(pan_id) partial(i.id_state_y i.pan_id) first savefirst
est restore _ivreg2_it_new
outreg2 using tableC1.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Service layoffs, No, County fixed effects, Yes, State-Year fixed effects, Yes) keep(bartik_leo5 it_instr2) label append

* Table C2 (other confounders)
*1
xi: xtivreg2 ddem_votes_pct1 LAU_unemp_rate_4y it1 pers_m_total_share_4y_y1 pers_coll_share_4y_y1 msl_educ_prop msl_male_prop msl_age_prop white_counties_4y white_counties_4y_y1 (msl_pc4y2 it_new= bartik_leo5 it_instr2) i.id_state_y if year>=2008, cluster(pan_id) fe partial(i.id_state_y) first
outreg2 using tableC2.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Sevice Layoffs, No, Other Layoffs, Yes, State-year fixed effects, Yes, County fixed effects, Yes) keep(msl_pc4y2 it_new) label
*2
xi: xtivreg2 ddem_votes_pct1 LAU_unemp_rate_4y it1 pers_m_total_share_4y_y1 pers_coll_share_4y_y1 msl_educ_prop msl_male_prop msl_age_prop white_counties_4y white_counties_4y_y1 msl_nw_pc4y2 it_nw2 (msl_w_pc4y2 it_w2=bartik_leo5_w2 it_w2_instr_new) i.id_state_y if year>=2008, cluster(pan_id) fe partial(i.id_state_y) first
outreg2 using tableC2.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Sevice Layoffs, No, Other Layoffs, Yes, State-year fixed effects, Yes, County fixed effects, Yes) keep(msl_nw_pc4y2 msl_w_pc4y2 it_w2 it_nw2) label append
*3
xi: xtivreg2 ddem_votes_pct1 LAU_unemp_rate_4y it1 pers_m_total_share_4y_y1 pers_coll_share_4y_y1 white_counties_4y white_counties_4y_y1 autor it_autor (msl_pc4y2 it_new= bartik_leo5 it_instr2) i.id_state_y if year>=2008, cluster(pan_id) fe partial(i.id_state_y) first
outreg2 using tableC2.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Sevice Layoffs, No, Other Layoffs, No, State-year fixed effects, Yes, County fixed effects, Yes) keep(msl_pc4y2 it_new autor it_autor) label append
*4
xi: xtivreg2 ddem_votes_pct1 LAU_unemp_rate_4y it1  pers_m_total_share_4y_y1 pers_coll_share_4y_y1 white_counties_4y white_counties_4y_y1 autor it_autor msl_nw_pc4y2 it_nw2 (msl_w_pc4y2 it_w2=bartik_leo5_w2 it_w2_instr_new) i.id_state_y if year>=2008, cluster(pan_id) fe partial(i.id_state_y) first
outreg2 using tableC2.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Sevice Layoffs, No, Other Layoffs, No, State-year fixed effects, Yes, County fixed effects, Yes) keep(msl_nw_pc4y2 msl_w_pc4y2 it_w2 it_nw2 autor it_autor) label append
*5
xi: xtivreg2 ddem_votes_pct1 LAU_unemp_rate_4y it1 pers_m_total_share_4y_y1 pers_coll_share_4y_y1 white_counties_4y white_counties_4y_y1 (msl_pc4y2 it_new autor it_autor= bartik_leo5 it_instr2 autor_instr it_autor_instr) i.id_state_y if year>=2008, cluster(pan_id) fe partial(i.id_state_y) first
outreg2 using tableC2.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Sevice Layoffs, No, Other Layoffs, No, State-year fixed effects, Yes, County fixed effects, Yes) keep(msl_pc4y2 it_new autor it_autor) label append
*6
xi: xtivreg2 ddem_votes_pct1 LAU_unemp_rate_4y it1 pers_m_total_share_4y_y1 pers_coll_share_4y_y1 white_counties_4y white_counties_4y_y1 msl_nw_pc4y2 it_nw2 (msl_w_pc4y2 it_w2 autor it_autor=bartik_leo5_w2 it_w2_instr_new autor_instr it_autor_instr) i.id_state_y if year>=2008, cluster(pan_id) fe partial(i.id_state_y) first
outreg2 using tableC2.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Sevice Layoffs, No, Other Layoffs, No, State-year fixed effects, Yes, County fixed effects, Yes) keep(msl_nw_pc4y2 msl_w_pc4y2 it_w2 it_nw2 autor it_autor) label append

* Table C3 (including trends)
egen id_cd=group(cd_id)
quietly tabulate id_cd, gen(eta)
foreach num of numlist 1(1)327 {
gen time_eta`num'=eta`num'*year
}
*1
xi: xtivreg2 ddem_votes_pct1 LAU_unemp_rate_4y it1 pers_m_total_share_4y_y1 pers_coll_share_4y_y1 white_counties_4y white_counties_4y_y1 (msl_pc4y2 it_new= bartik_leo5 it_instr2) i.id_state_y time_eta* if year>=2008, cluster(pan_id) fe partial(i.id_state_y time_eta*) first
outreg2 using tableC3.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Sevice Layoffs, No, County fixed effects, Yes, State-year fixed effects, Yes, CZ trends, Yes) keep(msl_pc4y2 it_new) label
*2
xi: xtivreg2 ddem_votes_pct1 LAU_unemp_rate_4y it1 pers_m_total_share_4y_y1 pers_coll_share_4y_y1 white_counties_4y white_counties_4y_y1 msl_nw_pc4y2 it_nw2 (msl_w_pc4y2 it_w2=bartik_leo5_w2 it_w2_instr_new) i.id_state_y time_eta* if year>=2008, cluster(pan_id) fe partial(i.id_state_y time_eta*) first
outreg2 using tableC3.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Sevice Layoffs, No, County fixed effects, Yes, State-year fixed effects, Yes, CZ trends, Yes) keep(msl_w_pc4y2 it_w2 msl_nw_pc4y2 it_nw2) label append

* Table C4 (CZ as unit of analysis)
collapse (first) state_fips (mean) ddem_votes_pct1 msl_pc4y2 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y bartik_leo5 bartik_leo5_w2 msl_w_pc4y2 msl_nw_pc4y2 white_counties_4y, by(id_cz year)
label variable msl_pc4y2 "Manufacturing Layoffs"
label variable msl_w_pc4y2 "White Manufacturing Layoffs"
label variable msl_nw_pc4y2 "Non-white Manufacturing Layoffs"
label variable ddem_votes_pct1 "Change of Democratic Vote Share"
label variable bartik_leo5 "Bartik instrument (total)"
label variable bartik_leo5_w2 "Bartik instrument (white)"
xtset id_cz year
egen id_state=group(state_fips)
gen dem_inc=0
replace dem_inc=1 if year>2008
xtset id_cz year
gen it_new=msl_pc4y2*dem_inc
gen it1=LAU_unemp_rate_4y*dem_inc
gen it_w=msl_w_pc4y2*dem_inc
gen it_nw=msl_nw_pc4y2*dem_inc
gen pers_coll_share_4y_y1=pers_coll_share_4y*dem_inc
gen pers_m_total_share_4y_y1=pers_m_total_share_4y*dem_inc
gen white_counties_4y_y1=white_counties_4y*dem_inc
gen it_instr= bartik_leo5*dem_inc
gen it_w_instr= bartik_leo5_w2*dem_inc
egen id_state_year=group(state_fips year)
label variable it_new "Manufacturing Layoffs*Dem Inc"
label variable it_w "White Manufacturing Layoffs*Dem Inc"
label variable it_nw "Non-white Manufacturing Layoffs*Dem Inc"
label variable it_instr "Bartik instrument*Dem Inc"
label variable it_w_instr "Bartik instrument (white)*Dem Inc"
*1
xi: xtivreg2 ddem_votes_pct1 LAU_unemp_rate_4y it1 pers_m_total_share_4y_y1 pers_coll_share_4y_y1 white_counties_4y white_counties_4y_y1 (msl_pc4y2 it_new= bartik_leo5 it_instr) i.id_state_y if year>=2008, cluster(id_cz) fe partial(i.id_state_y) first
outreg2 using tableC4.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Sevice Layoffs, No, State fixed effects, No, State-year fixed effects, Yes, CZ fixed effects, Yes) keep(msl_pc4y2 it_new) label
*2
xi: xtivreg2 ddem_votes_pct1 LAU_unemp_rate_4y it1 pers_m_total_share_4y_y1 pers_coll_share_4y_y1 white_counties_4y white_counties_4y_y1 msl_nw_pc4y2 it_nw (msl_w_pc4y2 it_w=bartik_leo5_w it_w_instr) i.id_state_y if year>=2008, cluster(id_cz) fe partial(i.id_state_y) first
outreg2 using tableC4.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, Yes, Sevice Layoffs, No, State fixed effects, No, State-year fixed effects, Yes, CZ fixed effects, Yes) keep(msl_w_pc4y2 it_w msl_nw_pc4y2 it_nw) label append
*3
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y msl_nw_pc4y2  (msl_w_pc4y2 =bartik_leo5_w) i.id_state if year==2008, r partial(i.id_state) first
outreg2 using tableC4.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, Sevice Layoffs, No, State fixed effects, Yes, State-year fixed effects, No, CZ fixed effects, No) keep(msl_nw_pc4y2 msl_w_pc4y2) label append
*4
xi: ivreg2 ddem_votes_pct1 LAU_unemp_rate_4y pers_m_total_share_4y pers_coll_share_4y msl_nw_pc4y2  (msl_w_pc4y2 =bartik_leo5_w ) i.id_state if year==2012, r partial(i.id_state) first
outreg2 using tableC4.xls, bdec(3) tdec(3) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Demography Controls, Yes, White Population Share, No, Sevice Layoffs, No, State fixed effects, Yes, State-year fixed effects, No, CZ fixed effects, No) keep(msl_nw_pc4y2 msl_w_pc4y2) label append

* ZAG: tweaked logging
log close county