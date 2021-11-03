*     ***************************************************************** *;
*     ***************************************************************** *;
*       File-Name:      individual_level.do                             *;
*       Date:           12/28/2020                                      *;
*       Authora:        Baccini & Weymouth                              *;
*       Purpose:        Replication of "Gone For Good:                  *;
*						Deindustrialization, White Voter Backlash,      *;
*						and US Presidential Voting"                     *;                                                                      
*       Input Files:    individual_level.dta 						    *; 
*       Output Files:   individual_logfile.log                          *;
*       Machine:        Office                                          *;
*       Program: 		Stata 15                                        *;
*     ****************************************************************  *;
*     ****************************************************************  *;

*Note: to run this dofile, you need to install the following stata comands: ivreghdfe, outreg2, and ivreg2 (plus related commands).

/*
### TO INSTALL  ivreghdfe and ivreg2

* Install ftools (remove program if it existed previously)
cap ado uninstall ftools
net install ftools, from("https://raw.githubusercontent.com/sergiocorreia/ftools/master/src/")

* Install reghdfe
cap ado uninstall reghdfe
net install reghdfe, from("https://raw.githubusercontent.com/sergiocorreia/reghdfe/master/src/")

* Install boottest (Stata 11 and 12)
if (c(version)<13) cap ado uninstall boottest
if (c(version)<13) ssc install boottest

* Install moremata (sometimes used by ftools but not needed for reghdfe)
cap ssc install moremata

* Install ivreg2, the core package
cap ado uninstall ivreg2
ssc install ivreg2

* Finally, install this package
cap ado uninstall ivreghdfe
net install ivreghdfe, from(https://raw.githubusercontent.com/sergiocorreia/ivreghdfe/master/src/)
*/


* Setting initial commands
clear
clear matrix
clear mata
set maxvar 50000
set matsize 11000
set more off 

* Loading the dataset
cd "C:\Users\lbacci\Dropbox\Projects\Baccini Weymouth 2017\April2018_Data_Legislators_Voting\Replication\.A_Replication"
use individual_level, clear
cd "C:\Users\lbacci\Dropbox\Projects\Baccini Weymouth 2017\April2018_Data_Legislators_Voting\Replication\.AA Check"
log using individual_logfile.log


############ MAIN TEXT

* Table 3   
*1
ivreghdfe vote_dem white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white (it_white = it_white_instr) if year==2016, absorb(pan_id) cluster(pan_id)
outreg2 using table3.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, No, Service Layoffs, No, County fixed effects, Yes) keep(white it_white) label
*2
ivreghdfe vote_dem white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white male_white coll_white white_counties_white (it_white = it_white_instr) if year==2016, absorb(pan_id) cluster(pan_id)
outreg2 using table3.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, No, County fixed effects, Yes) keep(white it_white) label append
*3
ivreghdfe vote_dem white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white male_white coll_white white_counties_white it_white_serv (it_white = it_white_instr) if year==2016, absorb(pan_id) cluster(pan_id) 
outreg2 using table3.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, Yes, County fixed effects, Yes) keep(white it_white) label append
*4
ivreghdfe voting white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white (it_white = it_white_instr) if year==2016, absorb(pan_id) cluster(pan_id)
outreg2 using table3.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, No, Service Layoffs, No, County fixed effects, Yes) keep(white it_white) label append

* Table 4
global controls1 it_black_unempl it_his_unempl it_asian_unempl it_native_unempl it_mixed_unempl it_other_unempl it_me_unempl it_black_gender it_his_gender it_asian_gender it_native_gender it_mixed_gender it_other_gender it_me_gender it_black_age it_his_age it_asian_age it_native_age it_mixed_age it_other_age it_me_age it_black_educ it_his_educ it_asian_educ it_native_educ it_mixed_educ it_other_educ it_me_educ it_black_sen it_his_sen it_asian_sen it_native_sen it_mixed_sen it_other_sen it_me_sen
global controls2 it_black_coll it_his_coll it_asian_coll it_native_coll it_mixed_coll it_other_coll it_me_coll it_black_male it_his_male it_asian_male it_native_male it_mixed_male it_other_male it_me_male it_black_whitec it_his_whitec it_asian_whitec it_native_whitec it_mixed_whitec it_other_whitec
global controls3 it_black_serv it_his_serv it_asian_serv it_native_serv it_mixed_serv it_other_serv it_me_serv
*1
ivreghdfe vote_dem $controls1 race12-race18 ( it_black it_his it_asian it_native it_mixed it_other it_me = it_black_instr it_his_instr it_asian_instr it_native_instr it_mixed_instr it_other_instr it_me_instr ) if year==2016, absorb(pan_id) cluster(pan_id)
outreg2 using table4, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, No, Service Layoffs, No, County fixed effects, Yes) keep(race12 race13 race14 race15 race16 race17 race18 it_black it_his it_asian it_native it_mixed it_other it_me) label
*2
ivreghdfe vote_dem $controls1 $controls2 race12-race18 ( it_black it_his it_asian it_native it_mixed it_other it_me = it_black_instr it_his_instr it_asian_instr it_native_instr it_mixed_instr it_other_instr it_me_instr ) if year==2016, absorb(pan_id) cluster(pan_id)
outreg2 using table4.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, No, County fixed effects, Yes) keep(race12 race13 race14 race15 race16 race17 race18 it_black it_his it_asian it_native it_mixed it_other it_me) label append
*3
ivreghdfe vote_dem $controls1 $controls2 $controls3 race12-race18 ( it_black it_his it_asian it_native it_mixed it_other it_me = it_black_instr it_his_instr it_asian_instr it_native_instr it_mixed_instr it_other_instr it_me_instr ) if year==2016, absorb(pan_id) cluster(pan_id)
outreg2 using table4.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, Yes, County fixed effects, Yes) keep(race12 race13 race14 race15 race16 race17 race18 it_black it_his it_asian it_native it_mixed it_other it_me) label append
*4
ivreghdfe voting $controls1 race12-race18 ( it_black it_his it_asian it_native it_mixed it_other it_me = it_black_instr it_his_instr it_asian_instr it_native_instr it_mixed_instr it_other_instr it_me_instr ) if year==2016, absorb(pan_id) cluster(pan_id)
outreg2 using table4.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, No, Service Layoffs, No, County fixed effects, Yes) keep(race12 race13 race14 race15 race16 race17 race18 it_black it_his it_asian it_native it_mixed it_other it_me) label append

* Table 7
*1
ivreghdfe vote_dem white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white male_white coll_white white_counties_white (it_white = it_white_instr), absorb(id_county_year) cluster(pan_id) 
outreg2 using table7.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, Yes, White counties Control, Yes, County fixed effects, No, County-election fixed effects, Yes) keep(white it_white) label
*2
ivreghdfe vote_dem white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white (it_white = it_white_instr) if year==2008, absorb(pan_id) cluster(pan_id)
outreg2 using table7.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White counties Control, No, County fixed effects, Yes, County-election fixed effects, No) keep(white it_white) label append
*3
ivreghdfe vote_dem white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white (it_white = it_white_instr) if year==2012, absorb(pan_id) cluster(pan_id)
outreg2 using table7.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White counties Control, No, County fixed effects, Yes, County-election fixed effects, No) keep(white it_white) label append


########## APPENDIX A

* Table A2
tab race if year==2016

tab race


########## APPENDIX B

* Table B9 (first stage)
*1
xi: ivreg2 vote_dem white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white (it_white = it_white_instr) i.pan_id if year==2016, r partial(i.pan_id) cluster(pan_id) first savefirst
est restore _ivreg2_it_white
outreg2 using tableB9.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, No, Service Layoffs, No, County fixed effects, Yes) keep(it_white_instr) label

* Table B10 (by gender)
*1
ivreghdfe vote_dem white white_counties_white (it_white = it_white_instr) if gender==0 & year==2016, absorb(pan_id) cluster(pan_id)
outreg2 using tableB10.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, No, Individual Controls, No, Demography Controls, No, White Population Share, Yes, Service Layoffs, No, County fixed effects, Yes) keep(white it_white) label
*2
ivreghdfe vote_dem white white_counties_white (it_white = it_white_instr) if gender==1 & year==2016, absorb(pan_id) cluster(pan_id)
outreg2 using tableB10.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, No, Individual Controls, No, Demography Controls, No, White Population Share, Yes, Service Layoffs, No, County fixed effects, Yes) keep(white it_white) label append

* Table B11 (including white manufacturing)
*1
ivreghdfe vote_dem white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white white_counties_white it_white_nw2 (it_white_w2 = it_white_instr_w2) if year==2016, absorb(pan_id) cluster(pan_id)
outreg2 using tableB11.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, Yes, Service Layoffs, No, County fixed effects, Yes) keep(white it_white_w2 it_white_nw2) label
*2
ivreghdfe voting white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white white_counties_white it_white_nw2 (it_white_w2 = it_white_instr_w2) if year==2016, absorb(pan_id) cluster(pan_id)
outreg2 using tableB11.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, Yes, Service Layoffs, No, County fixed effects, Yes) keep(white it_white_w2 it_white_nw2) label append

* Table B12 (China shock)
*1
ivreghdfe vote_dem white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white white_counties_white it_autor it_white_serv (it_white = it_white_instr) if year==2016, absorb(pan_id) cluster(pan_id)
outreg2 using tableB12.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, Yes, Service Layoffs, Yes, County fixed effects, Yes) keep(white it_white it_autor) label
*2
ivreghdfe vote_dem white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white white_counties_white it_white_serv (it_white it_autor = it_white_instr it_autor_instr) if year==2016, absorb(pan_id) cluster(pan_id)
outreg2 using tableB12.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, Yes, Service Layoffs, Yes, County fixed effects, Yes) keep(white it_white it_autor) label append

* Table B13 (cumulative layoffs)
*1
ivreghdfe vote_dem white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white (it_white_cum = it_white_instr_cum) if year==2016, absorb(pan_id) cluster(pan_id)
outreg2 using tableB13.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, No, Service Layoffs, No, County fixed effects, Yes) keep(white it_white_cum) label
*2
ivreghdfe vote_dem white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white male_white coll_white white_counties_white (it_white_cum = it_white_instr_cum) if year==2016, absorb(pan_id) cluster(pan_id)
outreg2 using tableB13.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, No, County fixed effects, Yes) keep(white it_white_cum) label append
*3
ivreghdfe vote_dem white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white male_white coll_white white_counties_white it_white_serv (it_white_cum = it_white_instr_cum) if year==2016, absorb(pan_id) cluster(pan_id)
outreg2 using tableB13.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, Yes, County fixed effects, Yes) keep(white it_white_cum ) label append
*4
ivreghdfe voting white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white (it_white_cum = it_white_instr_cum) if year==2016, absorb(pan_id) cluster(pan_id)
outreg2 using tableB13.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, No, Service Layoffs, No, County fixed effects, Yes) keep(white it_white_cum) label append

* Table B14 (CZ as unit of analysis)
*1
ivreghdfe vote_dem white it_empl_cz i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white (it_white_cz = it_white_instr_cz) if year==2016, absorb(id_cz) cluster(id_cz)
outreg2 using tableB14.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, No, Service Layoffs, No, CZ fixed effects, Yes) keep(white it_white_cz) label
*2
ivreghdfe vote_dem white it_empl_cz i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white it_male_cz it_coll_cz it_whitec_cz (it_white_cz = it_white_instr_cz) if year==2016, absorb(id_cz) cluster(id_cz)
outreg2 using tableB14.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, No, CZ fixed effects, Yes) keep(white it_white_cz) label append
*3
ivreghdfe vote_dem white it_empl_cz i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white it_male_cz it_coll_cz it_whitec_cz it_serv_cz (it_white_cz = it_white_instr_cz) if year==2016, absorb(id_cz) cluster(id_cz)
outreg2 using tableB14.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, Yes, White Population Share, Yes, Service Layoffs, Yes, CZ fixed effects, Yes) keep(white it_white_cz) label append
*4
ivreghdfe voting white it_empl_cz i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white (it_white_cz = it_white_instr_cz) if year==2016, absorb(id_cz) cluster(id_cz)
outreg2 using tableB14.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, No, Service Layoffs, No, CZ fixed effects, Yes) keep(white it_white_cz) label append


########### APPENDIX C

* Table C5 (by race)
global controls1 it_black_unempl it_his_unempl it_asian_unempl it_native_unempl it_mixed_unempl it_other_unempl it_me_unempl it_black_gender it_his_gender it_asian_gender it_native_gender it_mixed_gender it_other_gender it_me_gender it_black_age it_his_age it_asian_age it_native_age it_mixed_age it_other_age it_me_age it_black_educ it_his_educ it_asian_educ it_native_educ it_mixed_educ it_other_educ it_me_educ it_black_sen it_his_sen it_asian_sen it_native_sen it_mixed_sen it_other_sen it_me_sen
global controls2 it_black_coll it_his_coll it_asian_coll it_native_coll it_mixed_coll it_other_coll it_me_coll it_black_male it_his_male it_asian_male it_native_male it_mixed_male it_other_male it_me_male it_black_whitec it_his_whitec it_asian_whitec it_native_whitec it_mixed_whitec it_other_whitec
global controls3 it_black_serv it_his_serv it_asian_serv it_native_serv it_mixed_serv it_other_serv it_me_serv
*1
ivreghdfe vote_dem $controls1 $controls2 race12-race18 (it_black it_his it_asian it_native it_mixed it_other it_me = it_black_instr it_his_instr it_asian_instr it_native_instr it_mixed_instr it_other_instr it_me_instr), absorb(id_county_year) cluster(pan_id) 
outreg2 using tableC5.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, Yes, White Population Share, Yes, County fixed effects, No, County-election fixed effects, Yes) keep(race12 race13 race14 race15 race16 race17 race18 it_black it_his it_asian it_native it_mixed it_other it_me) label
*2
ivreghdfe vote_dem $controls1 race12-race18 (it_black it_his it_asian it_native it_mixed it_other it_me = it_black_instr it_his_instr it_asian_instr it_native_instr it_mixed_instr it_other_instr it_me_instr) if year==2008, absorb(pan_id) cluster(pan_id)
outreg2 using tableC5.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, No, County fixed effects, Yes, County-election fixed effects, No) keep(race12 race13 race14 race15 race16 race17 race18 it_black it_his it_asian it_native it_mixed it_other it_me) label append
*3
ivreghdfe vote_dem $controls1 race12-race18 (it_black it_his it_asian it_native it_mixed it_other it_me = it_black_instr it_his_instr it_asian_instr it_native_instr it_mixed_instr it_other_instr it_me_instr) if year==2012, absorb(pan_id) cluster(pan_id)
outreg2 using tableC5.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, No, County fixed effects, Yes, County-election fixed effects, No) keep(race12 race13 race14 race15 race16 race17 race18 it_black it_his it_asian it_native it_mixed it_other it_me) label append

* Table C6 (China trade shock)
*1
ivreghdfe vote_dem white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white white_counties_white it_autor (it_white = it_white_instr), absorb(id_county_year) cluster(pan_id)
outreg2 using tableC6.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, Yes, County fixed effects, No, County-election fixed effects, Yes) keep(white it_white it_autor) label
*2
ivreghdfe vote_dem white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white white_counties_white (it_white it_autor = it_white_instr it_autor_instr), absorb(id_county_year) cluster(pan_id)
outreg2 using tableC6.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, Yes, County fixed effects, No, County-election fixed effects, Yes) keep(white it_white it_autor) label append

*Table C7 (county specific trends)
*County Linear Trends
quietly tabulate pan_id, gen(beta)
foreach num of numlist 1(1)1000 {
gen time_beta`num'=beta`num'*year
}
foreach num of numlist 1001(1)2000 {
gen time_beta`num'=beta`num'*year
}
foreach num of numlist 2001(1)2872 {
gen time_beta`num'=beta`num'*year
}
*1
capture ivreghdfe vote_dem white unempl_white i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white male_white coll_white white_counties_white (it_white = it_white_instr), absorb(id_county_year time_beta*) cluster(pan_id)
outreg2 using tableC7.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, Yes, White Population Share, Yes, County fixed effects, No, County-election fixed effects, Yes, County trends, Yes) keep(white msl_pc4y2 it_white) label

* Table C8 (CZ as unit of analysis)
*1
ivreghdfe vote_dem white it_empl_cz i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white it_male_cz it_coll_cz it_whitec_cz (it_white_cz = it_white_instr_cz), absorb(id_cz_year ) cluster(id_cz)
outreg2 using tableC8.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, Yes, White Population Share, Yes, CZ fixed effects, No, CZ-election fixed effects, Yes) keep(white it_white_cz) label
*2
ivreghdfe vote_dem white it_empl_cz i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white (it_white_cz = it_white_instr_cz) if year==2008, absorb(id_cz) cluster(id_cz)
outreg2 using tableC8.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, No, CZ fixed effects, Yes, CZ-election fixed effects, No) keep(white it_white_cz) label append
*3
ivreghdfe vote_dem white it_empl_cz i.gender gender_white i.educ educ_white age age_white i.approval_sen1 sen_white (it_white_cz = it_white_instr_cz) if year==2012, absorb(id_cz) cluster(id_cz)
outreg2 using tableC8.xls, bdec(2) tdec(2) addstat(`e(r2_p)') addtext(Unemployment Control, Yes, Individual Controls, Yes, Demography Controls, No, White Population Share, No, CZ fixed effects, Yes, CZ-election fixed effects, No) keep(white it_white_cz) label append
