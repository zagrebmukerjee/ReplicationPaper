*     ***************************************************************** *;
*     ***************************************************************** *;
*       File-Name:      anes_level.do                                   *;
*       Date:           12/28/2020                                      *;
*       Authora:        Baccini & Weymouth                              *;
*       Purpose:        Replication of "Gone For Good:                  *;
*						Deindustrialization, White Voter Backlash,      *;
*						and US Presidential Voting"                     *;                                                                      
*       Input Files:    anes_level.dta 					         	    *; 
*       Output Files:   anes_logfile.log                                *;
*       Machine:        Office                                          *;
*       Program: 		Stata 15                                        *;
*     ****************************************************************  *;
*     ****************************************************************  *;

*Note: to run this dofile, you need to install the following stata comands: outreg2 and ivreg2 (plus related commands).

* Setting initial commands
clear
clear matrix
clear mata
set maxvar 50000
set matsize 11000
set more off 

* Loading the dataset
cd "C:\Users\lbacci\Dropbox\Projects\Baccini Weymouth 2017\April2018_Data_Legislators_Voting\Replication\.A_Replication"
use anes_level, replace
cd "C:\Users\lbacci\Dropbox\Projects\Baccini Weymouth 2017\April2018_Data_Legislators_Voting\Replication\.AA Check"
log using anes_logfile.log


###### MAIN TEXT

* Table 5 (mechanisms)
*1
ivreg2 econ_worse  white_new (it5=it_instr5) gender unemployed college union ideology working_class_alignment i.id_district [w= V160101w ],r  partial(i.id_district)
outreg2 using table5.xls, bdec(2) tdec(2) addtext(Individual Controls, Yes, District fixed effects, Yes) keep(white_new it5) label
*2
ivreg2 rightwrong_track_re white_new (it5=it_instr5) gender unemployed college union ideology working_class_alignment i.id_district [w= V160101w ],r  partial(i.id_district)
outreg2 using table5.xls, bdec(2) tdec(2) addtext(Individual Controls, Yes, District fixed effects, Yes) keep(white_new it5) label append
*3
ivreg2 opportunity_ahead_rec white_new (it5=it_instr5) gender unemployed college union ideology working_class_alignment i.id_district [w= V160101w ],r  partial(i.id_district)
outreg2 using table5.xls, bdec(2) tdec(2) addtext(Individual Controls, Yes, District fixed effects, Yes) keep(white_new it5) label append
*4
ivreg2 negecon  white_new (it5=it_instr5) gender unemployed college union ideology working_class_alignment i.id_district [w= V160101w ],r  partial(i.id_district)
outreg2 using table5.xls, bdec(2) tdec(2) addtext(Individual Controls, Yes, District fixed effects, Yes) keep(white_new it5) label append


##### APPENDIX B

* Table B15 (first stage)
*1
ivreg2 econ_worse white_new (it5=it_instr5) gender unemployed college union ideology working_class_alignment i.id_district [w= V160101w ],r  partial(i.id_district) first savefirst
est restore _ivreg2_it5
outreg2 using tableB15.xls, bdec(2) tdec(2) addtext(District FE, Yes, Individual Controls FE, Yes) keep(it_instr5) label
