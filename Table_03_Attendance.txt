/* Program to Create Table_03: Effects on Monitored School Attendance Rates */

version 9.2
clear
#delimit ;
set mem 200m;
set more off;
capture log close;
log using Table_03_Attendance.log, replace text;

/*****
*** LOAD AND FORMAT VARIABlES
*****/

global varbaseline "i.s_teneviv s_utilities s_durables s_infraest_hh s_age_sorteo s_age_sorteo2 s_years_back s_sexo i.s_estcivil s_single s_edadhead s_yrshead s_tpersona s_num18 i.s_estrato s_puntaje s_ingtotal i.grade suba s_over_age";

/*****
** Table Variables
*****/

global strformatsmall "%9.2f";
global strformatbig "%9.4f";
global strformat "%9.2f";

capture program drop sig;
program sig;
   args obj_var point_est_var point_sd_var row_id_var row_num;
   local point_est `point_est_var';
   local point_sd `point_sd_var';
   local t_stat abs(`point_est'/`point_sd');
   if `t_stat' < 1.645 {;
      replace `obj_var' = string(`point_est', "$strformat") if `row_id_var' == `row_num';
      };
   if `t_stat' >= 1.645 & `t_stat' < 1.96 {;
      replace `obj_var' = string(`point_est', "$strformat")+"*" if `row_id_var' == `row_num';
      };
   if `t_stat' >= 1.96 & `t_stat' < 2.576 {;
      replace `obj_var' = string(`point_est', "$strformat")+"**" if `row_id_var' == `row_num';
      };
   if `t_stat' >= 2.576 {;
      replace `obj_var' = string(`point_est', "$strformat")+"***" if `row_id_var' == `row_num';
      };
   replace `obj_var' = "<" + string(`point_sd', "$strformat") + ">" + " " if `row_id_var' == `row_num' + 1;
   end;

/*****
*** Table
*****/
 
use "Public_Data_AEJApp_2010-0132.dta";
drop if suba == 1 & grade < 9;

keep if survey_selected;
drop if grade == 11;

*Regressions of enrollment for San Cristobal;
xi: reg at_msamean T1_treat T2_treat if suba == 0, cluster(school_code);
outreg T1_treat T2_treat using "Table_03_Attendance.csv", 3aster coefastr se comma replace;
test T1_treat == T2_treat;

xi: reg at_msamean T1_treat T2_treat $varbaseline if suba == 0, cluster(school_code);
outreg T1_treat T2_treat using "Table_03_Attendance.csv", 3aster coefastr se comma append;
test T1_treat == T2_treat;

xi: reg at_msamean T1_treat T2_treat $varbaseline i.school_code if suba == 0, cluster(school_code);
outreg T1_treat T2_treat using "Table_03_Attendance.csv", 3aster coefastr se comma append;
test T1_treat == T2_treat;

*Regressions of enrollment for Suba;
xi: reg at_msamean T3_treat if suba == 1 & grade > 8, cluster(school_code);
outreg T3_treat using "Table_03_Attendance.csv", 3aster coefastr se comma append;

xi: reg at_msamean T3_treat $varbaseline if suba == 1 & grade > 8, cluster(school_code);
outreg T3_treat using "Table_03_Attendance.csv", 3aster coefastr se comma append;

xi: reg at_msamean T3_treat $varbaseline i.school_code if suba == 1 & grade > 8, cluster(school_code);
outreg T3_treat using "Table_03_Attendance.csv", 3aster coefastr se comma append;

*Regression of T3 vs. T1;
xi: reg at_msamean T1_treat T2_treat T3_treat $varbaseline suba i.school_code if grade > 8, cluster(school_code);
outreg T1_treat T2_treat T3_treat using "Table_03_Attendance.csv", 3aster coefastr se comma append;
test T1_treat == T2_treat;
test T1_treat == T3_treat;

su at_msamean if treatment ~= 1 & grade > 8;

/****
*** Unreported Results -- Effects for non-missing enrollment sample
****/

xi: reg at_msamean T1_treat T2_treat $varbaseline i.school_code if suba == 0 & m_enrolled ~= ., cluster(school_code);
xi: reg at_msamean T3_treat $varbaseline i.school_code if suba == 1 & grade > 8 & m_enrolled ~= ., cluster(school_code);

log close;
