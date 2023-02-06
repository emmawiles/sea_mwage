clear all
cap log close

*****************
***           ***
*** Set paths ***
***           ***
***************** 


global path_project "R:\\Project\SeattleMinimumWage\\Stata\\AttenuationPaper\\dofiles\\AEJEP_submission_2020\\"
global path_data "${path_project}\\data\\data_confidential\\"
global path_output "${path_project}\\output\\output_confidential\\"


log using "${path_project}\\log\\log_confidential\\matched_sample_nn1.log", replace

*************************
***                   ***
*** Choose what to do ***
***                   ***
*************************
 

foreach cohort in 2012  2015  {

*** Load the data ***
use "${path_data}mw`cohort'_cohort_100.dta", clear

*** Set wagerates of workers without jobs to 0
*** for periods used for macthing
*** (these people will be matched to other workers with "0" wagerate)
replace wagerate_l1 = 0 if wagerate_l1 == .
replace wagerate_l2 = 0 if wagerate_l2 == .
replace multiplejobs_0 = 0 if multiplejobs_0 == .
replace multiplejobs_l1 = 0 if multiplejobs_l1 == .
replace multiplejobs_l2 = 0 if multiplejobs_l2 == .

*** Merge matched sample for different sample sizes ***
local all_neighbors
forvalues i=1/1 {
	local all_neighbors "`all_neighbors' nn`i'"
}

merge 1:1 personid treat using "${path_data}ESD_matched_sample_treated_`cohort'cohort_wo_rep_100pct_nn1.dta", keepusing(weights matchID `all_neighbors')


drop _merge
foreach var in `all_neighbors' {
	destring `var', replace
}
destring matchID, replace
rename matchID matchID_matchmatrix

*** Merge dataset with sorting from R ***
merge 1:1 personid using "${path_data}ESDdata_randomsample_`cohort'cohort.dta"
drop _merge
destring matchID, replace

*** Correct weights for matched control pool ***
*** So that Sum weights treated = Sum weights control ***
keep if weights ==1


save "${path_data}ESDdata_matched_`cohort'sample.dta", replace
clear
 }
 
 