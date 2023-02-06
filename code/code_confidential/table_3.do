*Table 1a- total # of workers
*******************************************************************************************************
* PREFIX TO SAVE RESULTS
*******************************************************************************************************

local Prefix "Table3_"
local datename = trim("$S_DATE")


*******************************************************************************************************
* PATHS 
*******************************************************************************************************

global path_project "R:\\Project\SeattleMinimumWage\\Stata\\AttenuationPaper\\dofiles\\AEJEP_submission_2020\\"
global path_ESD "R:\Project\SeattleMinimumWage\Data\State Data\ESD\data\"
global path_cpi "${path_project}\\data\\"
global path_geo_data "${path_project}data\\data_confidential\\"
global path_output "${path_project}output\\output_confidential\\"

log using "${path_project}\\log\\log_confidential\\`Prefix'`datename'.log", replace


use "R:\Project\SeattleMinimumWage\Stata\Worker_analysis\data\worker_analysis_Fall_2018_100.dta"
	preserve
	gen nn =1
	keep if yearquarter ==20151 |yearquarter == 20144 | yearquarter==20143
	
	
	*restrict analysis of wagerate covariates and outcome to only be for valid wagerates
	*replace wagerate=. if wagerateflawed==1	
	gen t_wrf= 1 if wagerateflawed==1 
	bys personid: egen person_wrf= max(wagerateflawed) 
	drop if person_wrf==1
	drop t_wrf person_wrf
	
	keep if  employed==1
	keep if yearquarter ==20151
	drop if wagerateflawed==1
	keep if wagerate <11
	drop if region ==9
	*restrict analysis to 100% treatment and 0 percent Seattle/king employment

	gen mult= (multiest ==1)
	gen bad = bad_data if  multiest==0 
	
	
	gen n_sea = (sea_dosage <1 & sea_dosage >0)
	gen n_con = (wa_dosage <1 & wa_dosage >0)
	replace n_sea = 0 if multiest==1 
	replace n_sea =0 if bad_data==1
	replace n_con = 0 if multiest==1 
	replace n_con =0 if  bad_data==1
	
	**EITHER CODING OF DOSAGE WORKS!
	gen dtemp = max(wa_dosage, sea_dosage)	
	gen dosage =0
	replace dosage = 1 if dtemp ==1 & multiest==0 & bad_data==0
	
	*gen dosage = (wa_dosage==1 | sea_dosage==1)	
	*replace dosage =0 if (multiest==1 | bad_data==1)

	collapse (max) nn multiest bad  dosage n_sea n_con   , by (personid yearquarter)
	collapse(sum) nn  multiest bad dosage n_sea n_con   , by (yearquarter)
	save "${path_output}\table1a_1000911.dta", replace
	restore 
	
clear
log close