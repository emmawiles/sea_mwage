args sample_size

set more off
set matsize 11000
capture clear


*********************************************************************
* PATHS 
*********************************************************************
global path_project "R:\\Project\SeattleMinimumWage\\Stata\\AttenuationPaper\\dofiles\\AEJEP_submission_2020\\"
global path_ESD "R:\Project\SeattleMinimumWage\Data\State Data\ESD\data\"
global path_cpi "${path_project}\\data\\"
global path_geo_data "${path_project}\\data\\data_confidential\\"
global path_analysis_data "${path_project}\\data\\data_confidential\\"
global path_collapsed_data "${path_project}\\data\\data_confidential\\"
global path_output "${path_project}\\output\\output_confidential\\"

log using "${path_project}\\log\\log_confidential\\table_a3_`datename'.log", replace
*********************************************************************


	* load the data	
	use  "${path_analysis_data}establishment_analysis_Spring_2018_`sample_size'.dta"

	* Assign missing region to region "9" (these are state-wide, county wide businesses etc.)
	replace region = 9 if region == .
	replace region = 9 if county_id ==.		

	rename naics2 industry
	
	* Combine some industries
	* Manufacturing 31-33
	replace industry=31 if industry==32 | industry==33
	* Retail Trade 44-45
	replace industry=44 if industry==45
	* Transportation and Warehousing
	replace industry=48 if industry==49
	
	* For size variables --> stick 0 for non-operating establishments
	unab vars_put_zeroes: nworkers_* hours_* payroll_* 
	foreach y in `vars_put_zeroes' {
		replace `y' = 0 if operating == 0		 	
	}	

	* Exclude multi-establishment firms?
	drop if multiest == 1

	
	* COLLAPSE TO Save time series for each 
	collapse (sum) payroll_flow payroll_beg hours_flow hours_beg wages nworkers_beg N = operating  , by(region puma_id yearquarter wagegroup industry)	
	gen seattle =1 if region ==1	
		
	*BINS ANALYSIS
	* Generate Variables at Each Wage Group Level for wagerate = 9 to wagerate = 40
		gen bin_payroll_flow = payroll_flow
		gen bin_payroll_beg = payroll_beg
		gen bin_hours_flow = hours_flow
		gen bin_nworkers_beg = nworkers_beg
				
		* Generate earnings and wagerate bins
		gen bin_earnings = bin_payroll_beg / bin_nworkers_beg
		gen bin_mean_wagerate = bin_payroll_flow / bin_hours_flow
				
		save "${path_analysis_data}\establishment_analysis_industry_`sample_size'.dta" , replace

*****Create Table T_industry				
		use "${path_analysis_data}establishment_analysis_industry_`sample_size'.dta", replace
	
		gen group = 18 if wagegroup <=18
		replace group = 19 if wagegroup >=19
		rename puma_id puma_id0
		replace seattle = 0 if (puma_id0==11101 | puma_id0==11401 | puma_id0==11402 | puma_id0==11706 | puma_id0==11701 | puma_id0==11702 |  ///
								puma_id0==10100 | puma_id0==11704 | puma_id0==10800 | puma_id0== 10503 | puma_id0==10504 |  puma_id0==10701)
		
		foreach var in bin_hours_flow {

			replace `var' = `var'*0.19598976 if  puma_id0==11101
			replace `var' = `var'*0.18288814 if  puma_id0==	11401
			replace `var' = `var'*0.15918153 if  puma_id0==	11402
			replace `var' = `var'*0.15097339 if  puma_id0==	11706
			replace `var' = `var'*0.08451649 if  puma_id0==	11701
			replace `var' = `var'*0.07891341 if  puma_id0==	11702
			replace `var' = `var'*0.06869631 if  puma_id0==	10100
			replace `var' = `var'*0.04435831 if  puma_id0==	11704
			replace `var' = `var'*0.01830387 if  puma_id0==	10800
			replace `var' = `var'*0.01617880 if  puma_id0==	10503
			replace `var' = `var'*0.00000000 if  puma_id0==	10504
			replace `var' = `var'*0.00000000 if  puma_id0==	10701
				}
			foreach var in bin_nworkers_beg {

			replace `var' = `var'*0.11821761 if  puma_id0==	11101
			replace `var' = `var'*0.14675280 if  puma_id0==11401
			replace `var' = `var'*0.00000000 if  puma_id0==11402
			replace `var' = `var'*0.18174859 if  puma_id0==11706
			replace `var' = `var'*0.21656168 if  puma_id0==11701
			replace `var' = `var'*0.12987613 if  puma_id0==11702
			replace `var' = `var'*0.12261134 if  puma_id0==10100
			replace `var' = `var'*0.07400751 if  puma_id0==11704
			replace `var' = `var'*0.00000000 if  puma_id0==10800
			replace `var' = `var'*0.00000000 if  puma_id0==10503
			replace `var' = `var'*0.00296812 if  puma_id0==10504
			replace `var' = `var'*0.00725622 if  puma_id0==10701
				}

			
		keep if yearquarter ==20132| yearquarter ==20142 
		collapse (sum) bin_payroll_flow  bin_hours_flow  bin_nworkers_beg (mean) bin_mean_wagerate  , by(seattle group  yearquarter industry)	
		
		save "${path_output}establishment_analysis_industrytable_`sample_size'.dta", replace
		