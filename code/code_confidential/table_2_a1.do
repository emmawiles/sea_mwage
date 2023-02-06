
set more off
set matsize 11000
clear all
capture log close

************************************************************************
* PREFIX TO SAVE RESULTS
***********************************************************************
local Prefix "Table_2_a1_"
local datename = trim("$S_DATE")

*********************************************************************
* PATHS 
*********************************************************************
global path_project "R:\\Project\SeattleMinimumWage\\Stata\\AttenuationPaper\\dofiles\\AEJEP_submission_2020\\"
global path_ESD "R:\Project\SeattleMinimumWage\Data\State Data\ESD\data\"
global path_cpi "${path_project}\\data\\"
global path_geo_data "${path_project}\\data\\data_confidential\\"
global path_analysis_data "${path_project}\\data\\data_confidential\\"
global path_collapsed_data "${path_project}\\data\\data_confidential\\"

log using "${path_project}\\log\\log_confidential\\`Prefix'`datename'.log", replace

**********************************************************
**** COUNT THE NUMBER OF EINs AND EMPLOYEACCOUNTNUMNERS ***
***********************************************************

	* Get identifiers based on EIN to merge them to establishment level data 
	tempfile EIN_to_ACCT
	use "${path_ESD}ESD_Wages_v4.dta"
	
	drop if yearquarter > 20163
	
	rename employeraccountnumber acct
	keep acct taxid yearquarter 
	duplicates drop
	isid acct yearquarter
	
	save `EIN_to_ACCT'
	
	* Load establishment level data

	use "${path_analysis_data}establishment_analysis_Spring_2018_100.dta"
		
	* Merge EINs 
	merge m:1 acct yearquarter using `EIN_to_ACCT'
	drop if _merge == 2
	drop _merge

	* Assign missing region to region "9" (these are state-wide, county wide businesses etc.)
	replace region = 9 if region == .
	replace region = 9 if county_id == .		

	rename naics2 industry

	*** Drop industries with unreliable data ***
	drop if naicscode == 624120 | naicscode == 814000 
	
	* Region and naics2
	gen year = int(yearquarter/10)
	
	* Combine some industries
	* Manufacturing 31-33
	replace industry = 31 if industry == 32 | industry == 33
	* Retail Trade 44-45
	replace industry = 44 if industry == 45
	* Transportation and Warehousing
	replace industry = 48 if industry == 49
	
	* Modify region 
	gen region_coarse = 1 * (region < 9) + 2 * (region >= 9)
	
	**************************************
	*** Establishment level sum. stat. ***
	**************************************
	
	***** Collapse sample to establishment level -- jobs which pay <$19 *****
	preserve
		keep if wagegroup <= 18
		collapse (sum) nworkers_beg if operating == 1, by(acct taxid industry yearquarter region_coarse multiest)
		gen operating = 1
		drop if yearquarter == 20051 | yearquarter == 20164

		* By industry
		collapse (sum) nworkers_beg operating, by(yearquarter region_coarse industry multiest)
		gen in_analysis = (region_coarse == 1 & multiest == 0) 
		collapse (sum) nworkers_beg operating, by(yearquarter industry in_analysis)
		
		egen total_nworkers_beg = sum(nworkers_beg), by(industry yearquarter)
		egen total_operating = sum(operating), by(industry yearquarter)	

		gen share_nworkers_beg = nworkers_beg / total_nworkers_beg
		gen share_operating = operating / total_operating
		
		tabstat nworkers_beg operating share_nworkers_beg share_operating if in_analysis == 1, stats(mean) by(industry)
		tabstat nworkers_beg operating if in_analysis == 0, stats(mean) by(industry)

		* All industries combined
		collapse (sum) nworkers_beg operating, by(yearquarter in_analysis)
		
		egen total_nworkers_beg = sum(nworkers_beg), by(yearquarter)
		egen total_operating = sum(operating), by(yearquarter)	

		gen share_nworkers_beg = nworkers_beg / total_nworkers_beg
		gen share_operating = operating / total_operating		

		tabstat nworkers_beg operating share_nworkers_beg share_operating, stats(mean) by(in_analysis)
	restore	
	
	preserve
		keep if wagegroup <= 18
		collapse (sum) nworkers_beg if operating == 1, by(acct taxid industry yearquarter region_coarse multiest)
		gen operating = 1
		gen in_analysis = (region_coarse == 1 & multiest == 0) 
		drop if yearquarter == 20051 | yearquarter == 20164

		* All industries combined
		collapse (sum) nworkers_beg operating, by(yearquarter in_analysis multiest)
		
		egen total_nworkers_beg = sum(nworkers_beg), by(yearquarter)
		egen total_operating = sum(operating), by(yearquarter)	

		gen share_nworkers_beg = nworkers_beg / total_nworkers_beg
		gen share_operating = operating / total_operating		

		tabstat nworkers_beg operating share_nworkers_beg share_operating if in_analysis==0, stats(mean) by(multiest)
	restore		
		
	***** Collapse sample to establishment level -- all jobs *****
	preserve
		collapse (sum) nworkers_beg if operating == 1, by(acct taxid industry yearquarter region_coarse multiest)
		gen operating = 1
		drop if yearquarter == 20051 | yearquarter == 20164
		gen in_analysis = (region_coarse == 1 & multiest == 0) 
		
		* Average no. employees per firm
		tabstat nworkers_beg if in_analysis == 1, stats(mean sd)	
		tabstat nworkers_beg if in_analysis == 0, stats(mean sd)	
		tabstat nworkers_beg if in_analysis == 0 & multiest==0, stats(mean sd)	
		tabstat nworkers_beg if in_analysis == 0 & multiest==1, stats(mean sd)		

		collapse (sum) nworkers_beg operating, by(yearquarter region_coarse industry multiest)
		gen in_analysis = (region_coarse == 1 & multiest == 0) 
		collapse (sum) nworkers_beg operating, by(yearquarter industry in_analysis)
		
		
		egen total_nworkers_beg = sum(nworkers_beg), by(industry yearquarter)
		egen total_operating = sum(operating), by(industry yearquarter)	

		gen share_nworkers_beg = nworkers_beg / total_nworkers_beg
		gen share_operating = operating / total_operating
		
		tabstat nworkers_beg operating share_nworkers_beg share_operating if in_analysis == 1, stats(mean) by(industry)
		tabstat nworkers_beg operating share_nworkers_beg share_operating if in_analysis == 1, stats(sd) by(industry)
		tabstat nworkers_beg operating if in_analysis == 0, stats(mean) by(industry)
		tabstat nworkers_beg operating if in_analysis == 0, stats(sd) by(industry)
		
		collapse (sum) nworkers_beg operating, by(yearquarter in_analysis)
		
		egen total_nworkers_beg = sum(nworkers_beg), by(yearquarter)
		egen total_operating = sum(operating), by(yearquarter)	

		gen share_nworkers_beg = nworkers_beg / total_nworkers_beg
		gen share_operating = operating / total_operating		

		tabstat nworkers_beg operating share_nworkers_beg share_operating, stats(mean) by(in_analysis)
	restore
	
	preserve
		collapse (sum) nworkers_beg if operating == 1, by(acct taxid industry yearquarter region_coarse multiest)
		gen operating = 1
		drop if yearquarter == 20051 | yearquarter == 20164
		gen in_analysis = (region_coarse == 1 & multiest == 0) 
				
		collapse (sum) nworkers_beg operating, by(yearquarter in_analysis multiest)
		
		egen total_nworkers_beg = sum(nworkers_beg), by(yearquarter)
		egen total_operating = sum(operating), by(yearquarter)	

		gen share_nworkers_beg = nworkers_beg / total_nworkers_beg
		gen share_operating = operating / total_operating		

		tabstat nworkers_beg operating share_nworkers_beg share_operating if in_analysis == 0, stats(mean) by(multiest)
	restore	
	
	****************************
	*** EIN level sum. stat. ***
	****************************
	
	preserve
	* Collapse sample to EIN level	
	collapse (sum) nworkers_beg, by(taxid yearquarter region_coarse multiest)
	gen operating = 1

	gen in_analysis = (region_coarse == 1 & multiest == 0) 
		
	* Average no. employees per firm
	tabstat nworkers_beg if in_analysis == 1, stats(mean sd)	
	tabstat nworkers_beg if in_analysis == 0, stats(mean sd)
	tabstat nworkers_beg if in_analysis == 0 & multiest == 0, stats(mean sd)
	tabstat nworkers_beg if in_analysis == 0 & multiest == 1, stats(mean sd)
	
	collapse (sum) nworkers_beg operating, by(yearquarter region_coarse multiest)
	gen in_analysis = (region_coarse == 1 & multiest == 0) 
		
	tabstat operating if in_analysis == 0, stats(mean) by(multiest)
	tabstat operating if in_analysis == 1, stats(mean) by(multiest)

	restore 

		
log close
di "Done"


