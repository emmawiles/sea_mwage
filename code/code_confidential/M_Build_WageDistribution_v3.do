args sample_size
* Note: use this for testing to run on a subsample -- to run on the full sample choose sample_size = 100

set more off, perm
clear
capture log close

*******************************************************************************************************
* PREFIX TO SAVE RESULTS
*******************************************************************************************************

local Prefix "WageDistribution_samplesize`sample_size'"
local datename = trim("$S_DATE")


*******************************************************************************************************
* PATHS 
*******************************************************************************************************

global path_project "R:\\Project\SeattleMinimumWage\\Stata\\AttenuationPaper\\dofiles\\AEJEP_submission_2020\\"
global path_ESD "R:\Project\SeattleMinimumWage\Data\State Data\ESD\data\"
global path_cpi "${path_project}\\data\\"
global path_geo_data "${path_project}data\\data_confidential\\"
global path_output "${path_project}data\\data_confidential\\"

log using "${path_project}\\log\\`Prefix'`datename'.log", replace


*******************************************************************************************************
* CHOOSE WHAT TO RUN
*******************************************************************************************************

local do_build_analysis_data = 1
local do_entry_exit = 1
local do_wage_dist = 1
local do_median_wage = 1

*******************************************************************************************************
* CHOOSE SPECIFICATION
*******************************************************************************************************

*** Base period for prices ***
local base_price 20152

*** Type of quarterly CPI deflator ***
* options are "last" (last month of the quarter) 
* or "average" (average through the quarter)
local cpi_type "last"

*** CPI file (can choose CPI-U or CPI-W) ***
*options are "cpi_u" or "cpi_w"
local cpi_file "cpi_w_00_16"

*******************************************************************************************************
* LOAD AND PROCESS THE CPI DATA
*******************************************************************************************************

if `do_build_analysis_data'==1 {
	tempfile CPI

	insheet using "${path_cpi}`cpi_file'.csv", comma

	* Note: CPI-W is series CWUR0000SA0, Not Seasonally Adjusted, U.S. city average, All items
	* Note: CPI-U is series CUUR0000SA0, Not Seasonally Adjusted, U.S. city average, All items

	sort year month
	if "`cpi_type'"=="average" {
		collapse (mean) cpi, by(year quarter)
	}
	if "`cpi_type'"=="last" {
		collapse (last) cpi, by(year quarter)
	}
	gen yearquarter = year*10 + quarter
	egen cpi_base = max(cpi * (yearquarter == `base_price'))
	gen deflator = cpi_base / cpi
	assert deflator < .

	keep yearquarter deflator
	save `CPI', replace
}

if `do_build_analysis_data'==1 {

	if `sample_size'== 100 {
		use "${path_ESD}ESD_Wages_v4.dta"
	}

	if `sample_size'< 100 {
		* temporaily using a fraction of the sample	
		use "${path_ESD}ESD_Wages_v4.dta" if personid-int(personid/100)*100<=`sample_size'
	}
	
	* Merge definition of a region based on geocoding ***
	merge m:1 address_id using "${path_geo_data}Geocodes_v4.dta", keepusing(region puma_id)
	drop if _merge == 2 
	drop _merge 
	
	rename employeraccountnumber acct
	rename totalwageamount wages
	rename totalhours hours

	*** Drop multi-establishment firms ***
	drop if multiest == 1

	*** Collapse multiple records per quarter ***
	sort acct personid yearquarter 
    by acct personid yearquarter: gen dupl = cond(_N==1,0,_N)
    tab dupl
          
    collapse (sum) hours wages (max)  region puma_id address_id naicscode naics2 multiest taxid ///
			 (count) n_obs_quarter = dupl, by(acct personid yearquarter) fast
		
	*** Inflate wages ***  
	merge m:1 yearquarter using `CPI'
	drop if _merge==2 
	drop if _merge==1  
	replace wages = wages*deflator
	drop deflator _merge

	**** Clean up likely measurement error (very high wage rates )
    gen wagerate = wages / hours
    * Note: making choice to treat any wagerates over $500 as missing (i.e., assuming it reflects faulty data) if less than 10 hours reported in quarter (this is what the Oregon Employment Department does)
	replace wagerate = . if wagerate < 9
	replace wagerate = . if wagerate > 500 & hours < 10
	replace wagerate = . if hours == 0
    replace wagerate = . if hours > 1000

	* Time variable
	gen time=1+-4*(2014.75-(int(yearquarter/10)+(yearquarter-int(yearquarter/10)*10)/4)) 	/* normalize so that july, 1 2014 (3rd quarter) is 1)  Since first quarter of 2005 is our first quarter, t=-37 is minimum */

	* Separations and hires for continuing firms
	sort acct personid time 
	gen separatesnextquarter=(acct==acct[_n+1] & (personid~=personid[_n+1] | (personid==personid[_n+1] & time+1~=time[_n+1]))) if time < 9
	gen hiredthisquarter=(acct==acct[_n-1] & (personid~=personid[_n-1] | (personid==personid[_n-1] & time-1~=time[_n-1]))) if time > -37
	
	gen nworkers = 1
	
	*** GENERATE TWO DEFINITIONS OF NUMBER OF WORKERS ***
	
	* 1. Beginning of quarter jobs
	gen nworkers_beg = hiredthisquarter == 0
	* 2. Flow employment== All active worker accounts 
	gen nworkers_flow = nworkers	
	
	***** GENERATE TWO DEFINITIONS FOR NUMBER OF HOURS *****
	
	* 1. Beginning of quarter jobs
	gen hours_beg = hours if hiredthisquarter == 0 & wagerate < .	
	* 2. Flow employment == All active worker accounts 
	gen hours_flow = hours if wagerate < .
	
	* Correctly format hours	
	format hours_beg %16.0g 	
	format hours_flow %16.0g 		

	***** EARNINGS FOR THE WHOLE QUARTER *****
	
	* 1. Beginning of quarter jobs
	gen payroll_beg = wages if hiredthisquarter == 0 & wagerate < .	
	* 2. Flow employment == All active worker accounts 
	gen payroll_flow = wages if wagerate < .
	
	* Correctly format earnings
	format payroll_flow %16.0g 
	format payroll_beg %16.0g 	

	*** Assign missing region to region "9" (these are state-wide, county wide businesses etc.) **
	replace region = 9 if region == .

	*** Recode industry ***
	rename naics2 industry
	gen ffood = 1 if (naicscode >= 722000 & naicscode < 723000)
	gen hhs = 1 if (naicscode >= 620000 & naicscode < 630000)

	*** Combine some industries
	* Manufacturing 31-33
	replace industry = 31 if industry == 32 | industry == 33
	* Retail Trade 44-45
	replace industry = 44 if industry == 45
	* Transportation and Warehousing
	replace industry = 48 if industry == 49

	*** Drop industries with unreliable data ***
	drop if naicscode == 624120 | naicscode == 814000 

}	

**********************************************
*** SURVIVING/ENTERING/EXITING BUSINESSES ***
**********************************************

if `do_entry_exit' == 1 {
	tempfile EntryExit
	preserve
		gen operating = 1
		keep acct yearquarter operating
		duplicates drop
		gen date_q = yq(floor(yearquarter/10),yearquarter - 10*floor(yearquarter/10))
		tsset acct date_q
		gen entry_L4 = (L4.operating != 1)
		gen exit_F4 = (F4.operating != 1)
		gen surviving_L4 = (L4.operating == 1)
		gen surviving_F4 = (F4.operating == 1)
		drop date_q
		save `EntryExit', replace
	restore

	merge m:1 acct yearquarter using `EntryExit'
	drop _merge
}

****************************************************
*** COLLAPSE THE DATA TO PLOT WAGE DISTRIBUTIONS ***
****************************************************

if `do_wage_dist' == 1 {

	* Program to collapse wage distribution
	capture program drop WageDist
	program define WageDist
		args if lbl_stepup lbl_t lbl_series filename replace
		
		di "Stepup: `lbl_stepup'"
		di "Period: `lbl_t' (condition `if')"
		di "Series: `lbl_series' (condition `if')"

		preserve
		*** Wage grid for real wage rate in the current period ***
		capture drop w_grid
		gen w_grid = floor(wagerate*10)
		replace w_grid = w_grid/10 + 0.10
		*** Truncate at $40 
		replace w_grid = 40 if w_grid >= 40 & w_grid < .

		capture gen N = 1
		collapse (sum) hours n_obs = N if wagerate < . `if', by(w_grid yearquarter region) fast
		gen period_type = `lbl_t'
		gen series_type = `lbl_series'
		gen MW_stepup = `lbl_stepup'

		if "`replace'"=="append" {
			append using `filename'
		}
			save `filename', replace
		restore
	end	

	* Wage distribution in Seattle
	* Note: may be will add Outlying King vs. SKP later 
	* 2014.2 vs 2015.2 vs 2017.2

	*** Local conditions for "before" and "after" periods
	local t_before 20122 20142 20152 20162
	local t_after  20132 20152 20162 20172
	local MW_step 9 11 13 15
	local n_MW: list sizeof MW_step

	*** Loop through "before" dates ***
	local replace "replace"
	forvalues i=1/`n_MW' {	
		local t: word `i' of `t_before'
		local StepUp: word `i' of `MW_step'
		* All businesses
			WageDist "& yearquarter==`t'" "`StepUp'" "0" "0" "$path_output\\WageDistribution_NBER_WP_v2.dta" "`replace'"
		* Surviving businesses
			WageDist "& yearquarter==`t' & surviving_F4 == 1" "`StepUp'" "0" "2" "$path_output\\WageDistribution_NBER_WP_v2.dta" "append"
		* Exiting businesses
			WageDist "& yearquarter==`t' & exit_F4 == 1" "`StepUp'" "0" "3" "$path_output\\WageDistribution_NBER_WP_v2.dta" "append"
		local replace "append"	
	}	

	*** Loop through "after dates" ***
	forvalues i=1/`n_MW' {	
		local t: word `i' of `t_after'
		local StepUp: word `i' of `MW_step'
		* All businesses
			WageDist "& yearquarter==`t'" "`StepUp'" "1" "0" "$path_output\\WageDistribution_NBER_WP_v2.dta" "append"
		* Surviving businesses
			WageDist "& yearquarter==`t' & surviving_L4 == 1" "`StepUp'" "1" "2" "$path_output\\WageDistribution_NBER_WP_v2.dta" "append"
		* Entering businesses
			WageDist "& yearquarter==`t' & entry_L4 == 1" "`StepUp'" "1" "1" "$path_output\\WageDistribution_NBER_WP_v2.dta" "append"
	}	
}

******************************************
*** MEDIAN HOURLY WAGE RATE IN SEATTLE ***
******************************************

if `do_median_wage'==1 {
	preserve 
		collapse (median) wagerate if region == 1 & multiest == 0 & yearquarter >= 20142 [aweight = hours], by(yearquarter)
		rename wagerate wagerate_weighted
		save "$path_output\\MedianWage_Seattle.dta", replace
	restore 
	preserve 
		collapse (median) wagerate if region == 1 & multiest == 0 & yearquarter >= 20142, by(yearquarter)
		rename wagerate wagerate_unweighted
		merge 1:1 yearquarter using "$path_output\\MedianWage_Seattle.dta"
		drop _merge 
		save "$path_output\\MedianWage_Seattle.dta", replace
	restore	
}


log close

di "Done"

		