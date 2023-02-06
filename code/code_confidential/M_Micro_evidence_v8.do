args sample_size version

set more off
capture clear
capture log close

*******************************************************************************************************
* PREFIX TO SAVE RESULTS
*******************************************************************************************************
local Prefix "establishments_samplesize`sample_size'"
local datename = trim("$S_DATE")

*******************************************************************************************************
* PATHS 
*******************************************************************************************************

global path_project "R:\\Project\SeattleMinimumWage\\Stata\\AttenuationPaper\\dofiles\\AEJEP_submission_2020\\"
global path_ESD "R:\Project\SeattleMinimumWage\Data\State Data\ESD\data\"
global path_cpi "${path_project}\\data\\"
global path_geo_data "${path_project}data\\data_confidential\\"
global path_output "${path_project}data\\data_confidential\\"


log using "${path_project}\\log\\log_confidential\\Micro_evidence_`datename'.log", replace
*******************************************************************************************************
* CHOOSE WHAT TO RUN
*******************************************************************************************************

local do_build_analysis_data = 1
local do_transition_CDF = 1
local do_transition_multi_site = 1
local do_transition_matrix_all = 1

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

*** Process data on CPI deflator ***

if `do_build_analysis_data'==1 {
	tempfile CPI

	insheet using "${path_cpi}\\`cpi_file'.csv", comma

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

*** Construct main analysis sample ***

if `do_build_analysis_data'==1 {

	if `sample_size'== 100 {
		use "${path_ESD}ESD_Wages_v4.dta"
	}

	if `sample_size'< 100 {
		* temporaily using a fraction of the sample	
		use "${path_ESD}ESD_Wages_v4.dta" if personid-int(personid/100)*100<=`sample_size'
	}

	* Drop erroneously reported records from 2016.4
	drop if yearquarter > 20163
	
	* Merge definition of a region based on geocoding ***
	merge m:1 address_id using "${path_ESD}Geocodes_v4.dta", keepusing(region county_id puma_id)
	drop if _merge == 2 
	drop _merge 
	
	rename employeraccountnumber acct
	rename totalwageamount wages
	rename totalhours hours

	**** Flag multiple records per quarter
	sort acct personid yearquarter 
    by acct personid yearquarter: gen dupl = cond(_N==1,0,_N)
    tab dupl
	
    collapse (sum) hours wages (first) county (max)  region county_id puma_id naicscode naics2 multiest taxid (count) n_obs_quarter = dupl, by(acct personid yearquarter) fast
			
	* Inflate wages  
	merge m:1 yearquarter using `CPI'
	drop if _merge==2 
	drop if _merge==1  
	replace wages = wages*deflator
	drop deflator _merge

	**** Clean up likely measurement error (very high wage rates)
    * Constructed variables
    gen wagerate = wages / hours
    * Note: making choice to treat any wage rates over $500 as missing 
    * (i.e., assuming it reflects faulty data) if less than 10 hours 
    * reported in quarter (this is what the Oregon Employment Department does)
	replace wagerate =. if wagerate < 9
	replace wagerate =. if wagerate > 500 & hours < 10
	replace wagerate =. if hours == 0
    replace wagerate =. if hours > 1000
	
	*** Drop industries with unreliable data ***
	drop if naicscode == 624120 | naicscode == 814000 
	
	* Time variable
	gen time=1+-4*(2014.75-(int(yearquarter/10)+(yearquarter-int(yearquarter/10)*10)/4)) 	/* normalize so that july, 1 2014 (3rd quarter) is 1)  Since first quarter of 2005 is our first quarter, t=-37 is minimum */

	* Separations and hires for continuing jobs
	sort acct personid time 
	gen separatesnextquarter=(acct==acct[_n+1] & (personid~=personid[_n+1] | (personid==personid[_n+1] & time+1~=time[_n+1]))) if time<9
	gen hiredthisquarter=(acct==acct[_n-1] & (personid~=personid[_n-1] | (personid==personid[_n-1] & time-1~=time[_n-1]))) if time>-37
	
	gen nworkers = 1
	
	** GENERATE TWO DEFINITIONS OF NUMBER OF WORKERS
	
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
	
	***** WAGERATE GROUP FOR THE WHOLE QUARTER *****	
	gen wagegroup = floor(wagerate)
	assert wagegroup >= 9
	replace wagegroup = 40 if wagegroup >=40 & wagegroup < .

	***** Job stayers *****
	isid acct personid yearquarter	
	
	egen long job_id = group(acct personid)	
	isid job_id yearquarter		

	* Stata time format
	gen year = floor(yearquarter/10)
	gen quarter = yearquarter - 10*floor(yearquarter/10)
	gen date_q = yq(year,quarter)

	* Job stayers (over the year change)
	sort job_id date_q
	tsset job_id date_q	
	gen job_stayer = L4.date_q < .

	**** Wages in t-4 for job stayers ****
	gen wagerate_L4 = L4.wagerate

	**** Wages in t+4 for job stayers ****
	gen wagerate_F4 = F4.wagerate
	
	**** Check if employers change type ****
	gen multiest_L4 = L4.multiest if L4.date_q < .
	
	tab multiest multiest_L4

	**** Status in t-4 ****
	gen job_status_L4 = 1 * (L4.date_q ==.) + 2 * (L4.date_q < . & L4.wagerate < 19) + ///
						3 * (L4.date_q < . & L4.wagerate >= 19 & L4.wagerate < .) + ///
						9 * (L4.date_q < . & L4.wagerate == .)
	* Codes
	* 1 -- new hire
	* 2 -- continuing job, payed below 19
	* 3 -- continuing job, payed above 19
	* 9 -- continuing job, had missing wage rate

	**** Status in t-1 ****
	gen job_status_L1 = 1 * (L1.date_q ==.) + 2 * (L1.date_q < . & L1.wagerate < 19) + ///
						3 * (L1.date_q < . & L1.wagerate >= 19 & L1.wagerate < .) + ///
						9 * (L1.date_q < . & L1.wagerate == .)
	* Codes
	* 1 -- new hire
	* 2 -- continuing job, payed below 19
	* 3 -- continuing job, payed above 19
	* 9 -- continuing job, had missing wage rate
	**** Status in t+4 ****
	gen job_status_F4 = 2 * (F4.date_q < . & F4.wagerate < 19) + ///
						3 * (F4.date_q < . & F4.wagerate >= 19 & F4.wagerate < .) + ///
						4 * (F4.date_q == .) + ///
						9 * (F4.date_q < . & F4.wagerate == .)
	* Codes
	* 2 -- continuing job, payed below 19 in t+4
	* 3 -- continuing job, payed above 19 in t+4
	* 4 -- separation
	* 9 -- continuing job, missing wagerate in t+4
}	

*******************************************************************************************************
* PREPARE THE COLLAPSED DATA FOR FURTHER ANALYSIS
*******************************************************************************************************

************************************
***                              ***
*** Transition matrices for jobs ***
*** Year-over-year changes       ***
***                              ***
************************************

if `do_transition_CDF'==1 {

	
	*********************************************************
	*** Probability to transition to a job above $19/hour ***
	*********************************************************
	
	*** Job status in t + 1 and in t+ 4 ***
	tempfile WorkerHistory_F1 WorkerHistory_F4  
	preserve
		gen Seattle_above19 = (region==1 & multiest == 0 & wagerate >= 19 & wagerate < .)
		egen any_Seattle_above19 = max(Seattle_above19), by(personid yearquarter)
		gen outsideKing_above19 = (region > 2 & region < 9 & multiest == 0 & wagerate >= 19 & wagerate < .)
		egen any_outsideKing_above19 = max(outsideKing_above19), by(personid yearquarter)
		egen max_job_status_L4 = max(job_status_L4), by(personid yearquarter)
		gen all_jobs_new_F4 = (max_job_status_L4 == 1)
		egen max_job_status_L1 = max(job_status_L1), by(personid yearquarter)
		gen all_jobs_new_F1 = (max_job_status_L1 == 1)
		
		keep any_Seattle_above19 any_outsideKing_above19 all_jobs_new_F1 all_jobs_new_F4 personid date_q  
		duplicates drop 
		rename any_Seattle_above19 any_Seattle_above19_F4 
		rename any_outsideKing_above19 any_outsideKing_above19_F4
		rename all_jobs_new_F1 all_jobs_new_F1_INCORRECT
		replace date_q = date_q - 4 
		save `WorkerHistory_F4', replace

		drop all_jobs_new_F4
		rename all_jobs_new_F1_INCORRECT all_jobs_new_F1 
		rename any_Seattle_above19 any_Seattle_above19_F1 
		rename any_outsideKing_above19 any_outsideKing_above19_F1
		replace date_q = date_q + 4 - 1
		save `WorkerHistory_F1', replace	
		
	restore 
	
	merge m:1 personid date_q using `WorkerHistory_F4'
	drop all_jobs_new_F1_INCORRECT
	drop _merge 

	merge m:1 personid date_q using `WorkerHistory_F1'
	drop _merge  
	
	gen any_above19_F1 = any_Seattle_above19_F1 * (region == 1) + any_outsideKing_above19_F1 * (region > 2 & region < 9)
	gen any_above19_F4 = any_Seattle_above19_F4 * (region == 1) + any_outsideKing_above19_F4 * (region > 2 & region < 9)

	*** Probability to transition to $19 and more, PUMA - yearquarter level ***
	*** t+ k ***
	capture gen N = 1
	local replace replace
	foreach k in 1 4 {
		*** All workers ***
		preserve
			collapse (mean) any_above19_F`k' (sum) hours = N (count) jobs = wages if wagerate < 19 & multiest == 0 [aweight=hours_flow], by(region puma_id yearquarter year quarter) fast
			gen all_jobs = 1
			gen job_stayer = 0
			gen job_switcher = 0
			save "${path_output}Prob_upgraded_below_above_19_`k'_v`version'.dta", replace
		restore
		*** Job stayers ***
		preserve
			collapse (mean) any_above19_F`k' (sum) hours = N (count) jobs = wages if wagerate < 19 & multiest == 0 & all_jobs_new_F`k' == 0 [aweight=hours_flow], by(region puma_id yearquarter year quarter) fast
			gen all_jobs = 0
			gen job_stayer = 1
			gen job_switcher = 0
			append using "${path_output}Prob_upgraded_below_above_19_`k'_v`version'.dta"
			save "${path_output}Prob_upgraded_below_above_19_`k'_v`version'.dta", replace
		restore
		*** Job switchers ***
		preserve
			collapse (mean) any_above19_F`k' (sum) hours = N (count) jobs = wages if wagerate < 19 & multiest == 0 & all_jobs_new_F`k' == 1 [aweight=hours_flow], by(region puma_id yearquarter year quarter) fast
			gen all_jobs = 0
			gen job_stayer = 0
			gen job_switcher = 1
			append using "${path_output}Prob_upgraded_below_above_19_`k'_v`version'.dta"
			save "${path_output}Prob_upgraded_below_above_19_`k'_v`version'.dta", replace
		restore	
	}	
}

*********************************************
***                                       ***
*** Transitions into multi-site employers ***
*** Year-over-year changes                ***
***                                       ***
*********************************************

if `do_transition_multi_site'==1 {

	*************************
	*** Job status in t+4 ***
	*************************
	
	sort job_id date_q 
	gen job_stayer_F4 = F4.date_q < .
	gen with_job = 1
	
	tempfile WorkerHistory
	preserve 
		collapse (max) with_job multiest, by(personid date_q)
		sort personid date_q 
		tsset personid date_q
		gen with_job_F4 = F4.date_q < .
		gen multiest_F4 = F4.multiest
		keep personid date_q with_job_F4 multiest_F4
		save `WorkerHistory', replace
	restore

	merge m:1 personid date_q using `WorkerHistory'
	drop if _merge == 2
	drop _merge
	
	sort job_id date_q

	**** Status in t+4 ****
	capture drop job_status_F4
	gen job_status_F4 = 1 * (F4.date_q < .) + 2 * (F4.date_q == .)
	* Codes
	* 1 -- job stayer
	* 2 -- separations

	* Workers who separated from a single-site employer
	* in t but transitioned to a multi-site employer in t+4
	
	capture gen N = 1	

	preserve
		* Keep only single-site establishments
		keep if multiest == 0
		* Keep only jobs paying <$19
		keep if wagegroup < 19

		collapse (sum) with_job_F4 multiest_F4 N if wagerate < 19 & wagerate < . & job_status_F4 == 2, by(region puma_id year quarter date_q) fast

		gen P_withjob = with_job_F4 / N
		gen P_multiest_uncond = multiest_F4 / N
		gen P_multiest_cond = multiest_F4 / with_job_F4

		replace year = year + 1
		gen yearquarter = 10*year + quarter

		save "${path_output}\transitions_to_multisite_v`version'.dta", replace	
restore
}

	***********************************************
	*** Change in hours worked by group         ***
	*** i.e. hires, separations, wage upgrading ***
	***********************************************

	* Hours(t) / Hours(t-4) = 
	*        1
	* (1)    + Hours(hires since t-2) / Hours(t-4)
	* (2)    - Hours(separations since t-4) / Hours(t-4)
	* (3)    + delta_Hours(cont. jobs which paid <$19 in t-4 and pay <$19 in t) / Hours(t-4)
	* (5)    + Hours(cont. jobs which paid >$19 in t-4 and pay <$19 in t) / Hours(t-4)
	*		 - Hours(cont. jobs which pay >$19 in t but paid <$19 in t-4) / Hours(t-4)
	* (9)	 + Hours(cont. jobs which paid missing in t-4 and pay <$19 in t) / Hours(t-4)
	*		 - Hours(cont. jobs which pay missing in t but paid <$19 in t-4) / Hours(t-4)

if `do_transition_matrix_all'==1 {

	capture gen N = 1

	*** Hours in jobs paying <$19 in t-4 ***
	*** and Hours in jobs which existed in t-4 but ceased to exist by t ***
	preserve
		* Keep only single-site establishments
		keep if multiest == 0
		* Keep only jobs paying <$19
		keep if wagegroup < 19

		collapse (sum) hours_flow N if wagerate < ., by(job_status_F4 region puma_id date_q) fast
		reshape wide hours_flow N, i(region puma_id date_q) j(job_status_F4)

		qui gen L4_hours_flow = hours_flow2 + hours_flow3 + hours_flow4 + hours_flow9
		qui gen L4_N = N2 + N3 + N4

		rename hours_flow3 L4_hours_flow3
		rename hours_flow2 L4_hours_flow2
		rename hours_flow4 L4_hours_flow4
		rename hours_flow9 L4_hours_flow9
		rename N2 L4_N2
		rename N4 L4_N4
		rename N9 L4_N9

		replace date_q = date_q + 4
		save "${path_output}\delta_hours_decomposed_jobs_below_19_separations_v`version'.dta", replace		
	restore

	*** Hours in jobs which existed in t ***
	preserve
		* Keep only single-site establishments
		keep if multiest == 0
		*** Wage grid for <$19 vs. >=19$ in t-4 ***
		gen w_grid_19 = 1 * (wagerate < 19) + 2 * (wagerate >= 19) if wagerate < .

		collapse (sum) hours_flow N if wagerate < . & puma_id < ., by(job_status_L4 w_grid_19 region puma_id yearquarter year quarter date_q) fast
		* Drop jobs which paid >19 in t-4 and in t
		drop if job_status_L4 == 3 & w_grid_19 == 2
		* Drop new hires into >19
		drop if job_status_L4 == 1 & w_grid_19 == 2
		* Drop transitions from missing wagerate into >19
		drop if job_status_L4 == 9 & w_grid_19 == 2
		* New variable for status by transition
		* New hires
		qui gen job_group = 1 if job_status_L4 == 1 & w_grid_19 == 1
		* Payed >19 in t-4, pays <19 in t 
		qui replace job_group = 2 if job_status_L4 == 3 & w_grid_19 == 1
		* Payed <19 in t-4, pays <19 in t
		qui replace job_group = 3 if job_status_L4 == 2 & w_grid_19 == 1
		* Payed <19 in t-4, pays >19 in t
		qui replace job_group = 5 if job_status_L4 == 2 & w_grid_19 == 2		
		* Payed missing wage in t-4, pays < 19 in t
		qui replace job_group = 8 if job_status_L4 == 9 & w_grid_19 == 1		
		* Check that the new variable is never missing
		assert job_group < .
		drop job_status_L4 w_grid_19

		reshape wide hours_flow N, i(region puma_id yearquarter) j(job_group)

		* foreach var in hours_flow N {
		* 	rename `var'1 `var'_hires
		* 	rename `var'2 `var'_L4_below19
		* 	rename `var'3 `var'_L4_19up
		* 	rename `var'3 `var'_L4_19up
		* }

		merge 1:1 region puma_id date_q using "${path_output}\delta_hours_decomposed_jobs_below_19_separations_v`version'.dta"
		drop if _merge == 2
		drop _merge

		qui gen hours_all = hours_flow1 + hours_flow2 + hours_flow3 + hours_flow8
		qui gen region_puma_id = real(strofreal(region) + strofreal(puma_id))

		tsset region_puma_id date_q

		qui gen L4_hours_all = L4.hours_all
		assert L4_hours_all == L4_hours_flow if L4_hours_all < . & L4_hours_flow < .
		replace L4_hours_all = L4_hours_flow if L4_hours_all == .

		qui gen d_hours_hires = hours_flow1 / L4_hours_all
		qui gen d_hours_seps = - L4_hours_flow4 / L4_hours_all
		qui gen d_hours_hires_seps = (hours_flow1 - L4_hours_flow4) / L4_hours_all
		qui gen d_hours_down = hours_flow2  / L4_hours_all
		qui gen d_hours_contjobs = (hours_flow3 - L4_hours_flow2) / L4_hours_all
		qui gen d_hours_up = - L4_hours_flow3 / L4_hours_all
		qui gen d_hours_up_down = (hours_flow2 - L4_hours_flow3) / L4_hours_all
		qui gen d_hours_missing = (hours_flow8 - L4_hours_flow9) / L4_hours_all
		
		gen d_hours_abs_0 = hours_all - L4_hours_all
		gen d_hours_abs_1 = hours_flow1 - L4_hours_flow4 + hours_flow3 - L4_hours_flow2 + hours_flow2 - L4_hours_flow3 + hours_flow8 - L4_hours_flow9 
*  		Check that hours are the same
*		Note: there is a 1 hour difference in 1 observation	
*		assert d_hours_abs_0 == d_hours_abs_1 if d_hours_abs_0 < . & d_hours_abs_1 < .
		
		gen d_hours_all_0 = hours_all / L4_hours_all - 1 
		gen d_hours_all_1 = d_hours_hires_seps + d_hours_contjobs + d_hours_up_down + d_hours_missing 
*		Check that obs. are the same 
*		Note: all obs are the same up to 4th digit
*		Note: there are 7 obs which are different in the 6th digit and 3 obs which are dif in the 5th digit		
*		assert floor(1e6*d_hours_all_0)/1e6 == floor(1e6*d_hours_all_1)/1e6 if d_hours_all_0 < . & d_hours_all_1 < .
*       assert floor(1e5*d_hours_all_0)/1e5 == floor(1e5*d_hours_all_1)/1e5 if d_hours_all_0 < . & d_hours_all_1 < .
*		assert floor(1e4*d_hours_all_0)/1e4 == floor(1e4*d_hours_all_1)/1e4 if d_hours_all_0 < . & d_hours_all_1 < .

		save "${path_output}\delta_hours_decomposed_jobs_below_19_v`version'.dta", replace		
	restore

}



log close
di "Done"
		