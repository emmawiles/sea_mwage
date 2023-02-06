args sample_size

set more off
set matsize 11000
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
global path_geo_data "${path_project}\\data\\data_confidential\\"
global path_analysis_data "${path_project}\\data\\data_confidential\\"
global path_collapsed_data "${path_project}\\data\\data_confidential\\"
log using "${path_project}\\log\\log_confidential\\Jobs_analysis_`Prefix'`datename'.log", replace
*******************************************************************************************************
* CHOOSE WHAT TO RUN
*******************************************************************************************************

* Aggregate the data at the establishment level 
local do_build_analysis_data = 1
* Prepare and save the data for the Diff-in-Diff analysis 
local do_sumstat_analysis_DiD = 1
* Prepare and save the data for the SCM and IFE analysis 
local do_sumstat_analysis_Synth = 1

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
	*Drop new data from 2016
	drop if yearquarter > 20163
	
	* Merge definition of a region based on geocoding ***
	merge m:1 address_id using "${path_geo_data}Geocodes_v4.dta", keepusing(region county_id puma_id)
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
		
	*** PROCESS DATA ***

	* Inflate wages  
	merge m:1 yearquarter using `CPI'
	drop if _merge==2 
	drop if _merge==1  
	replace wages = wages*deflator
	drop deflator _merge

	**** Clean up likely measurement error (very high wage rates )
    * Constructed variables
    gen wagerate = wages / hours
    * Note: making choice to treat any wagerates over $500 as missing (i.e., assuming it reflects faulty data) if less than 10 hours reported in quarter (this is what the Oregon Employment Department does)
	replace wagerate = . if wagerate < 9
	replace wagerate = . if wagerate > 500 & hours < 10 
	replace wagerate = . if hours == 0
    replace wagerate = . if hours > 1000
	
	* Time variable
	gen time=1+-4*(2014.75-(int(yearquarter/10)+(yearquarter-int(yearquarter/10)*10)/4)) 	/* normalize so that july, 1 2014 (3rd quarter) is 1)  Since first quarter of 2005 is our first quarter, t=-37 is minimum */
	*tab time  /// what does this tell us?

	* Separations and hires for continuing firms
	sort acct personid time 
	gen separatesnextquarter=(acct==acct[_n+1] & (personid~=personid[_n+1] | (personid==personid[_n+1] & time+1~=time[_n+1]))) if time<6
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

	**** COLLAPSE THE DATA TO THE ESTABLISHMENT/TIME LEVEL ****

	* Total payroll, hours, number of workers
	unab sum_hours: hours_*
	unab sum_workers: nworkers_*
	unab sum_payroll: payroll_*

	*Collapse at the establishment level 
	collapse (sum)	`sum_workers' `sum_hours' `sum_payroll' wages  hiredthisquarter   ///
			(max) multiest region naics2 naicscode county_id puma_id  ///
					if wagerate !=. , by(acct time yearquarter wagegroup) fast			

	**** CHECK FOR ESTABLISHMENTS WITH 0 HOURS ****
	gen hourszero=hours_flow==0
	tabstat hourszero, by(yearquarter)
	* NOTE: sharp change after Q2 2009, from 5% to less than 1% and almost 0 afterwards

	* First time period when establishment appears in the data
	egen time_in = min(time), by(acct)
	sum time_in

	* Last time period when establishment appears in the data
	egen time_out = max(time), by(acct)		
	sum time_out			

	* **** CHECK LONGITUDINAL CONSISTENCY ****
	sort acct time	

	di _n "MULTI-LOCATION ESTABLISHMENTS"
	tab multiest

	*** DROP INDUSTRIES WITH UNRELIABLE DATA ***
	drop if naicscode == 624120 | naicscode == 814000 
		
	*** Now, give all establishments 44 rows of data -- one for each of the 44 quarters from first quarter of 2005 to 4th quarter of 2015 				
	preserve
	collapse (mean) nworkers_flow, by(acct)
	drop nworkers_flow
	expand 44	
	sort acct
	gen time=-37
	replace time=time[_n-1]+1 if acct==acct[_n-1]
	sort acct time
	save temp.dta, replace
	restore
	sort acct time
	merge acct time using temp.dta
	drop _merge
	erase temp.dta
	sort acct time
	gen operating = nworkers_flow~=.	 /* "=1 in all quarters with any paid employees" */

	egen temp = mean(yearquarter), by(time)
	replace yearquarter = temp if yearquarter == .
	drop temp

	egen temp = max(time_in), by(acct)
	replace time_in = temp if time_in == . 
	drop temp

	egen temp = max(time_out), by(acct)
	replace time_out = temp if time_out == .
	drop temp

	sort acct time


	* Fill in region and naics2 for those with missing region and naics2.  Use first observed region and naics2 to fill in all prior quarter's region and naics2.  Use last observed region and naics2 to fill in all subsequent quarter's region and naics2.
	replace region=region[_n-1] if region==. & acct==acct[_n-1]
	replace county_id=county_id[_n-1] if county_id==. & acct==acct[_n-1]
	replace naics2=naics2[_n-1] if naics2==. & acct==acct[_n-1]
	replace naicscode=naicscode[_n-1] if naicscode==. & acct==acct[_n-1]
	gsort acct -time
	replace region=region[_n-1] if region==. & acct==acct[_n-1]
	replace county_id=county_id[_n-1] if county_id==. & acct==acct[_n-1]
	replace naics2=naics2[_n-1] if naics2==. & acct==acct[_n-1]
	replace naicscode=naicscode[_n-1] if naicscode==. & acct==acct[_n-1]
	
	sum region naics2 

	* Add firm identifiers
	sort acct yearquarter
	gen temp=acct~=acct[_n-1]
	gen estid=1
	replace estid=estid[_n-1]+temp if _n>1
	sum estid
	drop temp

	sort acct time
	save "${path_analysis_data}establishment_analysis_Spring_2018_`sample_size'.dta", replace
}	

* LOAD THE ANALYSIS DATA

if `do_sumstat_analysis_DiD'==1 | `do_sumstat_analysis_Synth'==1 {
	tempfile analysis

	* load the data	
	use using "${path_analysis_data}establishment_analysis_Spring_2018_`sample_size'.dta"

	* Assign missing region to region "9" (these are state-wide, county wide businesses etc.)
	replace region = 9 if region == .
	replace region = 9 if county_id ==.		

	rename naics2 industry
	gen ffood = 1 if (naicscode > 719999 & naicscode < 730000)
	gen hhs = 1 if (naicscode > 619999 & naicscode < 630000)
	capture drop city 
	
	* Year
	gen year = int(yearquarter/10)
	
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

	save "`analysis'", replace
}	

	*******************************************	
	**** Save data for DiD region analysis ****
	*******************************************

if `do_sumstat_analysis_DiD'==1 {
	
	foreach I in all food  {	

	use "`analysis'", clear	
	
		di _n "DnD data prep -- `I' " 

		gen all = 1
		gen retail = industry == 44
		gen food = ffood ==1
		gen health = hhs==1
		keep if `I' == 1			
		
		* COLLAPSE TO Save time series
		collapse (sum)  payroll_flow payroll_beg hours_flow hours_beg nworkers_beg wages N = operating (max) industry, by(region yearquarter wagegroup)	

		* Make sure that the panel is complete
		tempfile FullPanel
		preserve
			keep region yearquarter
			duplicates drop 
			expand 32 
			sort region yearquarter
			by region yearquarter: gen wagegroup = 8 + _n
			tab wagegroup
			save `FullPanel', replace
		restore
		
		merge 1:1 region yearquarter wagegroup using `FullPanel'
		drop _merge 
		foreach var in payroll_flow payroll_beg hours_flow hours_beg nworkers_beg wages N {
			replace `var' = 0 if `var' == .
		}
		
		* CREATE TIME SERIES
		sort  region yearquarter wagegroup 
		drop if wagegroup ==.
		egen region_wage = group(region wagegroup)
		egen region_time = group(region yearquarter)		
		gen year = floor(yearquarter/10)
		gen quarter = yearquarter - 10*floor(yearquarter/10)
		gen date_q = yq(year,quarter)

		sort region_wage yearquarter
		tsset region_wage date_q, format(%tq)
		
		*CREATE ANALYSIS VARIABLES
		rename region region0	
		
		* BINS ANALYSIS 
		*Generate Variables at Each Wage Group Level for wagesrates =9 to wagerates =40
			gen bin_payroll_flow = payroll_flow
			gen bin_payroll_beg = payroll_beg
			gen bin_hours_flow = hours_flow
			gen bin_nworkers_beg = nworkers_beg
			
		*Generate earnings and wagerate bins
			gen bin_earnings = bin_payroll_beg / bin_nworkers_beg
			gen bin_mean_wagerate = bin_payroll_flow / bin_hours_flow
				
		* Year-Over-Year Differences
			sort region0 wagegroup date_q
			foreach V in bin_payroll_flow bin_payroll_beg bin_hours_flow bin_nworkers_beg bin_earnings bin_mean_wagerate {
					 gen d_`V' = `V' / `V'[_n-4]
			}

		* GENERATE VARIABLES CUMULATIVELY FROM THE BOTTOM
			sort region0 date_q wagegroup
			gen cum_payroll_flow = payroll_flow if wagegroup==9
			gen cum_payroll_beg = payroll_beg if wagegroup==9
			gen cum_hours_flow = hours_flow if wagegroup==9
			gen cum_nworkers_beg = nworkers_beg  if wagegroup==9

			sort region0 date_q wagegroup
			forvalues i = 10/40{
				foreach V in payroll_flow payroll_beg hours_flow nworkers_beg {
						replace cum_`V'= `V' + cum_`V'[_n-1] if wagegroup == `i'
				}
			}			
					
		*Generate EARNINGS and WAGERATE
			gen cum_earnings = cum_payroll_beg / cum_nworkers_beg
			gen cum_mean_wagerate = cum_payroll_flow / cum_hours_flow
				
		* Format everything correctly		
			foreach V in payroll_flow payroll_beg hours_flow nworkers_beg earnings mean_wagerate {
				format cum_`V' %16.0g 
			}
				
		* Year-Over-Year Differences
			sort region_wage date_q
			foreach V in cum_payroll_flow cum_payroll_beg cum_hours_flow cum_nworkers_beg cum_earnings cum_mean_wagerate {			
				gen d_`V' = `V' / L4.`V'
			}
		
		unab bins: bin_payroll_flow bin_payroll_beg bin_hours_flow bin_nworkers_beg bin_earnings bin_mean_wagerate d_bin_payroll_flow d_bin_payroll_beg d_bin_hours_flow d_bin_nworkers_beg d_bin_earnings d_bin_mean_wagerate
		unab cumulative: cum_payroll_flow  cum_payroll_beg cum_hours_flow cum_nworkers_beg cum_earnings cum_mean_wagerate  d_cum_payroll_flow d_cum_payroll_beg d_cum_hours_flow d_cum_nworkers_beg d_cum_earnings d_cum_mean_wagerate
	
		**CREATE DATASETS
		drop industry
		capture drop health
		
		preserve
			drop `cumulative' 
			reshape wide region_wage N payroll_flow payroll_beg hours_flow wages nworkers_beg hours_beg  `bins' , i(region_time) j(wagegroup)

			gen T = 0
			replace T= 1  if yearquarter == 20143 & region0 == 1
			replace T= 2  if yearquarter == 20144 & region0 == 1
			replace T= 3  if yearquarter == 20151 & region0 == 1
			replace T= 4  if yearquarter == 20152 & region0 == 1
			replace T= 5  if yearquarter == 20153 & region0 == 1
			replace T= 6  if yearquarter == 20154 & region0 == 1
			replace T= 7  if yearquarter == 20161 & region0 == 1
			replace T= 8  if yearquarter == 20162 & region0 == 1
			replace T= 9  if yearquarter == 20163 & region0 == 1
			save "${path_collapsed_data}\\`Prefix'DiD_bins_prep_`I'.dta", replace
		restore
	
		preserve 
			drop `bins'
			
			reshape wide region_wage N payroll_flow payroll_beg hours_flow wages nworkers_beg hours_beg `cumulative' , i(region_time) j(wagegroup)

			gen T = 0
			replace T = 1  if yearquarter == 20143 & region0 == 1
			replace T = 2  if yearquarter == 20144 & region0 == 1
			replace T = 3  if yearquarter == 20151 & region0 == 1
			replace T = 4  if yearquarter == 20152 & region0 == 1
			replace T = 5  if yearquarter == 20153 & region0 == 1
			replace T = 6  if yearquarter == 20154 & region0 == 1
			replace T = 7  if yearquarter == 20161 & region0 == 1
			replace T = 8  if yearquarter == 20162 & region0 == 1
			replace T = 9  if yearquarter == 20163 & region0 == 1
			
			save "${path_collapsed_data}\\`Prefix'DiD_cumulative_prep_`I'.dta", replace
		restore
	}
}


if `do_sumstat_analysis_Synth'==1 {
					
	** Save data for synthetic control region analysis ****
	foreach I in all food  {	
	
	use "`analysis'", clear

	
		di _n "Synth data prep -- `I' "
			
		gen all = 1
		gen retail = industry==44
		gen food = ffood ==1
		gen health = hhs==1
		keep if `I'==1		
			
		* COLLAPSE TO Save time series for each 
		collapse (sum) payroll_flow payroll_beg hours_flow hours_beg wages nworkers_beg N = operating (max) industry health, by(region puma_id yearquarter wagegroup)	

		* Make sure that the panel is complete
		tempfile FullPanel
		preserve
			keep region puma_id yearquarter
			duplicates drop 
			expand 32 
			sort region puma_id yearquarter
			by region puma_id yearquarter: gen wagegroup = 8 + _n
			tab wagegroup
			save `FullPanel', replace
		restore
		
		merge 1:1 region puma_id yearquarter wagegroup using `FullPanel'
		drop _merge 
		foreach var in payroll_flow payroll_beg hours_flow hours_beg nworkers_beg N {
			replace `var' = 0 if `var' == .
		}
		
							
		* CREATE TIME SERIES
			sort  region puma_id wagegroup yearquarter
			drop if wagegroup==.
			egen region_wage = group(region puma_id wagegroup)
			egen region_time = group(region puma_id yearquarter)
			drop if region_wage==.
			gen year = floor(yearquarter/10)
			gen quarter = yearquarter - 10*floor(yearquarter/10)
			
			gen date_q = yq(year,quarter)
			sort region_wage date_q
			tsset region_wage date_q, format(%tq)
		
		*CREATE ANALYSIS VARIABLES	
			rename region region0
			rename puma_id puma_id0				
		
		*BINS ANALYSIS
		* Generate Variables at Each Wage Group Level for wagerate = 9 to wagerate = 40
			gen bin_payroll_flow = payroll_flow
			gen bin_payroll_beg = payroll_beg
			gen bin_hours_flow = hours_flow
			gen bin_nworkers_beg = nworkers_beg
				
		* Generate earnings and wagerate bins
			gen bin_earnings = bin_payroll_beg / bin_nworkers_beg
			gen bin_mean_wagerate = bin_payroll_flow / bin_hours_flow
				
		* Year-Over-Year Differences
			sort region0 puma_id0 wagegroup date_q
			foreach V in bin_payroll_flow bin_payroll_beg bin_hours_flow bin_nworkers_beg bin_earnings bin_mean_wagerate {
				gen d_`V' = `V' / `V'[_n-4]
			}

		* GENERATE VARIABLES CUMULATIVELY FROM THE BOTTOM
			sort region0 puma_id0 date_q wagegroup
			gen cum_payroll_flow=payroll_flow if wagegroup == 9
			gen cum_payroll_beg = payroll_beg if wagegroup == 9
			gen cum_hours_flow = hours_flow if wagegroup == 9
			gen cum_nworkers_beg = nworkers_beg  if wagegroup == 9

			sort region0 puma_id0 date_q wagegroup
			forvalues i = 10/40{
				foreach V in payroll_flow payroll_beg hours_flow nworkers_beg {
					replace cum_`V'= `V' + cum_`V'[_n-1] if wagegroup==`i'
				}
			}		
					
		*Generate EARNINGS and WAGERATE bins
			gen cum_earnings = cum_payroll_beg / cum_nworkers_beg
			gen cum_mean_wagerate = cum_payroll_flow / cum_hours_flow
				
		* Format everything correctly		
			foreach V in payroll_flow payroll_beg hours_flow nworkers_beg  earnings mean_wagerate {
					format cum_`V' %16.0g 
			}
					
		* Year-Over-Year Differences
			sort region_wage date_q
			foreach V in cum_payroll_flow  cum_payroll_beg  cum_hours_flow  cum_nworkers_beg  cum_earnings  cum_mean_wagerate {			
				gen d_`V' = `V' / L4.`V'
			}		
					
		unab bins: bin_payroll_flow bin_payroll_beg bin_hours_flow bin_nworkers_beg bin_earnings bin_mean_wagerate d_bin_payroll_flow d_bin_payroll_beg d_bin_hours_flow d_bin_nworkers_beg d_bin_earnings d_bin_mean_wagerate
		unab cumulative: cum_payroll_flow cum_payroll_beg cum_hours_flow cum_nworkers_beg cum_earnings cum_mean_wagerate  d_cum_payroll_flow d_cum_payroll_beg d_cum_hours_flow d_cum_nworkers_beg d_cum_earnings d_cum_mean_wagerate
	
		*** CREATE DATASETS
		drop industry
		capture drop health
		preserve
			drop `cumulative' 	
			
			* Lagged (at t-4) hours and no workers 
			* To use as weights for aggregating PUMAs into Seattle 
			gen L4_bin_hours = L4.bin_hours_flow
			gen L4_bin_nmworkers = L4.bin_nworkers_beg			
			
			reshape wide region_wage N payroll_flow payroll_beg hours_flow wages nworkers_beg hours_beg L4_bin_hours L4_bin_nmworkers `bins', i(region_time) j(wagegroup)
		
			* Treatment periods (Seattle minimum wage)
			gen R = 0
			replace R = 1  if yearquarter == 20143 & region0 == 1
			replace R = 2  if yearquarter == 20144 & region0 == 1
			replace R = 3  if yearquarter == 20151 & region0 == 1
			replace R = 4  if yearquarter == 20152 & region0 == 1
			replace R = 5  if yearquarter == 20153 & region0 == 1
			replace R = 6  if yearquarter == 20154 & region0 == 1
			replace R = 7  if yearquarter == 20161 & region0 == 1
			replace R = 8  if yearquarter == 20162 & region0 == 1
			replace R = 9  if yearquarter == 20163 & region0 == 1
		
		save "${path_collapsed_data}\\`Prefix'synth_bin_prep_`I'.dta", replace
		restore	

		preserve
			drop `bins' 
			
			* Lagged (at t-4) hours and no workers 
			* To use as weights for aggregating PUMAs into Seattle 
			gen L4_cum_hours = L4.cum_hours_flow
			gen L4_cum_nmworkers = L4.cum_nworkers_beg			
			
			reshape wide region_wage N payroll_flow payroll_beg hours_flow wages nworkers_beg hours_beg `cumulative' L4_cum_hours L4_cum_nmworkers, i(region_time) j(wagegroup)
				
			* Treatment periods (Seattle minimum wage)					
			gen R = 0
			replace R = 1  if yearquarter == 20143 & region0 == 1
			replace R = 2  if yearquarter == 20144 & region0 == 1
			replace R = 3  if yearquarter == 20151 & region0 == 1
			replace R = 4  if yearquarter == 20152 & region0 == 1
			replace R = 5  if yearquarter == 20153 & region0 == 1
			replace R = 6  if yearquarter == 20154 & region0 == 1
			replace R = 7  if yearquarter == 20161 & region0 == 1	
			replace R = 8  if yearquarter == 20162 & region0 == 1	
			replace R = 9  if yearquarter == 20163 & region0 == 1	
				
			save "${path_collapsed_data}\\`Prefix'synth_cumulative_prep_`I'.dta", replace

		restore	
		capture rename region0 region
	}
}	

log close

	