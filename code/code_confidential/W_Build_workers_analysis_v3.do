
args sample_size 



set more off
set matsize 11000
capture clear
capture log close

*******************************************************************************************************
* PREFIX TO SAVE RESULTS
*******************************************************************************************************

local Prefix "workers_samplesize`sample_size'_"

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
log using "${path_project}\\log\\log_confidential\\Build_`Prefix'`datename'.log", replace

*******************************************************************************************************
* CHOOSE WHAT TO RUN
*******************************************************************************************************
local do_build_data = 1
local do_build_worker_analysis_data = 1

local do_build_cohort_analysis = 1
local do_falsification_cohort = 1




*******************************************************************************************************
* LOAD AND PROCESS THE CPI DATA
*******************************************************************************************************

if `do_build_data'==1 {
	
	

	*** Base period for prices ***
	local base_price 20152

	*** Type of quarterly CPI deflator ***
	* options are "last" (last month of the quarter) 
	* or "average" (average through the quarter)
	local cpi_type "last"

	*** CPI file (can choose CPI-U or CPI-W) ***
	*options are "cpi_u" or "cpi_w"
	local cpi_file "cpi_w_00_16"
	
	
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

*******************************************************************************************************
* LOAD & PROCESS THE ESD DATA & GEOCODES
*******************************************************************************************************

if `do_build_data'==1 {

	if `sample_size'==100 {
		use "${path_ESD}\\ESD_Wages_v4.dta"
	}
	if `sample_size'<100 {
		* temporaily using a fraction of the sample	
		use "${path_ESD}\\ESD_Wages_v4.dta" if personid-int(personid/100)*100<=`sample_size'
	}

**** Using only data from 2005-15 due to peculiarities in city addresses before 2005
	keep if yearquarter>=20051 & yearquarter~=.
	drop if yearquarter >20163
	
	
	* Merge definition of a region based on geocoding *** bad_data 
	merge m:1 address_id using  "${path_geo_data}\\Geocodes_v4.dta", keepusing(bad_data puma_id region)
	drop if _merge == 2
	drop _merge
	
	* Check to see if any regions or PUMAS are missing
	tab region, m
	tab puma_id, m

	* Generate new EAN to protect confidentiality
	egen long acct = group(employeraccountnumber)
	
	*drop unnecessary identifier variables
	drop city address
	tabulate bad_data
*** Flag multiple records per quarter
	sort acct personid yearquarter 
    by acct personid yearquarter: gen dupl = cond(_N==1,0,_N)
    tab dupl
    collapse (sum) totalhours totalwageamount (first) county  (max)  region bad_data puma_id nonunique_address    /// 
	 zipcode unknowncounty address_id  zip9 zip5  naicscode naics2 multiest taxid   ///
	(count) n_obs_quarter = dupl, by(acct personid yearquarter) 

}



*******************************************************************************************************
****CREATE  WORKER_ANALYSIS DATA
*******************************************************************************************************

if `do_build_worker_analysis_data'==1 {

**** First handle multiple jobs mismeasurement
	* For employees with multiple employment records in a quarter, we define industry and puma by employer for which employee works the most hours in the quarter, 
	*and compute sum of wages and hours.
	gsort personid yearquarter -totalwageamount
	by personid yearquarter: egen rank_wages = rank(-totalwageamount)
	egen multiplejobs=sum(1), by(personid yearquarter)
	tab multiplejobs
	count if multiplejobs>1
	count if multiplejobs>1 & totalwageamount~=. & totalhours==.
	count if multiplejobs>1 & totalwageamount==. & totalhours~=.
	
	* for those with wages but not hours (or vice versa) and multiple jobs, our wage rate computation will be flawed (and thus we will need to set it to missing).
	gen temp=(multiplejobs>1 & ((totalwageamount~=. & totalhours==.) | (totalwageamount==. & totalhours~=.)))
	egen wagerateflawed=max(temp), by(personid yearquarter)
	tab wagerateflawed
	drop temp
	
	* for those with hours from some jobs, but not others, our hours computation will be flawed (and thus we will need to set it to missing).
	gen temp1=totalhours==.
	gen temp2=totalhours~=.
	egen temp3=max(temp1), by(personid yearquarter)
	egen temp4=max(temp2), by(personid yearquarter)
	gen hoursflawed=temp4==temp3
	tab hoursflawed
	drop temp*
	
	* for those with wages from some jobs, but not others, our wages computation will be flawed (and thus we will need to set it to missing).
	gen temp1=totalwageamount==.
	gen temp2=totalwageamount~=.
	egen temp3=max(temp1), by(personid yearquarter)
	egen temp4=max(temp2), by(personid yearquarter)
	gen wagesflawed=temp4==temp3
	tab wagesflawed
	drop temp*

	*generate wages as total wages , merging two jobs into 1
	egen wages=sum(totalwageamount), by(personid yearquarter)
	replace wages=. if wagesflawed==1
	egen hours=sum(totalhours), by(personid yearquarter)
	replace hours=. if hoursflawed==1
	gen totalhours_seattle=totalhours*(region==1)
	egen hours_seattle=sum(totalhours_seattle), by(personid yearquarter)
	replace hours_seattle=. if hoursflawed==1
	sum wages hours hours_seattle
	
	* Check how many people have multiple jobs in different regions or switch regions within a quarter
	sort personid yearquarter acct
	gen regionchange = region!=region[_n-1] if personid==personid[_n-1] & yearquarter==yearquarter[_n-1]
	replace regionchange = 0 if regionchange==.
	egen multipleregions = max(regionchange), by(personid yearquarter)
	drop regionchange
	tab multipleregions
	tab region multipleregions, row
	tab multiplejobs multipleregions, row


	* Jobs with largest hours for each person
	gen share_hours = totalhours / hours

	* For those with multiple same hours  choose a job with biggest wages
	gen share_wages = totalwageamount / wages
	gen s_temp =0
	replace s_temp = share_wages if region ==1
	by personid yearquarter: egen sea_dosage = sum(s_temp)
	*Seattle dosage is the dosage of multiple jobs in seattle 0-1
	drop s_temp
	gen c_temp= 0
	replace c_temp= share_wages if (region ==4 | region ==5)
	*WA dosage is the dosage of multiple jobs in WA 0-1
	by personid yearquarter: egen wa_dosage= sum(c_temp)	
	drop c_temp
	egen max_share_wages = max(share_wages), by(personid yearquarter)
	egen max_sea_wages = max(sea_dosage), by (personid yearquarter)
	egen max_con_wages = max(wa_dosage), by (personid yearquarter)
	egen obs_max_share_wages = sum(share_wages == max_share_wages), by(personid yearquarter)  
	tab obs_max_share_wages	

	* For those with multiple same earnings choose a job with biggest hours
	egen max_share_hours = max(share_hours), by(personid yearquarter)
	egen obs_max_share_hours = sum(share_hours == max_share_hours), by(personid yearquarter)
	tab obs_max_share_hours if obs_max_share_wages > 1

	* Pick observation to represent the PUMA/region
	gen obs_largest_wages = share_wages == max_share_wages if obs_max_share_wages == 1
	replace obs_largest_wages = share_hours == max_share_hours if obs_max_share_wages > 1 & obs_max_share_hours == 1
	by personid yearquarter: replace obs_largest_wages = _n == 1 if obs_largest_wages == . 
	tab obs_largest_wages, m
	
	egen n_obs_largest_wages = sum(obs_largest_wages), by(personid yearquarter)
	tab n_obs_largest_wages
	qui sum n_obs_largest_wages
	assert `r(max)' == 1
	assert `r(min)' == 1
	count if obs_largest_wages == .
	assert `r(N)' == 0

	drop share_hours max_share_hours obs_max_share_hours 
	drop share_wages max_share_wages obs_max_share_wages
	drop n_obs_largest_wages	
		
	* puma per worker instead of puma per job
	sort personid yearquarter acct
	gen pumachange = puma_id!=puma_id[_n-1] if personid==personid[_n-1] & yearquarter==yearquarter[_n-1]
	replace pumachange = 0 if pumachange==.
	egen multiplepuma_id = max(pumachange), by(personid yearquarter)
	gen puma_max_wages1 = puma_id * obs_largest_wages
	egen puma_max_wages = max(puma_max_wages1), by(personid yearquarter)
    drop puma_max_wages1 
	
	* Region with max wages
	gen region_max_wages1 = region * obs_largest_wages
	egen region_max_wages = max(region_max_wages1), by(personid yearquarter)
	drop region_max_wages1
	
	* Employer with max wages
	gen long acct_max_wages1 = acct if obs_largest_wages == 1
	replace acct_max_wages1 = 0 if obs_largest_wages == 0
	egen long acct_max_wages= max(acct), by(personid yearquarter)
	drop acct_max_wages1
	
	* Industry with max wages
	gen long naics_max_wages1 = naicscode if obs_largest_wages == 1
	replace naics_max_wages1 = 0 if obs_largest_wages == 0
	egen long naics_max_wages= max(naicscode), by(personid yearquarter)
	drop naics_max_wages1
	
	
	gen temp_cohort1_acct=acct_max_wages if yearquarter==20151
	gen temp_cohort2_acct=acct_max_wages if yearquarter==20154
	gen temp_pseduocohort1_acct=acct_max_wages if yearquarter==20121
	gen temp_pseduocohort2_acct=acct_max_wages if yearquarter==20124	
	bys personid: egen cohort1_acct=max(temp_cohort1_acct)
	bys personid: egen pseduocohort1_acct=max(temp_pseduocohort1_acct)	
	bys personid: egen cohort2_acct=max(temp_cohort2_acct)
	bys personid: egen pseduocohort2_acct=max(temp_pseduocohort2_acct)	
	drop temp_cohort1_acct temp_cohort2_acct temp_pseduocohort1_acct temp_pseduocohort2_acct
	
	
	gen empl_coh1_baseline= 0
	gen empl_coh2_baseline= 0
	gen empl_pseudocoh1_baseline= 0
	gen empl_pseudocoh2_baseline= 0
	replace empl_coh1_baseline=(acct_max_wages==cohort1_acct)
	replace empl_coh2_baseline=(acct_max_wages==cohort2_acct)
	replace empl_pseudocoh1_baseline=(acct_max_wages==pseduocohort1_acct)
	replace empl_pseudocoh2_baseline=(acct_max_wages==pseduocohort2_acct)
	
	
*** Capture the share of workers affected by multiestablishments**
	* Create multiest, nonunique_address and region defintions per person rather than per job
	egen multiest_person = max(multiest), by(personid yearquarter)

**** Check how many jobs are affected by address and multilocation issue
	di _n "JOBS AT MULTI-LOCATION FIRM"
	tab multiest
	di _n "JOBS IN UNDEFINED REGIONS"
	tab region

	di _n "JOBS AT MULTI-LOCATION FIRM, PEOPLE WITH VALID WAGES AND HOURS"
	tab multiest if wages < . & hours < .
	di _n "JOBS IN UNDEFINED REGIONS, PEOPLE WITH VALID WAGES AND HOURS"
	tab region if wages < . & hours < .	

	drop county  zipcode unknowncounty address_id zip9 zip5 taxid
	
**** Convert data to Person - YearQuarter format *******
	drop if personid==personid[_n-1] & yearquarter==yearquarter[_n-1]
	replace multiplejobs=multiplejobs>1
	
	drop multiest
	rename multiest_person multiest

	replace region = region_max_wages
	drop region_max_wages

	replace puma_id = puma_max_wages
	drop puma_max_wages

	replace acct = acct_max_wages
	drop acct_max_wages
	
	replace naicscode = naics_max_wages
	drop naics_max_wages
	
	replace wa_dosage = max_con_wages
	drop max_con_wages
	replace sea_dosage= max_sea_wages
	drop max_sea_wages

	
**** Check how many people (rather than jobs) are affected by address and multilocation issue*****
	di _n "PEOPLE AT LEAST ONE JOB WITH MULTI-LOCATION FIRM"
	tab multiest
	di _n "PEOPLE AT LEAST ONE JOB IN UNDEFINED REGION"
	tab region

**** Check how many people (rather than jobs) are affected by address and multilocation issue
	di _n "PEOPLE AT LEAST ONE JOB WITH MULTI-LOCATION FIRM, PEOPLE WITH VALID WAGES AND HOURS"
	tab multiest if wages < . & hours < .	
	di _n "PEOPLE AT LEAST ONE JOB IN UNDEFINED REGION, PEOPLE WITH VALID WAGES AND HOURS"
	tab region if wages < . & hours < .	

**** Inflate wages
	merge m:1 yearquarter using `CPI'
	drop if _merge==2
	assert _merge==3
	replace wages = wages*deflator
	drop deflator _merge

	* Constructed variables
	gen wagerate=wages/hours
	*replace wagerate=. if wagerateflawed==1
	sum wagerate, d
**** Clean up likely measurement error (very high wage rates )
    * Note: making choice to treat any wagerates over $500 as missing (i.e., assuming it reflects faulty data) if less than 10 hours reported in quarter (this is what the Oregon Employment Department does)
	replace wagerateflawed=1 if wagerate < 8
	replace wagerateflawed =1 if wagerate > 500 & hours <10
	replace wagerateflawed =1 if hours == 0
    replace wagerateflawed = 1 if hours > 2190
	sum wagerate if wagerateflawed !=1	
	

	
	*Generate total number of quarters ever employed by baseline
	gen n=1
	sort personid yearquarter
	by personid : egen count = rank(yearquarter)
	gen temp_mw2015= count if yearquarter ==20151
	gen temp_mw2016= count if yearquarter ==20154

	by personid: egen dur_emp2015= max(temp_mw2015)
	by personid: egen dur_emp2016= max(temp_mw2016)

	drop temp_mw2015 temp_mw2016 
	sort acct personid yearquarter
	
	gen eeid= acct + personid
	*Generate duration
	gen year=int(yearquarter/10)
	tostring yearquarter , generate(yearQ)
	gen quarter = substr(yearQ , 5,1)
	destring quarter, replace
	gen YQdate=yq(year, quarter)
	format YQdate %tq
	sort personid YQdate
	**check that person-date are unique identifiers
	gen test =(personid==personid[_n-1] & YQdate==YQdate[_n-1])
	tab test
	assert test ==0
	drop test
	gen hiredthisquarter =  (eeid!=eeid[_n-1] | eeid==eeid[_n-1] & (YQdate !=YQdate[_n-1]+1))
	gen separatesnextquarter =  (eeid!=eeid[_n+1] | eeid==eeid[_n+1] & (YQdate !=YQdate[_n+1]-1))	
	gen c = (separatesnextquarter==0)
	gen c_2015= c if yearquarter ==20151
	gen c_2016=c if yearquarter ==20154
	gen c_2017= c if yearquarter ==20164


**** Generate Time variable --- NEW DEFINITION
	gen time=1+-4*(2014.75-(int(yearquarter/10)+(yearquarter-int(yearquarter/10)*10)/4)) 	/* normalize so that july, 1 2014 (3rd quarter) is 1)  Since first quarter of 2005 is our first quarter, t=-37 is minimum */
	tab time
	* First time period when person appears in the data
	egen t_in = min(time), by(personid)
	qui sum t_in

	sort personid yearquarter
	gen t_diff= 0
	replace t_diff= time-time[_n-1] if (hiredthisquarter==1 & separatesnextquarter[_n-1]==1) & (personid==personid[_n-1]) 
	gen time_diff= t_diff-1
	replace time_diff=. if time_diff==-1
	
	**Gen new entrants after 2010
	gen temp_ent = (t_in ==time)
	gen temp_ent_hrs= hours if t_in ==time
	replace temp_ent = 0 if yearquarter< 20101
	replace temp_ent = 1 if (time_diff >19 & time_diff !=.)
	replace temp_ent_hrs = 0 if yearquarter< 20101
	replace temp_ent_hrs = hours if (time_diff >19 & time_diff !=.)
	gen entrant = temp_ent if multiest==0 & bad_data==0
	gen entrant_hrs = temp_ent_hrs if multiest==0 & bad_data==0
	gen ent_15= entrant if wagerate<15
	gen ent_19 = entrant if wagerate <19
	gen ent_hrs_15= entrant_hrs if wagerate<15
	gen ent_hrs_19 = entrant_hrs if wagerate <19

	* Now, give all observed workers 50 rows of data -- one for each of the 44 quarters from first quarter of 2005 to 2nd quarter of 2017
	preserve
		collapse (mean) hours, by(personid)
		drop hours
		expand 50	
		sort personid
		gen time=-37
		replace time=time[_n-1]+1 if personid==personid[_n-1]
		sort personid time
		save temp2.dta, replace
	restore
	sort personid time
	merge personid time using temp2.dta
	drop _merge
	erase temp2.dta
	sort personid time
	gen employed=hours~=.	 /* "=1 in all quarters with any observed wages" */
	replace wages=0 if wages==.	
	replace hours=0 if hours==.	
	egen temp=mean(yearquarter), by(time)
	replace yearquarter=temp if yearquarter== .
	drop temp

	* Fill in region for those with missing region. Use first employed region to fill in all prior quarter's region. Use last employed region to fill in all subsequent quarter's region.
	sort personid time
	replace region=region[_n-1] if region==. & personid==personid[_n-1]
	replace puma_id=puma_id[_n-1] if puma_id==. & personid==personid[_n-1]
	replace dur_emp2015= dur_emp2015[_n-1] if dur_emp2015==. & personid==personid[_n-1]
	replace dur_emp2016= dur_emp2016[_n-1] if dur_emp2016==. & personid==personid[_n-1]	
	gsort personid -time
	replace region=region[_n-1] if region==. & personid==personid[_n-1]
	replace puma_id=puma_id[_n-1] if puma_id==. & personid==personid[_n-1]	
	replace dur_emp2015= dur_emp2015[_n-1] if dur_emp2015==. & personid==personid[_n-1]
	replace dur_emp2016= dur_emp2016[_n-1] if dur_emp2016==. & personid==personid[_n-1]		
	
	*Generate duration employed at account at baseline
	gen t2015= time if yearquarter==20151
	gen t2016 = time if yearquarter ==20154
	egen time_mw2015=max(t2015)
	egen time_mw2016= max(t2016)
	drop t2015 t2016
	
	***Generate duration worker is consistently working at 2015.1  employer
	gen a_2015= acct if yearquarter==20151 & acct!=.
	by personid: egen acct_2015= max(a_2015) if acct!=.
	gsort personid -time
	gen ea_2015= 1 if personid==personid[_n-1] & acct==acct_2015 & time <= time_mw2015 & acct_2015!=.
	gen timeb15= time if ea_2015[_n+1]==. & ea_2015==1 & personid==personid[_n-1]
	replace timeb15= time if time==-37 & ea_2015==1
	by personid: egen acct_begin15= max(timeb15)
	by personid: gen d_acct2015 = time_mw2015- acct_begin15 +1
	by personid: egen dur_acct2015 = max(d_acct2015)
	drop a_2015 acct_2015 ea_2015 timeb15 acct_begin15 d_acct2015
	
	***Generate duration worker is consistently working at 2015.4  employer
	gen a_2016= acct if yearquarter==20154 & acct!=.
	by personid: egen acct_2016= max(a_2016) if acct!=.
	gsort personid -time
	gen ea_2016= 1 if personid==personid[_n-1] & acct==acct_2016 & time <= time_mw2016 & acct_2016!=.
	gen timeb16= time if ea_2016[_n+1]==. & ea_2016==1 & personid==personid[_n-1]
	replace timeb16= time if time==-37 & ea_2016==1
	by personid: egen acct_begin16= max(timeb16)
	by personid: gen d_acct2016 = time_mw2016- acct_begin16 +1
	by personid: egen dur_acct2016 = max(d_acct2016)	
	drop a_2016 acct_2016 ea_2016 timeb16 acct_begin16 d_acct2016
	drop time_mw2015 time_mw2016
	
	tab region
	* Combine some industries
	* Manufacturing 31-33
	replace naics2=31 if naics2==32 | naics2==33
	* Retail Trade 44-45
	replace naics2=44 if naics2==45
	* Transportation and Warehousing
	replace naics2=48 if naics2==49
	egen industry= group(naics2)

	replace region= 9 if region ==0
	
	sort personid time
	saveold "${path_analysis_data}\\worker_analysis_Fall_2018_`sample_size'.dta", replace

	clear
}	


*******************************************************************************************************
*Cohort Analysis 
******************************************************************************************************
**Build two datasets for the main and falsification cohort

if `do_build_cohort_analysis'==1 {
forvalues c = 15 {
use "${path_analysis_data}\\worker_analysis_Fall_2018_`sample_size'.dta"

			sort personid yearquarter
			* Redefine time so that Q22015 is 0 for Cohort 1, Q42015 is 0 for Cohort 2
			if `c'==15 {
			gen time_temp = 1+-4*((2015+ .5)-(int(yearquarter/10)+(yearquarter-int(yearquarter/10)*10)/4))
				}
			if `c'==16 {	
			gen time_temp = 1+-4*((2015 + 1.25)-(int(yearquarter/10)+(yearquarter-int(yearquarter/10)*10)/4))			
				}
			tab yearquarter if time_temp==0	
			keep if time_temp >=-3 & time_temp <= 6
			
			*Generate treatment  -- do paren instead of =1
			gen t_temp = (sea_dosage ==1 & time_temp ==0)
			by personid: egen treat = max(t_temp)
			drop t_temp
			*restrict analysis to 100% treatment and 0 percent Seattle/king employment
			gen dosage = max(wa_dosage, sea_dosage)			
			gen c_temp =(dosage ==1 & time_temp ==0)
			by personid: egen cohort = max(c_temp)
			keep if cohort==1
			drop c_temp
			
			gen new_entrant= t_in==time
			
			if `c'==15 {
			gen d_age20`c'= 4 - t_in if time_temp <=0
			bys personid: egen dur_age20`c'= max(d_age20`c')
				}
			if `c'==16 {
			gen d_age20`c'= 7 - t_in if time_temp <=0
			bys personid: egen dur_age20`c'= max(d_age20`c')
				}	
			* Drop people whose t=0 outcomes are missing  **
			egen min_time_temp = min(time_temp), by(personid)
			tab min_time_temp
			drop if min_time_temp != -3
			sort personid time_temp
			drop min_time_temp
			*restrict analysis of wagerate covariates and outcome to only be for valid wagerates
			gen t_wrf= 1 if wagerateflawed==1 & (time_temp>-3 & time_temp<1)
			tab time_temp wagerateflawed
			tab time_temp t_wrf
			bys personid: egen person_wrf= max(t_wrf) 
			drop if person_wrf==1
			drop t_wrf person_wrf
			
			*Generate region-pumaid id to be compatable with Attenuation paper
			egen region_puma_id= concat(region puma_id)
			destring region_puma_id, replace
			*t0--generate baseline variables for time=0, the quarter before implmentation				
			foreach x in hours hours_seattle empl_coh1_baseline empl_coh2_baseline wages wagerate multiplejobs   ///
							employed  new_entrant region puma_id region_puma_id multiest industry yearquarter bad_data{
				gen temp_l`x'0= `x' if time_temp ==0
				by personid: egen `x'_0= max(temp_l`x'0)
				drop temp_l`x'0
				}
			
			*Restrict sample to workers in locatable firms who do not have bad data 
			drop if multiest_0==1
			drop if bad_data_0==1
			drop if region_0==9		
						
			tab region_0
			*Restrict sample to workers with valid wagerates earning <$11 per hour at baseline
			if `c'==15 {
				keep if wagerate_0 < 11
				}
			sort personid time_temp
			*outcome 1--X_1
			forvalues i=1/6 {
				foreach x in hours hours_seattle empl_coh1_baseline empl_coh2_baseline wages wagerate multiplejobs   ///
						employed multiest new_entrant region puma_id region_puma_id   {
					gen temp_`x'`i'= `x' if time_temp ==`i'
					by personid: egen `x'_`i'= max(temp_`x'`i')
					drop temp_`x'`i'
						}	
					}
			*t-3
			foreach x in hours hours_seattle empl_coh1_baseline empl_coh2_baseline wages wagerate multiplejobs   ///
						employed multiest new_entrant region puma_id region_puma_id {
					gen temp_l`x'3= `x' if time_temp ==-3
					by personid: egen `x'_l3= max(temp_l`x'3)
					drop temp_l`x'3
						}
					
			*t-2
			foreach x in hours hours_seattle empl_coh1_baseline empl_coh2_baseline wages wagerate multiplejobs  ///
						employed multiest new_entrant region puma_id region_puma_id  {
					gen temp_l`x'2= `x' if time_temp ==-2
					by personid: egen `x'_l2= max(temp_l`x'2)
					drop temp_l`x'2
						}
			*t-1			
			foreach x in hours hours_seattle empl_coh1_baseline empl_coh2_baseline wages wagerate multiplejobs   ///
						employed multiest new_entrant region puma_id region_puma_id {
					gen temp_l`x'1= `x' if time_temp ==-1
					by personid: egen `x'_l1= max(temp_l`x'1)
					drop temp_l`x'1
						}				
		
			unab hours: hours*
			unab wages: wages*
			unab wagerate: wagerate*
			unab multiplejobs: multiplejobs*
			unab employed: employed*
			unab new_entrant: new_entrant*
			unab region: region*
			unab puma_id: puma_id*
			unab region_puma_id: region_puma_id* 
			unab multiest: multiest*
			unab empl_coh1: empl_coh1_baseline*
			unab empl_coh2: empl_coh2_baseline*
				
			collapse (first) treat  dur_acct20`c'  dur_age20`c' `hours' `empl_coh1' `empl_coh2' `wages' `wagerate' `multiplejobs' `employed' `new_entrant' `region'  `puma_id' `multiest' yearquarter_0 industry_0  , by(personid)
			
			foreach x in treat `hours' `wages'  `employed' `new_entrant' {
				replace `x'=0 if `x'==.
				}

		local ematch employed_0 employed_l1 employed_l2 new_entrant_0 new_entrant_l1 new_entrant_l2
		local cont_cov hours_0 hours_l1 hours_l2 wagerate_0 wagerate_l1 wagerate_l2 dur_acct20`c' `c' dur_age20`c'
		local dependent wages_1 wages_2 wages_3 wages_4 wages_5 wages_6 hours_1 hours_2 hours_3 hours_4 hours_5 hours_6
		
		qui gen flag_nonmis = 1
		foreach var in `ematch' `cont_cov' `dependent' {
			qui replace flag_nonmis = 0 if `var' == .
			}
			
		tab flag_nonmis
		tab treat if flag_nonmis == 1 
		
		* Create grouping variable
		gen emp_group = 1 if (employed_0 ==1 & employed_l1 ==0 & employed_l2 ==0)
		qui replace emp_group = 2 if (employed_0 ==1 & employed_l1 ==1 & employed_l2 ==0)		
		qui replace emp_group = 3 if (employed_0 ==1 & employed_l1 ==0 & employed_l2 ==1)		
		qui replace emp_group = 4 if (employed_0 ==1 & employed_l1 ==1 & employed_l2 ==1)
		
		gen ent_group = 1 if (new_entrant_0 ==1 & new_entrant_l1 ==0 & new_entrant_l2 ==0)
		qui replace ent_group = 2 if (new_entrant_0 ==0 & new_entrant_l1 ==1 & new_entrant_l2 ==0)
		qui replace ent_group = 3 if (new_entrant_0 ==0 & new_entrant_l1 ==0 & new_entrant_l2 ==1)		
		qui replace ent_group = 4 if ent_group==.
		gen exact_match_group = 10*ent_group + emp_group 
		
		*** Summary statistics on the potential number of matches
		tab exact_match_group treat if flag_nonmis == 1 
		tab emp_group treat if flag_nonmis == 1 
		tab ent_group treat if flag_nonmis == 1 
		
		save "${path_collapsed_data}\\mw20`c'_cohort_`sample_size'.dta", replace
	
		clear
		}
	}
	


if `do_falsification_cohort'==1 {
forvalues c = 12 {
use "${path_analysis_data}\\worker_analysis_Fall_2018_`sample_size'.dta"

		if `c'==12 {
		gen t2012= time if yearquarter==20121
		egen time_mw2012=max(t2012)
		drop t2012
			gen a_2012= acct if yearquarter==20121 & acct!=.
			by personid: egen acct_2012= max(a_2012) if acct!=.
			gsort personid -time
			gen ea_2012= 1 if personid==personid[_n-1] & acct==acct_2012 & time <= time_mw2012 & acct_2012!=.
			gen timeb12= time if ea_2012[_n+1]==. & ea_2012==1 & personid==personid[_n-1]
			replace timeb12= time if time==-37 & ea_2012==1
			by personid: egen acct_begin12= max(timeb12)
			by personid: gen d_acct2012 = time_mw2012- acct_begin12 +1
			by personid: egen dur_acct2012 = max(d_acct2012)
			drop a_2012 acct_2012 ea_2012 timeb12 acct_begin12 d_acct2012
			drop time_mw2012
			sort personid yearquarter
			* Redefine time so that Q22015 is 0 for Cohort 1, Q42015 is 0 for Cohort 2
			gen time_temp = 1+-4*((2012+ .5)-(int(yearquarter/10)+(yearquarter-int(yearquarter/10)*10)/4))
				}
			
			tab yearquarter if time_temp==0	
			keep if time_temp >=-3 & time_temp <= 6
			
			*Generate treatment  -- do paren instead of =1
			gen t_temp = (sea_dosage ==1 & time_temp ==0)
			by personid: egen treat = max(t_temp)
			drop t_temp
			*restrict analysis to 100% treatment and 0 percent Seattle/king employment
			gen dosage = max(wa_dosage, sea_dosage)			
			gen c_temp =(dosage ==1 & time_temp ==0)
			by personid: egen cohort = max(c_temp)
			keep if cohort==1
			drop c_temp
			
			gen new_entrant= t_in==time
			
			if `c'==12 {
			gen d_age20`c'= -8- t_in if time_temp <=0
			bys personid: egen dur_age20`c'= max(d_age20`c')
				}
			if `c'==13 {
			gen d_age20`c'= -5- t_in if time_temp <=0
			bys personid: egen dur_age20`c'= max(d_age20`c')
				}	
			* Drop people whose t=0 outcomes are missing  **
			egen min_time_temp = min(time_temp), by(personid)
			tab min_time_temp
			drop if min_time_temp != -3
			sort personid time_temp
			drop min_time_temp
			*restrict analysis of wagerate covariates and outcome to only be for valid wagerates
			gen t_wrf= 1 if wagerateflawed==1 & (time_temp>-3 & time_temp<1)
			tab time_temp wagerateflawed
			tab time_temp t_wrf
			bys personid: egen person_wrf= max(t_wrf) 
			drop if person_wrf==1
			drop t_wrf person_wrf
			*Generate region-pumaid id to be compatable with Attenuation paper
			egen region_puma_id= concat(region puma_id)
			destring region_puma_id, replace		
			*t0--generate baseline variables for time=0, the quarter before implmentation				
			foreach x in hours hours_seattle empl_pseudocoh1_baseline empl_pseudocoh2_baseline wages wagerate multiplejobs   ///
							employed  new_entrant region puma_id region_puma_id multiest industry yearquarter bad_data{

				gen temp_l`x'0= `x' if time_temp ==0
				by personid: egen `x'_0= max(temp_l`x'0)
				drop temp_l`x'0
				}
			
			*Restrict sample to workers in locatable firms who do not have bad data 
			drop if multiest_0==1
			drop if bad_data_0==1
			drop if region_0==9		
						
			tab region_0
			*Restrict sample to workers with valid wagerates earning <$13 per hour at baseline
			if `c'==12 {
				keep if wagerate_0 < 11
				}
			if `c'==13 {
				keep if wagerate_0 < 13
				}
			sort personid time_temp
			*outcome 1--X_1
			forvalues i=1/6 {
					foreach x in hours hours_seattle empl_pseudocoh1_baseline empl_pseudocoh2_baseline wages wagerate multiplejobs   ///
						employed multiest new_entrant region  puma_id region_puma_id  {

					gen temp_`x'`i'= `x' if time_temp ==`i'
					by personid: egen `x'_`i'= max(temp_`x'`i')
					drop temp_`x'`i'
						}	
					}

			*t-3
			foreach x in hours hours_seattle empl_pseudocoh1_baseline empl_pseudocoh2_baseline wages wagerate multiplejobs   ///
						employed multiest new_entrant region puma_id region_puma_id {
					gen temp_l`x'3= `x' if time_temp ==-3
					by personid: egen `x'_l3= max(temp_l`x'3)
					drop temp_l`x'3
						}
					
			*t-2
			foreach x in hours hours_seattle empl_pseudocoh1_baseline empl_pseudocoh2_baseline wages wagerate multiplejobs  ///
						employed multiest new_entrant region  puma_id region_puma_id {
					gen temp_l`x'2= `x' if time_temp ==-2
					by personid: egen `x'_l2= max(temp_l`x'2)
					drop temp_l`x'2
						}
			*t-1			
			foreach x in hours hours_seattle empl_pseudocoh1_baseline empl_pseudocoh2_baseline wages wagerate multiplejobs   ///
						employed multiest new_entrant region puma_id region_puma_id {
					gen temp_l`x'1= `x' if time_temp ==-1
					by personid: egen `x'_l1= max(temp_l`x'1)
					drop temp_l`x'1
						}				
		
			unab hours: hours*
			unab wages: wages*
			unab wagerate: wagerate*
			unab multiplejobs: multiplejobs*
			unab employed: employed*
			unab new_entrant: new_entrant*
			unab region: region*
			unab puma_id: puma_id*
			unab region_puma_id: region_puma_id* 
			unab multiest: multiest*
			unab empl_coh1: empl_pseudocoh1_baseline*
			unab empl_coh2: empl_pseudocoh2_baseline*
			
			sum dur_acct20`c' dur_age20`c'
collapse (first) treat  dur_acct20`c'  dur_age20`c' `hours' `empl_coh1' `empl_coh2' `wages' `wagerate' `multiplejobs' `employed' `new_entrant' `region' `puma_id' `multiest' yearquarter_0 industry_0  , by(personid)
			foreach x in treat `hours' `wages'  `employed' `new_entrant' {
				replace `x'=0 if `x'==.
				}

		local ematch employed_0 employed_l1 employed_l2 new_entrant_0 new_entrant_l1 new_entrant_l2
		local cont_cov hours_0 hours_l1 hours_l2 wagerate_0 wagerate_l1 wagerate_l2 dur_acct20`c' `c' dur_age20`c'
		local dependent wages_1 wages_2 wages_3 wages_4 wages_5 wages_6 hours_1 hours_2 hours_3 hours_4 hours_5 hours_6
		
		
		qui gen flag_nonmis = 1
		foreach var in `ematch' `cont_cov' `dependent' {
			qui replace flag_nonmis = 0 if `var' == .
			}
			
		tab flag_nonmis
		tab treat if flag_nonmis == 1 
		
		* Create grouping variable
		gen emp_group = 1 if (employed_0 ==1 & employed_l1 ==0 & employed_l2 ==0)
		qui replace emp_group = 2 if (employed_0 ==1 & employed_l1 ==1 & employed_l2 ==0)		
		qui replace emp_group = 3 if (employed_0 ==1 & employed_l1 ==0 & employed_l2 ==1)		
		qui replace emp_group = 4 if (employed_0 ==1 & employed_l1 ==1 & employed_l2 ==1)
		
		gen ent_group = 1 if (new_entrant_0 ==1 & new_entrant_l1 ==0 & new_entrant_l2 ==0)
		qui replace ent_group = 2 if (new_entrant_0 ==0 & new_entrant_l1 ==1 & new_entrant_l2 ==0)
		qui replace ent_group = 3 if (new_entrant_0 ==0 & new_entrant_l1 ==0 & new_entrant_l2 ==1)		
		qui replace ent_group = 4 if ent_group==.
		gen exact_match_group = 10*ent_group + emp_group 
		
		*** Summary statistics on the potential number of matches
		tab exact_match_group treat if flag_nonmis == 1 
		tab emp_group treat if flag_nonmis == 1 
		tab ent_group treat if flag_nonmis == 1 
		
		save "${path_collapsed_data}\\mw20`c'_cohort_`sample_size'.dta", replace
	
		clear
		}
	}
	log close
		
		