capture clear all
capture log close

**** NOTE: THIS IS AN EDITED VERSION OF "DnDnDnD_elasticity_confidenceinterval_AEJEP_R1.do" AND CORRESPONDS TO THE FINAL ACCEPTED VERSION OF THE PAPER.

****************************************************************************
* TABLE 10, PANELS A, B, AND C WAS CONSTRUCTED BY CUTTING-AND-PASTING THE OUTPUT BELOW INTO EXCEL
****************************************************************************



set more off
global path_project "R:\\Project\SeattleMinimumWage\\Stata\\AttenuationPaper\\dofiles\\AEJEP_submission_2020"
global path_data "${path_project}\\data\\data_confidential\\"
local cdt = "`c(current_date)'"
local cdt: subinstr local cdt " " "-", all
log using "${path_project}\\log\\log_confidential\\table_10_panels_a_b_c.log", replace

version 14.2
set seed 123456789

****************************************************************************
* ESTIMATE EFFECTS ON COHORT 1, SEATTLE WORKERS EARNING < $11/HR IN 2015.1
****************************************************************************

* GET DATA ON PSEUDO-COHORT 1 AND APPEND COHORT 1
use treat nn1 matchID empl_pseudocoh1_baseline_0-empl_pseudocoh1_baseline_6 empl_pseudocoh1_baseline_l1 empl_pseudocoh1_baseline_l2 hours_seattle_0-hours_seattle_6 hours_seattle_l1 hours_seattle_l2 wagerate_0-wagerate_6 employed_0-employed_6 hours_0-hours_6 wages_0-wages_6 hours_l1 hours_l2 wagerate_l1 wagerate_l2 dur_acct2012 dur_age2012 using "${path_data}ESDdata_matched_2012sample.dta", clear
gen pseudo=1
forvalues i=0/6 {
	rename empl_pseudocoh1_baseline_`i' empl_coh1_baseline_`i'
}
forvalues i=1/2 {
	rename empl_pseudocoh1_baseline_l`i' empl_coh1_baseline_l`i'
}
append using "${path_data}ESDdata_matched_2015sample.dta", keep(treat nn1 matchID empl_coh1_baseline_0-empl_coh1_baseline_6 empl_coh1_baseline_l1 empl_coh1_baseline_l2 hours_seattle_0-hours_seattle_6 hours_seattle_l1 hours_seattle_l2 wagerate_0-wagerate_6 employed_0-employed_6 hours_0-hours_6 wages_0-wages_6 hours_l1 hours_l2 wagerate_l1 wagerate_l2 dur_acct2015 dur_age2015) 
replace pseudo=0 if pseudo==.
replace wagerate_l1 = 0 if wagerate_l1 == .
replace wagerate_l2 = 0 if wagerate_l2 == .
capture drop tot_hourspre 
gen tot_hourspre=hours_0+hours_l1+hours_l2 

* Clean outcome variable names
qui forvalues i=0/6 {
	rename wages_`i' earnings_`i'
	rename wagerate_`i' wage_`i'
}
rename wagerate_l1 wage_l1 
rename wagerate_l2 wage_l2
forvalues i=0/6 {
	rename empl_coh1_baseline_`i' empl_base_empl_`i'
}
forvalues i=1/2 {
	rename empl_coh1_baseline_l`i' empl_base_empl_l`i'
}

* Winsorize continuous outcomes by assigning anyone below bottom 0.5% the value at 0.5% and by assiging anyone above top 0.5% the value of 0.5%
foreach Y in wage hours hours_seattle earnings {
	qui forvalues i=0/6 {
		_pctile `Y'_`i' if pseudo==0 & treat == 1, p(0.5 99.5)
		replace `Y'_`i' = `r(r1)' if `Y'_`i' < `r(r1)' & pseudo==0 & treat == 1 & `Y'_`i' < .
		replace `Y'_`i' = `r(r2)' if `Y'_`i' > `r(r2)' & pseudo==0 & treat == 1 & `Y'_`i' < .
		_pctile `Y'_`i' if pseudo==0 & treat == 0, p(0.5 99.5)
		replace `Y'_`i' = `r(r1)' if `Y'_`i' < `r(r1)' & pseudo==0 & treat == 0 & `Y'_`i' < .
		replace `Y'_`i' = `r(r2)' if `Y'_`i' > `r(r2)' & pseudo==0 & treat == 0 & `Y'_`i' < .
		_pctile `Y'_`i' if pseudo==1 & treat == 1, p(0.5 99.5)
		replace `Y'_`i' = `r(r1)' if `Y'_`i' < `r(r1)' & pseudo==1 & treat == 1 & `Y'_`i' < .
		replace `Y'_`i' = `r(r2)' if `Y'_`i' > `r(r2)' & pseudo==1 & treat == 1 & `Y'_`i' < .
		_pctile `Y'_`i' if pseudo==1 & treat == 0, p(0.5 99.5)
		replace `Y'_`i' = `r(r1)' if `Y'_`i' < `r(r1)' & pseudo==1 & treat == 0 & `Y'_`i' < .
		replace `Y'_`i' = `r(r2)' if `Y'_`i' > `r(r2)' & pseudo==1 & treat == 0 & `Y'_`i' < .
	}
}

gen cluster=nn1 if pseudo==0 & treat==1
replace cluster=matchID if pseudo==0 & treat==0
replace cluster=1000000+nn1 if pseudo==1 & treat==1
replace cluster=1000000+matchID if pseudo==1 & treat==0

sum tot_hourspre if pseudo==0 & treat==1, d
local p50=r(p50)
local p25=r(p25)
local p75=r(p75)
gen lowexp=tot_hourspre<r(p50) if pseudo==0 & treat==1
* set the same hours threshold for pseudo cohort 
replace lowexp=tot_hourspre<`p50' if pseudo==1 & treat==1
* keep treated observations with their paired comparison in the same low/high experience group
egen temp=max(lowexp), by(cluster)
replace lowexp=temp if lowexp==.
drop temp

* ANALYSIS: COMPUTE DIFFERENCE-IN-DIFFERENCES-IN-DIFFERENCES
forvalues i=0/6 {
	gen hours_out_`i'=hours_`i'-hours_seattle_`i'
}
forvalues i=1/2 {
	gen hours_out_l`i'=hours_l`i'-hours_seattle_l`i'
}
foreach Y in wage hours {
	* Create bias-corrected versions of outcomes (ala Abadie and Imbens, 2011, JBES)
	qui forvalues i=0/6 {
		regress `Y'_`i' hours_0 hours_l1 hours_l2 wage_0 wage_l1 wage_l2 dur_acct2015 dur_age2015 if pseudo==0 & treat==0
		predict temp if pseudo==0
		sum temp if pseudo==0 & treat==1 
		local temp=r(mean)
		sum temp if pseudo==0 & treat==0
		gen `Y'_`i'_bc=`Y'_`i'+`temp'-r(mean) if pseudo==0 & treat==1 
		drop temp
		regress `Y'_`i' hours_0 hours_l1 hours_l2 wage_0 wage_l1 wage_l2 dur_acct2015 dur_age2015 if pseudo==0 & treat==1
		predict temp if pseudo==0
		sum temp if pseudo==0 & treat==0 
		local temp=r(mean)
		sum temp if pseudo==0 & treat==1
		replace `Y'_`i'_bc=`Y'_`i'+`temp'-r(mean) if pseudo==0 & treat==0
		drop temp
		regress `Y'_`i' hours_0 hours_l1 hours_l2 wage_0 wage_l1 wage_l2 dur_acct2012 dur_age2012 if pseudo==1 & treat==0
		predict temp if pseudo==1
		sum temp if pseudo==1 & treat==1 
		local temp=r(mean)
		sum temp if pseudo==1 & treat==0
		replace `Y'_`i'_bc=`Y'_`i'+`temp'-r(mean) if pseudo==1 & treat==1 
		drop temp
		regress `Y'_`i' hours_0 hours_l1 hours_l2 wage_0 wage_l1 wage_l2 dur_acct2012 dur_age2012 if pseudo==1 & treat==1
		predict temp if pseudo==1
		sum temp if pseudo==1 & treat==0 
		local temp=r(mean)
		sum temp if pseudo==1 & treat==1
		replace `Y'_`i'_bc=`Y'_`i'+`temp'-r(mean) if pseudo==1 & treat==0
		drop temp
		******************* REPEAT FOR LOW EXPERIENCE
		regress `Y'_`i' hours_0 hours_l1 hours_l2 wage_0 wage_l1 wage_l2 dur_acct2015 dur_age2015 if pseudo==0 & treat==0 & lowexp==1
		predict temp if pseudo==0
		sum temp if pseudo==0 & treat==1 & lowexp==1 
		local temp=r(mean)
		sum temp if pseudo==0 & treat==0 & lowexp==1
		gen `Y'_`i'_bc_low=`Y'_`i'+`temp'-r(mean) if pseudo==0 & treat==1 & lowexp==1
		drop temp
		regress `Y'_`i' hours_0 hours_l1 hours_l2 wage_0 wage_l1 wage_l2 dur_acct2015 dur_age2015 if pseudo==0 & treat==1 & lowexp==1
		predict temp if pseudo==0
		sum temp if pseudo==0 & treat==0 & lowexp==1 
		local temp=r(mean)
		sum temp if pseudo==0 & treat==1 & lowexp==1
		replace `Y'_`i'_bc_low=`Y'_`i'+`temp'-r(mean) if pseudo==0 & treat==0 & lowexp==1
		drop temp
		regress `Y'_`i' hours_0 hours_l1 hours_l2 wage_0 wage_l1 wage_l2 dur_acct2012 dur_age2012 if pseudo==1 & treat==0 & lowexp==1
		predict temp if pseudo==1
		sum temp if pseudo==1 & treat==1 & lowexp==1 
		local temp=r(mean)
		sum temp if pseudo==1 & treat==0 & lowexp==1
		replace `Y'_`i'_bc_low=`Y'_`i'+`temp'-r(mean) if pseudo==1 & treat==1 & lowexp==1 
		drop temp
		regress `Y'_`i' hours_0 hours_l1 hours_l2 wage_0 wage_l1 wage_l2 dur_acct2012 dur_age2012 if pseudo==1 & treat==1 & lowexp==1
		predict temp if pseudo==1
		sum temp if pseudo==1 & treat==0 & lowexp==1 
		local temp=r(mean)
		sum temp if pseudo==1 & treat==1 & lowexp==1
		replace `Y'_`i'_bc_low=`Y'_`i'+`temp'-r(mean) if pseudo==1 & treat==0 & lowexp==1
		drop temp		
		******************* REPEAT FOR HIGH EXPERIENCE
		regress `Y'_`i' hours_0 hours_l1 hours_l2 wage_0 wage_l1 wage_l2 dur_acct2015 dur_age2015 if pseudo==0 & treat==0 & lowexp==0
		predict temp if pseudo==0
		sum temp if pseudo==0 & treat==1 & lowexp==0 
		local temp=r(mean)
		sum temp if pseudo==0 & treat==0 & lowexp==0
		gen `Y'_`i'_bc_high=`Y'_`i'+`temp'-r(mean) if pseudo==0 & treat==1 & lowexp==0
		drop temp
		regress `Y'_`i' hours_0 hours_l1 hours_l2 wage_0 wage_l1 wage_l2 dur_acct2015 dur_age2015 if pseudo==0 & treat==1 & lowexp==0
		predict temp if pseudo==0
		sum temp if pseudo==0 & treat==0 & lowexp==0 
		local temp=r(mean)
		sum temp if pseudo==0 & treat==1 & lowexp==0
		replace `Y'_`i'_bc_high=`Y'_`i'+`temp'-r(mean) if pseudo==0 & treat==0 & lowexp==0
		drop temp
		regress `Y'_`i' hours_0 hours_l1 hours_l2 wage_0 wage_l1 wage_l2 dur_acct2012 dur_age2012 if pseudo==1 & treat==0 & lowexp==0
		predict temp if pseudo==1
		sum temp if pseudo==1 & treat==1 & lowexp==0 
		local temp=r(mean)
		sum temp if pseudo==1 & treat==0 & lowexp==0
		replace `Y'_`i'_bc_high=`Y'_`i'+`temp'-r(mean) if pseudo==1 & treat==1 & lowexp==0 
		drop temp
		regress `Y'_`i' hours_0 hours_l1 hours_l2 wage_0 wage_l1 wage_l2 dur_acct2012 dur_age2012 if pseudo==1 & treat==1 & lowexp==0
		predict temp if pseudo==1
		sum temp if pseudo==1 & treat==0 & lowexp==0 
		local temp=r(mean)
		sum temp if pseudo==1 & treat==1 & lowexp==0
		replace `Y'_`i'_bc_high=`Y'_`i'+`temp'-r(mean) if pseudo==1 & treat==0 & lowexp==0
		drop temp
	}
}

* Create temporary dataset to hold DnD estimates
preserve
clear
set obs 10000
qui forvalues i=1/6 {
	gen DnD_wage_`i'=.
	gen DnD_wage_pseudo`i'=.
	gen DnD_wage_LOW`i'=.
	gen DnD_wage_LOW_pseudo`i'=.
	gen DnD_wage_HIGH`i'=.
	gen DnD_wage_HIGH_pseudo`i'=.
	gen DnD_hours_`i'=.
	gen DnD_hours_pseudo`i'=.
	gen DnD_hours_LOW`i'=.
	gen DnD_hours_LOW_pseudo`i'=.
	gen DnD_hours_HIGH`i'=.
	gen DnD_hours_HIGH_pseudo`i'=.
}
save temp.dta, replace
clear
restore
qui forvalues bootstrap=1/10000 {
	preserve
	bsample, cluster(cluster) strata(pseudo)  
	sum wage_0_bc if pseudo==0 & treat==1
	local temp=r(mean)
	sum wage_0_bc if pseudo==0 & treat==0
	local diff0=`temp'-r(mean)
	forvalues i=1/6 {
		sum wage_`i'_bc if pseudo==0 & treat==1
		local temp=r(mean)
		sum wage_`i'_bc if pseudo==0 & treat==0
		local DnD_wage_`i'=`temp'-r(mean)-`diff0'		
	}
	sum wage_0_bc if pseudo==1 & treat==1
	local temp=r(mean)
	sum wage_0_bc if pseudo==1 & treat==0
	local diff0=`temp'-r(mean)
	forvalues i=1/6 {
		sum wage_`i'_bc if pseudo==1 & treat==1
		local temp=r(mean)
		sum wage_`i'_bc if pseudo==1 & treat==0
		local DnD_wage_pseudo`i'=`temp'-r(mean)-`diff0'		
	}
	sum hours_0_bc if pseudo==0 & treat==1
	local temp=r(mean)
	sum hours_0_bc if pseudo==0 & treat==0
	local diff0=`temp'-r(mean)
	forvalues i=1/6 {
		sum hours_`i'_bc if pseudo==0 & treat==1
		local temp=r(mean)
		sum hours_`i'_bc if pseudo==0 & treat==0
		local DnD_hours_`i'=`temp'-r(mean)-`diff0'		
	}
	sum hours_0_bc if pseudo==1 & treat==1
	local temp=r(mean)
	sum hours_0_bc if pseudo==1 & treat==0
	local diff0=`temp'-r(mean)
	forvalues i=1/6 {
		sum hours_`i'_bc if pseudo==1 & treat==1
		local temp=r(mean)
		sum hours_`i'_bc if pseudo==1 & treat==0
		local DnD_hours_pseudo`i'=`temp'-r(mean)-`diff0'		
	}
	clear
	use temp.dta
	forvalues i=1/6 {
		replace DnD_wage_`i'=`DnD_wage_`i'' if _n==`bootstrap'
		replace DnD_wage_pseudo`i'=`DnD_wage_pseudo`i'' if _n==`bootstrap'
		replace DnD_hours_`i'=`DnD_hours_`i'' if _n==`bootstrap'
		replace DnD_hours_pseudo`i'=`DnD_hours_pseudo`i'' if _n==`bootstrap'
	}	
	save temp.dta, replace
	clear
	restore
	******************* REPEAT BOOTSTRAP SAMPLE BY LOW/HIGH EXPERIENCE
	preserve
	bsample, cluster(cluster) strata(pseudo lowexp)  

	*** LOW EXPERIENCE
	sum wage_0_bc_low if pseudo==0 & treat==1 & lowexp==1
	local temp=r(mean)
	sum wage_0_bc_low if pseudo==0 & treat==0 & lowexp==1
	local diff0=`temp'-r(mean)
	forvalues i=1/6 {
		sum wage_`i'_bc_low if pseudo==0 & treat==1 & lowexp==1
		local temp=r(mean)
		sum wage_`i'_bc_low if pseudo==0 & treat==0 & lowexp==1
		local DnD_wage_LOW`i'=`temp'-r(mean)-`diff0'		
	}
	sum wage_0_bc_low if pseudo==1 & treat==1 & lowexp==1
	local temp=r(mean)
	sum wage_0_bc_low if pseudo==1 & treat==0 & lowexp==1
	local diff0=`temp'-r(mean)
	forvalues i=1/6 {
		sum wage_`i'_bc_low if pseudo==1 & treat==1 & lowexp==1
		local temp=r(mean)
		sum wage_`i'_bc_low if pseudo==1 & treat==0 & lowexp==1
		local DnD_wage_LOW_pseudo`i'=`temp'-r(mean)-`diff0'		
	}
	sum hours_0_bc_low if pseudo==0 & treat==1 & lowexp==1
	local temp=r(mean)
	sum hours_0_bc_low if pseudo==0 & treat==0 & lowexp==1
	local diff0=`temp'-r(mean)
	forvalues i=1/6 {
		sum hours_`i'_bc_low if pseudo==0 & treat==1 & lowexp==1
		local temp=r(mean)
		sum hours_`i'_bc_low if pseudo==0 & treat==0 & lowexp==1
		local DnD_hours_LOW`i'=`temp'-r(mean)-`diff0'		
	}
	sum hours_0_bc_low if pseudo==1 & treat==1 & lowexp==1
	local temp=r(mean)
	sum hours_0_bc_low if pseudo==1 & treat==0 & lowexp==1
	local diff0=`temp'-r(mean)
	forvalues i=1/6 {
		sum hours_`i'_bc_low if pseudo==1 & treat==1 & lowexp==1
		local temp=r(mean)
		sum hours_`i'_bc_low if pseudo==1 & treat==0 & lowexp==1
		local DnD_hours_LOW_pseudo`i'=`temp'-r(mean)-`diff0'		
	}

	*** HIGH EXPERIENCE
	sum wage_0_bc_high if pseudo==0 & treat==1 & lowexp==0
	local temp=r(mean)
	sum wage_0_bc_high if pseudo==0 & treat==0 & lowexp==0
	local diff0=`temp'-r(mean)
	forvalues i=1/6 {
		sum wage_`i'_bc_high if pseudo==0 & treat==1 & lowexp==0
		local temp=r(mean)
		sum wage_`i'_bc_high if pseudo==0 & treat==0 & lowexp==0
		local DnD_wage_HIGH`i'=`temp'-r(mean)-`diff0'		
	}
	sum wage_0_bc_high if pseudo==1 & treat==1 & lowexp==0
	local temp=r(mean)
	sum wage_0_bc_high if pseudo==1 & treat==0 & lowexp==0
	local diff0=`temp'-r(mean)
	forvalues i=1/6 {
		sum wage_`i'_bc_high if pseudo==1 & treat==1 & lowexp==0
		local temp=r(mean)
		sum wage_`i'_bc_high if pseudo==1 & treat==0 & lowexp==0
		local DnD_wage_HIGH_pseudo`i'=`temp'-r(mean)-`diff0'		
	}
	sum hours_0_bc_high if pseudo==0 & treat==1 & lowexp==0
	local temp=r(mean)
	sum hours_0_bc_high if pseudo==0 & treat==0 & lowexp==0
	local diff0=`temp'-r(mean)
	forvalues i=1/6 {
		sum hours_`i'_bc_high if pseudo==0 & treat==1 & lowexp==0
		local temp=r(mean)
		sum hours_`i'_bc_high if pseudo==0 & treat==0 & lowexp==0
		local DnD_hours_HIGH`i'=`temp'-r(mean)-`diff0'		
	}
	sum hours_0_bc_high if pseudo==1 & treat==1 & lowexp==0
	local temp=r(mean)
	sum hours_0_bc_high if pseudo==1 & treat==0 & lowexp==0
	local diff0=`temp'-r(mean)
	forvalues i=1/6 {
		sum hours_`i'_bc_high if pseudo==1 & treat==1 & lowexp==0
		local temp=r(mean)
		sum hours_`i'_bc_high if pseudo==1 & treat==0 & lowexp==0
		local DnD_hours_HIGH_pseudo`i'=`temp'-r(mean)-`diff0'		
	}
	clear
	use temp.dta
	forvalues i=1/6 {
		replace DnD_wage_LOW`i'=`DnD_wage_LOW`i'' if _n==`bootstrap'
		replace DnD_wage_LOW_pseudo`i'=`DnD_wage_LOW_pseudo`i'' if _n==`bootstrap'
		replace DnD_wage_HIGH`i'=`DnD_wage_HIGH`i'' if _n==`bootstrap'
		replace DnD_wage_HIGH_pseudo`i'=`DnD_wage_HIGH_pseudo`i'' if _n==`bootstrap'
		replace DnD_hours_LOW`i'=`DnD_hours_LOW`i'' if _n==`bootstrap'
		replace DnD_hours_LOW_pseudo`i'=`DnD_hours_LOW_pseudo`i'' if _n==`bootstrap'
		replace DnD_hours_HIGH`i'=`DnD_hours_HIGH`i'' if _n==`bootstrap'
		replace DnD_hours_HIGH_pseudo`i'=`DnD_hours_HIGH_pseudo`i'' if _n==`bootstrap'
	}	
	save temp.dta, replace
	clear
	restore
	if `bootstrap'==1 | `bootstrap'==25 | `bootstrap'-int(`bootstrap'/100)*100==0 {
		noisily display "Completed bootstrapped sample `bootstrap' of 10000 at $S_TIME, $S_DATE"
	}
}
clear
use temp.dta
qui forvalues i=1/6 {
	gen DnDnD_wage_`i'=DnD_wage_`i'-DnD_wage_pseudo`i'
	gen DnDnD_hours_`i'=DnD_hours_`i'-DnD_hours_pseudo`i'
	* Note: base mean hours and wages taken from "Treated (Seattle Workers), Mean" for 2015.1 for overall sample
	gen DnDnD_elasticity_`i'=(DnDnD_hours_`i'/239.366027832031)/(DnDnD_wage_`i'/10.0639696121215)
	noisily display "Mean Elasticity for Bootstrapped Samples"
	noisily sum DnDnD_elasticity_`i', d
	sort DnDnD_elasticity_`i'
	local pctile025=DnDnD_elasticity_`i'[250]
	local pctile975=DnDnD_elasticity_`i'[9750]
	noisily display "95% CI for Elasticity for Quarter `i' is `pctile025' to `pctile975'"
	if `i'<=3 {
		gen DnDnD_statutory_elasticity_`i'=(DnDnD_hours_`i'/239.366027832031)/((11-9.47)/9.47)
	}
	else {
		gen DnDnD_statutory_elasticity_`i'=(DnDnD_hours_`i'/239.366027832031)/((13-9.47)/9.47)
	}
	noisily display "Mean Staturory Elasticity for Bootstrapped Samples"
	noisily sum DnDnD_statutory_elasticity_`i', d
	sort DnDnD_statutory_elasticity_`i'
	local pctile025=DnDnD_statutory_elasticity_`i'[250]
	local pctile975=DnDnD_statutory_elasticity_`i'[9750]
	noisily display "95% CI for Statutory Elasticity for Quarter `i' is `pctile025' to `pctile975'"

	******************* REPEAT FOR LOW EXPERIENCE
	gen DnDnD_wage_LOW`i'=DnD_wage_LOW`i'-DnD_wage_LOW_pseudo`i'
	gen DnDnD_hours_LOW`i'=DnD_hours_LOW`i'-DnD_hours_LOW_pseudo`i'
	* Note: base mean hours and wages taken from "Treated (Seattle Workers), Mean" for 2015.1 for overall sample
	gen DnDnD_elasticity_LOW`i'=(DnDnD_hours_LOW`i'/108.6379)/(DnDnD_wage_LOW`i'/9.998683)
	* Mean Elasticity for Bootstrapped Samples
	noisily display "Mean Elasticity for Bootstrapped Samples With Low Experience"
	noisily sum DnDnD_elasticity_LOW`i', d
	sort DnDnD_elasticity_LOW`i'
	local pctile025=DnDnD_elasticity_LOW`i'[250]
	local pctile975=DnDnD_elasticity_LOW`i'[9750]
	noisily display "95% CI for Elasticity for Quarter `i' for workers with low experience is `pctile025' to `pctile975'"
	if `i'<=3 {
		gen DnDnD_statutory_elasticity_LOW`i'=(DnDnD_hours_LOW`i'/108.6379)/((11-9.47)/9.47)
	}
	else {
		gen DnDnD_statutory_elasticity_LOW`i'=(DnDnD_hours_LOW`i'/108.6379)/((13-9.47)/9.47)
	}
	noisily display "Mean Staturory Elasticity for Bootstrapped Samples with low experience"
	noisily sum DnDnD_statutory_elasticity_LOW`i', d
	sort DnDnD_statutory_elasticity_LOW`i'
	local pctile025=DnDnD_statutory_elasticity_LOW`i'[250]
	local pctile975=DnDnD_statutory_elasticity_LOW`i'[9750]
	noisily display "95% CI for Statutory Elasticity for Quarter `i' for workers with low experience is `pctile025' to `pctile975'"

	******************* REPEAT FOR HIGH EXPERIENCE
	gen DnDnD_wage_HIGH`i'=DnD_wage_HIGH`i'-DnD_wage_HIGH_pseudo`i'
	gen DnDnD_hours_HIGH`i'=DnD_hours_HIGH`i'-DnD_hours_HIGH_pseudo`i'
	* Note: base mean hours and wages taken from "Treated (Seattle Workers), Mean" for 2015.1 for overall sample
	gen DnDnD_elasticity_HIGH`i'=(DnDnD_hours_HIGH`i'/367.0558)/(DnDnD_wage_HIGH`i'/10.12976)
	noisily display "Mean Elasticity for Bootstrapped Samples With High Experience"
	noisily sum DnDnD_elasticity_HIGH`i', d
	sort DnDnD_elasticity_HIGH`i'
	local pctile025=DnDnD_elasticity_HIGH`i'[250]
	local pctile975=DnDnD_elasticity_HIGH`i'[9750]
	noisily display "95% CI for Elasticity for Quarter `i' for workers with high experience is `pctile025' to `pctile975'"
	if `i'<=3 {
		gen DnDnD_statutory_elasticity_HIGH`i'=(DnDnD_hours_HIGH`i'/367.0558)/((11-9.47)/9.47)
	}
	else {
		gen DnDnD_statutory_elasticity_HIGH`i'=(DnDnD_hours_HIGH`i'/367.0558)/((13-9.47)/9.47)
	}
	noisily display "Mean Staturory Elasticity for Bootstrapped Samples with high experience"
	noisily sum DnDnD_statutory_elasticity_HIGH`i', d
	sort DnDnD_statutory_elasticity_HIGH`i'
	local pctile025=DnDnD_statutory_elasticity_HIGH`i'[250]
	local pctile975=DnDnD_statutory_elasticity_HIGH`i'[9750]
	noisily display "95% CI for Statutory Elasticity for Quarter `i' for workers with high experience is `pctile025' to `pctile975'"
}	
clear
erase temp.dta
capture log close
