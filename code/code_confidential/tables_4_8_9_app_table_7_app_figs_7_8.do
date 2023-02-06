capture clear all
capture log close

**** NOTE: THIS IS AN EDITED VERSION OF "DnDnDnD_AEJEP_R2.do" AND CORRESPONDS TO THE FINAL ACCEPTED VERSION OF THE PAPER.


set more off
global path_project "R:\\Project\SeattleMinimumWage\\Stata\\AttenuationPaper\\dofiles\\AEJEP_submission_2020"
global path_data "${path_project}\\data\\data_confidential\\"
global path_output "${path_project}\\output\\output_confidential\\"
local cdt = "`c(current_date)'"
local cdt: subinstr local cdt " " "-", all
log using "${path_project}\\log\\log_confidential\\tables_4_8_9_app_table_7_app_figs_7_8.log", replace

version 14.2
set seed 123456789


****************************************************************************
* TABLE 4 WAS CONSTRUCTED BY CUTTING-AND-PASTING THE OUTPUT BELOW INTO EXCEL
****************************************************************************

****************************************************************************
* INVESTIGATE BALANCE BETWEEN SEATTLE AND POOL OF CONTROL WORKERS IN OUTLYING WA FOR COHORT 1, WORKERS EARNING < $11/HR IN 2015.1
****************************************************************************
use treat employed_0 employed_l1 employed_l2 new_entrant_0 new_entrant_l1 new_entrant_l2 dur_acct2015 dur_age2015 hours_0 hours_l1 hours_l2 wagerate_0 wagerate_l1 wagerate_l2 multiplejobs_0 multiplejobs_l1 multiplejobs_l2 using "${path_data}mw2015_cohort_100.dta", clear
replace wagerate_l1=. if employed_l1==0 
replace wagerate_l2=. if employed_l2==0 
replace multiplejobs_l1=0 if multiplejobs_l1==.
replace multiplejobs_l2=0 if multiplejobs_l2==.
tabstat employed_0 employed_l1 employed_l2 new_entrant_0 new_entrant_l1 new_entrant_l2 dur_acct2015 dur_age2015 hours_0 hours_l1 hours_l2 wagerate_0 wagerate_l1 wagerate_l2 multiplejobs_0 multiplejobs_l1 multiplejobs_l2, by(treat) statistics(mean sd) columns(statistics) longstub
count if treat==1
count if treat==0
* Normalized difference for all variables except employed_0 
quietly foreach Y in employed_l1 employed_l2 new_entrant_0 new_entrant_l1 new_entrant_l2 dur_acct2015 dur_age2015 hours_0 hours_l1 hours_l2 wagerate_0 wagerate_l1 wagerate_l2 multiplejobs_0 multiplejobs_l1 multiplejobs_l2 {
	sum `Y'
	replace `Y'=(`Y'-r(mean))/r(sd)	
	regress `Y' treat
	noisily display "Normalized differences = " _b[treat] " for `Y'"
}
clear

****************************************************************************
* INVESTIGATE BALANCE BETWEEN SEATTLE AND MATCHED CONTROL WORKERS IN OUTLYING WA FOR COHORT 1, WORKERS EARNING < $11/HR IN 2015.1
****************************************************************************
use treat employed_0 employed_l1 employed_l2 new_entrant_0 new_entrant_l1 new_entrant_l2 dur_acct2015 dur_age2015 hours_0 hours_l1 hours_l2 wagerate_0 wagerate_l1 wagerate_l2 multiplejobs_0 multiplejobs_l1 multiplejobs_l2 using "${path_data}ESDdata_matched_2015sample.dta", clear
replace wagerate_l1=. if employed_l1==0 
replace wagerate_l2=. if employed_l2==0 
tabstat employed_0 employed_l1 employed_l2 new_entrant_0 new_entrant_l1 new_entrant_l2 dur_acct2015 dur_age2015 hours_0 hours_l1 hours_l2 wagerate_0 wagerate_l1 wagerate_l2 multiplejobs_0 multiplejobs_l1 multiplejobs_l2, by(treat) statistics(mean sd) columns(statistics) longstub
count if treat==1
count if treat==0
* Normalized difference for variables that are not matched exactly
quietly foreach Y in dur_acct2015 dur_age2015 hours_0 hours_l1 hours_l2 wagerate_0 wagerate_l1 wagerate_l2 multiplejobs_0 multiplejobs_l1 multiplejobs_l2 {
	sum `Y'
	replace `Y'=(`Y'-r(mean))/r(sd)	
	regress `Y' treat
	noisily display "Normalized differences = " _b[treat] " for `Y'"
}
clear

****************************************************************************
* PREP DATA FOR ANALYSIS OF COHORT 1
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
	tabstat `Y'_0-`Y'_6 if pseudo==0, by(treat) stats(mean sd min p1 p5 p25 p50 p75 p95 p99 max)
	tabstat `Y'_0-`Y'_6 if pseudo==1, by(treat) stats(mean sd min p1 p5 p25 p50 p75 p95 p99 max)
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
	tabstat `Y'_0-`Y'_6 if pseudo==0, by(treat) stats(mean sd min p1 p99 max)
	tabstat `Y'_0-`Y'_6 if pseudo==1, by(treat) stats(mean sd min p1 p99 max)
}


****************************************************************************
* APPENIDIX FIGURE 7
****************************************************************************

* Graph showing growth in wages for pseudo cohort 1 treatment and control group, by percentiles in wage distibution
graph set window fontface "HelveticaNeueforSAS"
graph bar (p1) wage_3 (p5) wage_3 (p25) wage_3 (p50) wage_3 (p75) wage_3 (p95) wage_3 (p99) wage_3 if pseudo==1, over(treat) bargap(10) ytitle("Wage (Conditional on Employment)" " ") ylabel(10 "$10" 20 "$20" 30 "$30" 40 "$40", angle(horizontal)) scheme(s1color) legend(col(1))
gr_edit plotregion1.GraphEdit, cmd(_set_rotate)
gr_edit legend.plotregion1.label[1].text = {}
gr_edit legend.plotregion1.label[1].text.Arrpush Matched Controls
gr_edit legend.plotregion1.label[2].text = {}
gr_edit legend.plotregion1.label[2].text.Arrpush Seattle
gr_edit plotregion1.bars[11].style.editstyle shadestyle(color(gs12)) editcopy
gr_edit plotregion1.bars[2].style.editstyle shadestyle(color(black)) editcopy
gr_edit plotregion1.bars[2].style.editstyle linestyle(color(black)) editcopy
gr_edit grpaxis.edit_tick 1 6.53766 `""1st" "percentile""', custom tickset(major) editstyle(tickstyle(textstyle(size(small))) )
gr_edit grpaxis.edit_tick 2 21.0251 `""5th" "percentile""', custom tickset(major) editstyle(tickstyle(textstyle(size(small))) )
gr_edit grpaxis.edit_tick 3 35.5126 `""25th" "percentile""', custom tickset(major) editstyle(tickstyle(textstyle(size(small))) )
gr_edit grpaxis.edit_tick 4 50 `"Median"', custom tickset(major) editstyle(tickstyle(textstyle(size(small))) )
gr_edit grpaxis.edit_tick 5 64.4874 `""75th" "percentile""', custom tickset(major) editstyle(tickstyle(textstyle(size(small))) )
gr_edit grpaxis.edit_tick 6 78.9749 `""95th" "percentile""', custom tickset(major) editstyle(tickstyle(textstyle(size(small))) )
gr_edit grpaxis.edit_tick 7 93.4623 `""99th" "percentile""', custom tickset(major) editstyle(tickstyle(textstyle(size(small))) )
graph export "${path_output}app_fig7.png", replace
graph export "${path_output}app_fig7.tif", replace
graph set window fontface default



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
sum tot_hourspre if pseudo==0, d
sum tot_hourspre if lowexp==0 & pseudo==0,d
sum tot_hourspre if lowexp==1 & pseudo==0,d
sum tot_hourspre if pseudo==1, d
sum tot_hourspre if lowexp==0 & pseudo==1,d
sum tot_hourspre if lowexp==1 & pseudo==1,d


****************************************************************************
* APPENIDIX FIGURE 8
****************************************************************************

* DISTRIBUTION OF HOURS WORKED IN BASELINE AND PRIOR TWO QUARTERS
gen tot_hourspre_censored=min(tot_hourspre, 2500)
graph set window fontface "HelveticaNeueforSAS"
histogram tot_hourspre_censored if pseudo==0, width(100) xline(`p25' `p50' `p75') xtitle(" " "Hours Worked in Baseline and Prior Two Quarters" " ") ytitle("Number of Workers" "(Seattle and Matched)" " ") freq scheme(s1color) note(" " "Note: Vertical lines at 25th, 50th, and 75th percentiles.  Hours censored at 2,500.") fcolor(gs12) lcolor(black) xlabel(,format(%9.0gc)) ylabel(,format(%9.0gc))
graph export "${path_output}app_fig8.tif", replace
graph export "${path_output}app_fig8.png", replace
graph set window fontface default
drop tot_hourspre_censored


****************************************************************************
* TABLE 8 AND APPENIDIX TABLE 7
****************************************************************************

* ANALYSIS: COMPUTE DIFFERENCE-IN-DIFFERENCES-IN-DIFFERENCES
forvalues i=0/6 {
	gen hours_out_`i'=hours_`i'-hours_seattle_`i'
}
forvalues i=1/2 {
	gen hours_out_l`i'=hours_l`i'-hours_seattle_l`i'
}
foreach Y in wage employed hours earnings hours_out empl_base_empl {
	local tableN=8
	if "`Y'"=="hours_out" | "`Y'"=="empl_base_empl" {
		local tableN=9
	}
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
	* Matrix to store results that go into output tables
	matrix A = J(19,7,.)
	matrix A[1,1]=2015.1
	matrix A[1,2]=2015.2
	matrix A[1,3]=2015.3
	matrix A[1,4]=2015.4
	matrix A[1,5]=2016.1
	matrix A[1,6]=2016.2
	matrix A[1,7]=2016.3
	matrix A[9,1]=2012.1
	matrix A[9,2]=2012.2
	matrix A[9,3]=2012.3
	matrix A[9,4]=2012.4
	matrix A[9,5]=2013.1
	matrix A[9,6]=2013.2
	matrix A[9,7]=2013.3
	matrix A_LOW=A
	matrix A_HIGH=A
	matrix B = J(12,7,.)
	matrix B[1,1]=2015.1
	matrix B[1,2]=2015.2
	matrix B[1,3]=2015.3
	matrix B[1,4]=2015.4
	matrix B[1,5]=2016.1
	matrix B[1,6]=2016.2
	matrix B[1,7]=2016.3
	
	* Fill the matrix
	qui forvalues i=0/6 {
		local j=`i'+1
		sum `Y'_`i' if pseudo==0 & treat==1
		matrix A[2,`j']=r(mean)
		sum `Y'_`i' if pseudo==0 & treat==0
		matrix A[3,`j']=r(mean)
		sum `Y'_`i'_bc if pseudo==0 & treat==1
		local temp=r(mean)
		sum `Y'_`i'_bc if pseudo==0 & treat==0
		matrix A[4,`j']=`temp'-r(mean)
		sum `Y'_`i' if pseudo==1 & treat==1
		matrix A[10,`j']=r(mean)
		sum `Y'_`i' if pseudo==1 & treat==0
		matrix A[11,`j']=r(mean)
		sum `Y'_`i'_bc if pseudo==1 & treat==1
		local temp=r(mean)
		sum `Y'_`i'_bc if pseudo==1 & treat==0
		matrix A[12,`j']=`temp'-r(mean)
		******************* REPEAT FOR LOW EXPERIENCE
		sum `Y'_`i' if pseudo==0 & treat==1 & lowexp==1
		matrix A_LOW[2,`j']=r(mean)
		sum `Y'_`i' if pseudo==0 & treat==0 & lowexp==1
		matrix A_LOW[3,`j']=r(mean)
		sum `Y'_`i'_bc_low if pseudo==0 & treat==1 & lowexp==1
		local temp=r(mean)
		sum `Y'_`i'_bc_low if pseudo==0 & treat==0 & lowexp==1
		matrix A_LOW[4,`j']=`temp'-r(mean)
		sum `Y'_`i' if pseudo==1 & treat==1 & lowexp==1
		matrix A_LOW[10,`j']=r(mean)
		sum `Y'_`i' if pseudo==1 & treat==0 & lowexp==1
		matrix A_LOW[11,`j']=r(mean)
		sum `Y'_`i'_bc_low if pseudo==1 & treat==1 & lowexp==1
		local temp=r(mean)
		sum `Y'_`i'_bc_low if pseudo==1 & treat==0 & lowexp==1
		matrix A_LOW[12,`j']=`temp'-r(mean)
		******************* REPEAT FOR HIGH EXPERIENCE
		sum `Y'_`i' if pseudo==0 & treat==1 & lowexp==0
		matrix A_HIGH[2,`j']=r(mean)
		sum `Y'_`i' if pseudo==0 & treat==0 & lowexp==0
		matrix A_HIGH[3,`j']=r(mean)
		sum `Y'_`i'_bc_high if pseudo==0 & treat==1 & lowexp==0
		local temp=r(mean)
		sum `Y'_`i'_bc_high if pseudo==0 & treat==0 & lowexp==0
		matrix A_HIGH[4,`j']=`temp'-r(mean)
		sum `Y'_`i' if pseudo==1 & treat==1 & lowexp==0
		matrix A_HIGH[10,`j']=r(mean)
		sum `Y'_`i' if pseudo==1 & treat==0 & lowexp==0
		matrix A_HIGH[11,`j']=r(mean)
		sum `Y'_`i'_bc_high if pseudo==1 & treat==1 & lowexp==0
		local temp=r(mean)
		sum `Y'_`i'_bc_high if pseudo==1 & treat==0 & lowexp==0
		matrix A_HIGH[12,`j']=`temp'-r(mean)
	}
	qui forvalues j=2/7 {
		matrix A[5,`j']=A[4,`j']-A[4,1]
		matrix A[13,`j']=A[12,`j']-A[12,1]
		******************* REPEAT FOR LOW EXPERIENCE
		matrix A_LOW[5,`j']=A_LOW[4,`j']-A_LOW[4,1]
		matrix A_LOW[13,`j']=A_LOW[12,`j']-A_LOW[12,1]
		******************* REPEAT FOR HIGH EXPERIENCE
		matrix A_HIGH[5,`j']=A_HIGH[4,`j']-A_HIGH[4,1]
		matrix A_HIGH[13,`j']=A_HIGH[12,`j']-A_HIGH[12,1]
	}
	* Create temporary dataset to hold DnD estimates
	preserve
	clear
	set obs 1000
	qui forvalues i=1/6 {
		gen DnD`i'=.
		gen DnD_pseudo`i'=.
		gen DnD_LOW`i'=.
		gen DnD_LOW_pseudo`i'=.
		gen DnD_HIGH`i'=.
		gen DnD_HIGH_pseudo`i'=.
	}
	save temp.dta, replace
	clear
	restore
	qui forvalues bootstrap=1/1000 {
		preserve
		bsample, cluster(cluster) strata(pseudo)  
		sum `Y'_0_bc if pseudo==0 & treat==1
		local temp=r(mean)
		sum `Y'_0_bc if pseudo==0 & treat==0
		local diff0=`temp'-r(mean)
		forvalues i=1/6 {
			sum `Y'_`i'_bc if pseudo==0 & treat==1
			local temp=r(mean)
			sum `Y'_`i'_bc if pseudo==0 & treat==0
			local DnD`i'=`temp'-r(mean)-`diff0'		
		}
		sum `Y'_0_bc if pseudo==1 & treat==1
		local temp=r(mean)
		sum `Y'_0_bc if pseudo==1 & treat==0
		local diff0=`temp'-r(mean)
		forvalues i=1/6 {
			sum `Y'_`i'_bc if pseudo==1 & treat==1
			local temp=r(mean)
			sum `Y'_`i'_bc if pseudo==1 & treat==0
			local DnD_pseudo`i'=`temp'-r(mean)-`diff0'		
		}
		clear
		use temp.dta
		forvalues i=1/6 {
			replace DnD`i'=`DnD`i'' if _n==`bootstrap'
			replace DnD_pseudo`i'=`DnD_pseudo`i'' if _n==`bootstrap'
		}	
		save temp.dta, replace
		clear
		restore
		******************* REPEAT BOOTSTRAP SAMPLE BY LOW/HIGH EXPERIENCE
		preserve
		bsample, cluster(cluster) strata(pseudo lowexp)  
		*** LOW EXPERIENCE
		sum `Y'_0_bc_low if pseudo==0 & treat==1 & lowexp==1
		local temp=r(mean)
		sum `Y'_0_bc_low if pseudo==0 & treat==0 & lowexp==1
		local diff0=`temp'-r(mean)
		forvalues i=1/6 {
			sum `Y'_`i'_bc_low if pseudo==0 & treat==1 & lowexp==1
			local temp=r(mean)
			sum `Y'_`i'_bc_low if pseudo==0 & treat==0 & lowexp==1
			local DnD_LOW`i'=`temp'-r(mean)-`diff0'		
		}
		sum `Y'_0_bc_low if pseudo==1 & treat==1 & lowexp==1
		local temp=r(mean)
		sum `Y'_0_bc_low if pseudo==1 & treat==0 & lowexp==1
		local diff0=`temp'-r(mean)
		forvalues i=1/6 {
			sum `Y'_`i'_bc_low if pseudo==1 & treat==1 & lowexp==1
			local temp=r(mean)
			sum `Y'_`i'_bc_low if pseudo==1 & treat==0 & lowexp==1
			local DnD_LOW_pseudo`i'=`temp'-r(mean)-`diff0'		
		}
		*** HIGH EXPERIENCE
		sum `Y'_0_bc_high if pseudo==0 & treat==1 & lowexp==0
		local temp=r(mean)
		sum `Y'_0_bc_high if pseudo==0 & treat==0 & lowexp==0
		local diff0=`temp'-r(mean)
		forvalues i=1/6 {
			sum `Y'_`i'_bc_high if pseudo==0 & treat==1 & lowexp==0
			local temp=r(mean)
			sum `Y'_`i'_bc_high if pseudo==0 & treat==0 & lowexp==0
			local DnD_HIGH`i'=`temp'-r(mean)-`diff0'		
		}
		sum `Y'_0_bc_high if pseudo==1 & treat==1 & lowexp==0
		local temp=r(mean)
		sum `Y'_0_bc_high if pseudo==1 & treat==0 & lowexp==0
		local diff0=`temp'-r(mean)
		forvalues i=1/6 {
			sum `Y'_`i'_bc_high if pseudo==1 & treat==1 & lowexp==0
			local temp=r(mean)
			sum `Y'_`i'_bc_high if pseudo==1 & treat==0 & lowexp==0
			local DnD_HIGH_pseudo`i'=`temp'-r(mean)-`diff0'		
		}
		clear
		use temp.dta
		forvalues i=1/6 {
			replace DnD_LOW`i'=`DnD_LOW`i'' if _n==`bootstrap'
			replace DnD_LOW_pseudo`i'=`DnD_LOW_pseudo`i'' if _n==`bootstrap'
			replace DnD_HIGH`i'=`DnD_HIGH`i'' if _n==`bootstrap'
			replace DnD_HIGH_pseudo`i'=`DnD_HIGH_pseudo`i'' if _n==`bootstrap'
		}	
		save temp.dta, replace
		clear
		restore
		if `bootstrap'==1 | `bootstrap'==25 | `bootstrap'-int(`bootstrap'/100)*100==0 {
			noisily display "Completed bootstrapped sample `bootstrap' of 1000 at $S_TIME, $S_DATE"
		}
	}
	preserve
	clear
	use temp.dta
	qui forvalues i=1/6 {
		local j=`i'+1
		sum DnD`i'
		matrix A[6,`j']=r(sd)
		sum DnD_pseudo`i'
		matrix A[14,`j']=r(sd)
		matrix A[17,`j']=A[5,`j']-A[13,`j']
		gen DnDnD`i'=DnD`i'-DnD_pseudo`i'
		sum DnDnD`i'
		matrix A[18,`j']=r(sd)
		******************* REPEAT FOR LOW EXPERIENCE
		sum DnD_LOW`i'
		matrix A_LOW[6,`j']=r(sd)
		sum DnD_LOW_pseudo`i'
		matrix A_LOW[14,`j']=r(sd)
		matrix A_LOW[17,`j']=A_LOW[5,`j']-A_LOW[13,`j']
		gen DnDnD_LOW`i'=DnD_LOW`i'-DnD_LOW_pseudo`i'
		sum DnDnD_LOW`i'
		matrix A_LOW[18,`j']=r(sd)
		******************* REPEAT FOR HIGH EXPERIENCE
		sum DnD_HIGH`i'
		matrix A_HIGH[6,`j']=r(sd)
		sum DnD_HIGH_pseudo`i'
		matrix A_HIGH[14,`j']=r(sd)
		matrix A_HIGH[17,`j']=A_HIGH[5,`j']-A_HIGH[13,`j']
		gen DnDnD_HIGH`i'=DnD_HIGH`i'-DnD_HIGH_pseudo`i'
		sum DnDnD_HIGH`i'
		matrix A_HIGH[18,`j']=r(sd)
		******************* DnDnDnD for LOW-HIGH EXPERIENCE
		gen DnDnDnD`i'=DnDnD_LOW`i'-DnDnD_HIGH`i'
		sum DnDnDnD`i'
		matrix B[11,`j']=r(sd)
	}	
	clear
	erase temp.dta
	forvalues j=2/7 {
		matrix A[7,`j']=2*(1-normal(abs(A[5,`j']/A[6,`j'])))
		matrix A[15,`j']=2*(1-normal(abs(A[13,`j']/A[14,`j'])))
		matrix A[19,`j']=2*(1-normal(abs(A[17,`j']/A[18,`j'])))
		******************* REPEAT FOR LOW EXPERIENCE
		matrix A_LOW[7,`j']=2*(1-normal(abs(A_LOW[5,`j']/A_LOW[6,`j'])))
		matrix A_LOW[15,`j']=2*(1-normal(abs(A_LOW[13,`j']/A_LOW[14,`j'])))
		matrix A_LOW[19,`j']=2*(1-normal(abs(A_LOW[17,`j']/A_LOW[18,`j'])))
		******************* REPEAT FOR HIGH EXPERIENCE
		matrix A_HIGH[7,`j']=2*(1-normal(abs(A_HIGH[5,`j']/A_HIGH[6,`j'])))
		matrix A_HIGH[15,`j']=2*(1-normal(abs(A_HIGH[13,`j']/A_HIGH[14,`j'])))
		matrix A_HIGH[19,`j']=2*(1-normal(abs(A_HIGH[17,`j']/A_HIGH[18,`j'])))
		******************* DnDnDnD for LOW-HIGH EXPERIENCE
		matrix B[2,`j']=A_LOW[17,`j']
		matrix B[3,`j']=A_LOW[18,`j']
		matrix B[4,`j']=A_LOW[19,`j']
		matrix B[6,`j']=A_HIGH[17,`j']
		matrix B[7,`j']=A_HIGH[18,`j']
		matrix B[8,`j']=A_HIGH[19,`j']
		matrix B[10,`j']=B[2,`j']-B[6,`j']
		matrix B[12,`j']=2*(1-normal(abs(B[10,`j']/B[11,`j'])))
	}
	if "`Y'"!="hours_out" & "`Y'"!="empl_base_empl" {
		svmat A
		export excel "${path_output}app_table7_`Y'.xlsx", replace
		clear
	}
	svmat B
	export excel "${path_output}table`tableN'_`Y'.xlsx", replace
	restore
}

capture clear
capture log close
