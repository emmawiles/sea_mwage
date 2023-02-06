****************************************************************************
* EACH ROW OF APPENDIX TABLE 8 COMES FROM A SEPARATE EXCEL FILE GENERATED BELOW
****************************************************************************

****************************************************************************
****** Heterogeneity by work experience decile.
****************************************************************************

capture clear all
capture log close
set more off
global path_project "R:\\Project\SeattleMinimumWage\\Stata\\AttenuationPaper\\dofiles\\AEJEP_submission_2020"
global path_data "${path_project}\\data\\data_confidential\\"
global path_output "${path_project}\\output\\output_confidential\\"
log using "${path_project}\\log\\log_confidential\\app_table_8.log", replace

* GET DATA ON PSEUDO-COHORT 1 AND APPEND COHORT 1
use treat nn1 matchID empl_pseudocoh1_baseline_0-empl_pseudocoh1_baseline_6 empl_pseudocoh1_baseline_l1 empl_pseudocoh1_baseline_l2 hours_seattle_0-hours_seattle_6 wagerate_0-wagerate_6 employed_0-employed_6 hours_0-hours_6 wages_0-wages_6 hours_l1 hours_l2 wagerate_l1 wagerate_l2 dur_acct2012 dur_age2012 using "${path_data}ESDdata_matched_2012sample.dta", clear
gen pseudo=1
forvalues i=0/6 {
	rename empl_pseudocoh1_baseline_`i' empl_coh1_baseline_`i'
}
forvalues i=1/2 {
	rename empl_pseudocoh1_baseline_l`i' empl_coh1_baseline_l`i'
}
append using "${path_data}ESDdata_matched_2015sample.dta", keep(treat nn1 matchID empl_coh1_baseline_0-empl_coh1_baseline_6 empl_coh1_baseline_l1 empl_coh1_baseline_l2 hours_seattle_0-hours_seattle_6 wagerate_0-wagerate_6 employed_0-employed_6 hours_0-hours_6 wages_0-wages_6 hours_l1 hours_l2 wagerate_l1 wagerate_l2 dur_acct2015 dur_age2015) 
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

gen cluster=nn1 if pseudo==0 & treat==1
replace cluster=matchID if pseudo==0 & treat==0
replace cluster=1000000+nn1 if pseudo==1 & treat==1
replace cluster=1000000+matchID if pseudo==1 & treat==0

*** NOTE: THIS PART IS DIFFERENT FROM BASE DO PROGRAMS.  HERE, WE RESTRICT SAMPLE TO THE TARGET DECILE, AND RUN THE ANALYSIS FOR THAT DECLILE ALONE
pctile threshold=tot_hourspre if pseudo==0 & treat==1, nq(10)
list threshold if _n<=10
gen decile=1 if treat==1
replace decile=2 if tot_hourspre>=threshold[1] & tot_hourspre<threshold[2] & treat==1
replace decile=3 if tot_hourspre>=threshold[2] & tot_hourspre<threshold[3] & treat==1
replace decile=4 if tot_hourspre>=threshold[3] & tot_hourspre<threshold[4] & treat==1
replace decile=5 if tot_hourspre>=threshold[4] & tot_hourspre<threshold[5] & treat==1
replace decile=6 if tot_hourspre>=threshold[5] & tot_hourspre<threshold[6] & treat==1
replace decile=7 if tot_hourspre>=threshold[6] & tot_hourspre<threshold[7] & treat==1
replace decile=8 if tot_hourspre>=threshold[7] & tot_hourspre<threshold[8] & treat==1
replace decile=9 if tot_hourspre>=threshold[8] & tot_hourspre<threshold[9] & treat==1
replace decile=10 if tot_hourspre>=threshold[9] & tot_hourspre~=. & treat==1
drop threshold
* keep treated observations with their paired comparison in the same decile experience group
egen temp=max(decile), by(cluster)
replace decile=temp if decile==.
drop temp
tab decile
tabstat wage_0 employed_0 hours_0 earnings_0 tot_hourspre if pseudo==0, by(decile)
tabstat wage_0 employed_0 hours_0 earnings_0 tot_hourspre if pseudo==1, by(decile)
forvalues i=0/6 {
	gen hours_out_`i'=hours_`i'-hours_seattle_`i'
}
forvalues D=1/10 {
	* DESCRIPTIVE ANALYSIS
	gen new=hours_l1+hours_l2==0
	sum wage_0 employed_0 hours_0 hours_seattle_0 earnings_0 new if pseudo==0 & decile==`D'
	drop new
	* ANALYSIS: COMPUTE DIFFERENCE-IN-DIFFERENCES-IN-DIFFERENCES
	foreach Y in wage employed hours earnings {
		* Create bias-corrected versions of outcomes (ala Abadie and Imbens, 2011, JBES)
		qui forvalues i=0/6 {
			regress `Y'_`i' hours_0 hours_l1 hours_l2 wage_0 wage_l1 wage_l2 dur_acct2015 dur_age2015 if pseudo==0 & treat==0 & decile==`D'
			predict temp if pseudo==0 & decile==`D'
			sum temp if pseudo==0 & treat==1 & decile==`D'
			local temp=r(mean)
			sum temp if pseudo==0 & treat==0 & decile==`D'
			gen `Y'_`i'_bc=`Y'_`i'+`temp'-r(mean) if pseudo==0 & treat==1 & decile==`D' 
			drop temp
			regress `Y'_`i' hours_0 hours_l1 hours_l2 wage_0 wage_l1 wage_l2 dur_acct2015 dur_age2015 if pseudo==0 & treat==1 & decile==`D'
			predict temp if pseudo==0 & decile==`D'
			sum temp if pseudo==0 & treat==0 & decile==`D' 
			local temp=r(mean)
			sum temp if pseudo==0 & treat==1 & decile==`D'
			replace `Y'_`i'_bc=`Y'_`i'+`temp'-r(mean) if pseudo==0 & treat==0 & decile==`D'
			drop temp
			regress `Y'_`i' hours_0 hours_l1 hours_l2 wage_0 wage_l1 wage_l2 dur_acct2012 dur_age2012 if pseudo==1 & treat==0 & decile==`D'
			predict temp if pseudo==1 & decile==`D'
			sum temp if pseudo==1 & treat==1 & decile==`D' 
			local temp=r(mean)
			sum temp if pseudo==1 & treat==0 & decile==`D'
			replace `Y'_`i'_bc=`Y'_`i'+`temp'-r(mean) if pseudo==1 & treat==1 & decile==`D' 
			drop temp
			regress `Y'_`i' hours_0 hours_l1 hours_l2 wage_0 wage_l1 wage_l2 dur_acct2012 dur_age2012 if pseudo==1 & treat==1 & decile==`D'
			predict temp if pseudo==1 & decile==`D'
			sum temp if pseudo==1 & treat==0 & decile==`D'
			local temp=r(mean)
			sum temp if pseudo==1 & treat==1 & decile==`D'
			replace `Y'_`i'_bc=`Y'_`i'+`temp'-r(mean) if pseudo==1 & treat==0 & decile==`D'
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
		* Fill the matrix
		qui forvalues i=0/6 {
			local j=`i'+1
			sum `Y'_`i' if pseudo==0 & treat==1 & decile==`D'
			matrix A[2,`j']=r(mean)
			sum `Y'_`i' if pseudo==0 & treat==0 & decile==`D'
			matrix A[3,`j']=r(mean)
			sum `Y'_`i'_bc if pseudo==0 & treat==1 & decile==`D'
			local temp=r(mean)
			sum `Y'_`i'_bc if pseudo==0 & treat==0 & decile==`D'
			matrix A[4,`j']=`temp'-r(mean)
			sum `Y'_`i' if pseudo==1 & treat==1 & decile==`D'
			matrix A[10,`j']=r(mean)
			sum `Y'_`i' if pseudo==1 & treat==0 & decile==`D'
			matrix A[11,`j']=r(mean)
			sum `Y'_`i'_bc if pseudo==1 & treat==1 & decile==`D'
			local temp=r(mean)
			sum `Y'_`i'_bc if pseudo==1 & treat==0 & decile==`D'
			matrix A[12,`j']=`temp'-r(mean)
	
		}
		qui forvalues j=2/7 {
			matrix A[5,`j']=A[4,`j']-A[4,1]
			matrix A[13,`j']=A[12,`j']-A[12,1]
		}
		* Create temporary dataset to hold DnD estimates
		preserve
		clear
		set obs 1000
		qui forvalues i=1/6 {
			gen DnD`i'=.
			gen DnD_pseudo`i'=.
		}
		save temp2.dta, replace
		clear
		restore
		qui forvalues bootstrap=1/1000 {
			preserve
			bsample, cluster(cluster) strata(pseudo)  
			sum `Y'_0_bc if pseudo==0 & treat==1 & decile==`D'
			local temp=r(mean)
			sum `Y'_0_bc if pseudo==0 & treat==0 & decile==`D'
			local diff0=`temp'-r(mean)
			forvalues i=1/6 {
				sum `Y'_`i'_bc if pseudo==0 & treat==1 & decile==`D'
				local temp=r(mean)
				sum `Y'_`i'_bc if pseudo==0 & treat==0 & decile==`D'
				local DnD`i'=`temp'-r(mean)-`diff0'		
			}
			sum `Y'_0_bc if pseudo==1 & treat==1 & decile==`D'
			local temp=r(mean)
			sum `Y'_0_bc if pseudo==1 & treat==0 & decile==`D'
			local diff0=`temp'-r(mean)
			forvalues i=1/6 {
				sum `Y'_`i'_bc if pseudo==1 & treat==1 & decile==`D'
				local temp=r(mean)
				sum `Y'_`i'_bc if pseudo==1 & treat==0 & decile==`D'
				local DnD_pseudo`i'=`temp'-r(mean)-`diff0'		
			}
			clear
			use temp2.dta
			forvalues i=1/6 {
				replace DnD`i'=`DnD`i'' if _n==`bootstrap'
				replace DnD_pseudo`i'=`DnD_pseudo`i'' if _n==`bootstrap'
			}	
			save temp2.dta, replace
			clear
			restore
			if `bootstrap'==1 | `bootstrap'==25 | `bootstrap'-int(`bootstrap'/100)*100==0 {
				noisily display "Completed bootstrapped sample `bootstrap' of 1000 at $S_TIME, $S_DATE"
			}
		}
		preserve
		clear
		use temp2.dta
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
		}	
		clear
		erase temp2.dta
		forvalues j=2/7 {
			matrix A[7,`j']=2*(1-normal(abs(A[5,`j']/A[6,`j'])))
			matrix A[15,`j']=2*(1-normal(abs(A[13,`j']/A[14,`j'])))
			matrix A[19,`j']=2*(1-normal(abs(A[17,`j']/A[18,`j'])))
		}
		svmat A
		export excel "${path_output}app_table8_`Y'_`D'.xlsx", replace
		clear
		restore
	}
	drop *_bc
}
capture clear
capture log close