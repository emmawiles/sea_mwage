capture clear all
capture log close
set more off

*******************************************************************************************************
* PATHS 
*******************************************************************************************************

global path_project "R:\\Project\SeattleMinimumWage\\Stata\\AttenuationPaper\\dofiles\\AEJEP_submission_2020\\"
global path_ESD "R:\Project\SeattleMinimumWage\Data\State Data\ESD\data\"
global path_cpi "${path_project}\\data\\"
global path_geo_data "${path_project}\\data\\data_confidential\\"
global path_data "${path_project}\\data\\data_confidential\\"
log using "${path_project}\\log\\log_confidential\\Entrant_analysis_`datename'.log", replace

* Exclude data from one suspicious "employer" 
use personid yearquarter time t_in wagerate multiest bad_data puma_id employed acct using "${path_data}\worker_analysis_Fall_2018_100.dta" if employed==1 & acct!=86130
sort personid yearquarter
gen ent_15_5y= ((personid!=personid[_n-1] & time==t_in & yearquarter>= 20101)) & wagerate<15 & multiest==0 & bad_data==0
gen ent_15_1y= ((personid!=personid[_n-1] & time==t_in & yearquarter>= 20101)) & wagerate<15 & multiest==0 & bad_data==0
gen ent_15_1q= ((personid!=personid[_n-1] & time==t_in & yearquarter>= 20101)) & wagerate<15 & multiest==0 & bad_data==0
gen ent_19_5y= ((personid!=personid[_n-1] & time==t_in & yearquarter>= 20101)) & wagerate<19 & multiest==0 & bad_data==0
gen ent_19_1y= ((personid!=personid[_n-1] & time==t_in & yearquarter>= 20101)) & wagerate<19 & multiest==0 & bad_data==0
gen ent_19_1q= ((personid!=personid[_n-1] & time==t_in & yearquarter>= 20101)) & wagerate<19 & multiest==0 & bad_data==0
gen reent_15_5y= ((personid==personid[_n-1] & time-time[_n-1]>20) | (personid!=personid[_n-1] & time==t_in & yearquarter>= 20101)) & wagerate<15 & multiest==0 & bad_data==0
gen reent_15_1y= ((personid==personid[_n-1] & time-time[_n-1]>4)  | (personid!=personid[_n-1] & time==t_in & yearquarter>= 20101)) & wagerate<15 & multiest==0 & bad_data==0
gen reent_15_1q= ((personid==personid[_n-1] & time-time[_n-1]>1)  | (personid!=personid[_n-1] & time==t_in & yearquarter>= 20101)) & wagerate<15 & multiest==0 & bad_data==0
gen reent_19_5y= ((personid==personid[_n-1] & time-time[_n-1]>20) | (personid!=personid[_n-1] & time==t_in & yearquarter>= 20101)) & wagerate<19 & multiest==0 & bad_data==0
gen reent_19_1y= ((personid==personid[_n-1] & time-time[_n-1]>4)  | (personid!=personid[_n-1] & time==t_in & yearquarter>= 20101)) & wagerate<19 & multiest==0 & bad_data==0
gen reent_19_1q= ((personid==personid[_n-1] & time-time[_n-1]>1)  | (personid!=personid[_n-1] & time==t_in & yearquarter>= 20101)) & wagerate<19 & multiest==0 & bad_data==0
gen reemp_15_5y= ((personid==personid[_n-1] & time-time[_n-1]>20)) & wagerate<15 & multiest==0 & bad_data==0
gen reemp_15_1y= ((personid==personid[_n-1] & time-time[_n-1]>4) ) & wagerate<15 & multiest==0 & bad_data==0
gen reemp_15_1q= ((personid==personid[_n-1] & time-time[_n-1]>1) ) & wagerate<15 & multiest==0 & bad_data==0
gen reemp_19_5y= ((personid==personid[_n-1] & time-time[_n-1]>20)) & wagerate<19 & multiest==0 & bad_data==0
gen reemp_19_1y= ((personid==personid[_n-1] & time-time[_n-1]>4) ) & wagerate<19 & multiest==0 & bad_data==0
gen reemp_19_1q= ((personid==personid[_n-1] & time-time[_n-1]>1) ) & wagerate<19 & multiest==0 & bad_data==0
keep if yearquarter>=20101
collapse (sum) ent_15_5y ent_19_5y ent_15_1y ent_19_1y ent_15_1q ent_19_1q reent_15_5y reent_19_5y reent_15_1y reent_19_1y reent_15_1q reent_19_1q  reemp_15_5y reemp_19_5y reemp_15_1y reemp_19_1y reemp_15_1q reemp_19_1q, by (puma_id yearquarter)
save "${path_data}\\entrants_reentrats_pumaid_100.dta", replace
clear




foreach i in 15_5y      {
use "${path_data}\entrants_reentrats_pumaid_100.dta",clear
drop if yearquarter < 20101
generate quarter =0
forvalues year= 2010/2017{
	replace quarter = `year'.13 if yearquarter ==`year'1
	replace quarter = `year'.38 if yearquarter ==`year'2
	replace quarter = `year'.63 if yearquarter ==`year'3
	replace quarter = `year'.88 if yearquarter ==`year'4
	}
drop yearquarter	

*drop region 3
drop if puma_id >= 11606 & puma_id <= 11616
drop if puma_id ==0
drop if puma_id ==.

drop if quarter > 2016.75
*drop emp_15 share_entered
rename ent_`i' ent
keep ent quarter puma_id

reshape wide ent   , i(quarter) j(puma_id)


gen seattle_entrants = ent11601 + ent11602 + ent11603 + ent11604 + ent11605

label var ent11601 "SeattlePUMA"
label var ent11602 "SeattlePUMA"
label var ent11603 "SeattlePUMA"
label var ent11604 "SeattlePUMA"
label var ent11605 "SeattlePUMA"

gen temp_sea=(seattle_entrants+seattle_entrants[_n-1]+seattle_entrants[_n-2]+seattle_entrants[_n-3])/4
sum temp_sea if _n==18
local base=r(mean)
gen seattle_entrants_MA=100*temp_sea/`base'
label var seattle_entrants_MA "Seattle"
drop  temp_sea


foreach puma_id in 10100	10200	10300	10400	10501	10502	10503	10504	10600	10701	10702	10703	10800	10901	10902	11000	11101	11102	11103	11104	11200	11300	11401	11402	///
			11501	11502	11503	11504	11505	11506	11507	11701	11702	11703	11704	11705	11706	11801	11802	11900 {

	gen temp_puma`puma_id'=(ent`puma_id' +ent`puma_id'[_n-1]+ent`puma_id'[_n-2]+ent`puma_id'[_n-3])/4

	sum temp_puma`puma_id' if _n==18
	local base=r(mean)
	gen P`puma_id'_MA=100*temp_puma`puma_id'/`base'
	label var P`puma_id'_MA "OutlyingKingP `puma_id'"
	drop temp_puma`puma_id'
 
		}
		
order quarter seattle_entrants_MA P* seattle_entrants ent*		
save  "${path_data}\entrants_only_`i'_PUMA.dta", replace
}

log close