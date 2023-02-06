
set matsize 11000
set more off

log using "${path_log}/Entrants_MA_Analysis.log", replace

* Save Sets of 5 Contiguous PUMAs as a matrix
insheet using "${path_data}contiguous.csv", comma 
drop v1
*use R:/Project/SeattleMinimumWage/Stata/AttenuationPaper/dofiles/AEJEP_submission_2020/data/contiguous.dta
forvalues X=1/5 {
	replace puma`X'=puma`X'-500000 if puma`X'>=500000
	replace puma`X'=puma`X'-400000 if puma`X'>=400000
}
mkmat puma1 puma2 puma3 puma4 puma5, matrix(CONTIGUOUS)

foreach STEM1 in  entrants_only_ {
	foreach STEM2 in 15_5y_  { 

		display "**************************************************************************************"
		display "  Analysis of `STEM1'`STEM2'"
		display "**************************************************************************************"

		* Set up a stacked dataset
		clear
		use "${path_data}`STEM1'`STEM2'PUMA.dta"
		local STEM3 ent
		if "`STEM1'"=="entrant_reentrant_" {
			local STEM3 reent
			rename seattle_reentrants_MA seattle_entrants_MA
		}
		* add moving averages for each of Seattle's 5 individual PUMAs
		foreach X in 11601 11602 11603 11604 11605 {
			gen P`X'_MA=`STEM3'`X'+`STEM3'`X'[_n-1]+`STEM3'`X'[_n-2]+`STEM3'`X'[_n-3]
			local base=P`X'_MA[18]
			replace P`X'_MA=100*P`X'_MA/`base'
		}
		list seattle_entrants_MA P11601_MA P11602_MA P11603_MA P11604_MA P11605_MA
		line seattle_entrants_MA P11601_MA P11602_MA P11603_MA P11604_MA P11605_MA quarter, lwidth(thick medium medium medium medium medium) lpattern(solid dash solid dash solid dash)
		* Add moving averages for each set of 5 Contiguous PUMAs
		qui forvalues X=1/2994 {
			gen temp=0
			forvalues Y=1/5 {
				local puma=CONTIGUOUS[`X',`Y']
				replace temp=temp+`STEM3'`puma'
			}
			gen P`X'_MA=temp+temp[_n-1]+temp[_n-2]+temp[_n-3]
			local base=P`X'_MA[18]
			replace P`X'_MA=100*P`X'_MA/`base'
			drop temp
		}
		keep if seattle_entrants_MA~=.
		preserve
		keep quarter seattle_entrants_MA
		gen puma=0
		* Note: puma=0 denotes Seattle
		rename seattle_entrants_MA entrants_MA
		save R:/Project/SeattleMinimumWage/Stata/AttenuationPaper/dofiles/AEJEP_submission_2020/data/`STEM1'`STEM2'MA_PUMA_stacked.dta, replace
		restore
		quietly foreach X in P10100_MA P10200_MA P10300_MA P10400_MA P10501_MA P10502_MA P10503_MA P10504_MA P10600_MA P10701_MA P10702_MA P10703_MA P10800_MA P10901_MA P10902_MA P11000_MA P11101_MA P11102_MA P11103_MA P11104_MA P11200_MA P11300_MA P11401_MA P11402_MA P11501_MA P11502_MA P11503_MA P11504_MA P11505_MA P11506_MA P11507_MA P11601_MA P11602_MA P11603_MA P11604_MA P11605_MA P11701_MA P11702_MA P11703_MA P11704_MA P11705_MA P11706_MA P11801_MA P11802_MA P11900_MA {
			keep if seattle_entrants_MA~=.
			preserve
			keep quarter `X'
			local puma=substr("`X'",2,5)
			gen puma=`puma'
			rename `X' entrants_MA
			append using "${path_data}`STEM1'`STEM2'MA_PUMA_stacked.dta"
			save ${path_data}"`STEM1'`STEM2'MA_PUMA_stacked.dta", replace
			restore
		}
		quietly forvalues X=1/2994 {
			if `X'==1 | `X'-(int(`X'/100))*100==0 {
				noisily display "Stacking Data for `X' of 2994 at $S_TIME  $S_DATE"
			}
			keep if seattle_entrants_MA~=.
			preserve
			keep quarter P`X'_MA
			gen puma=`X'
			rename P`X'_MA entrants_MA
			append using "${path_data}`STEM1'`STEM2'MA_PUMA_stacked.dta"
			save "${path_data}`STEM1'`STEM2'MA_PUMA_stacked.dta", replace
			restore
		}
		clear
		use "${path_data}`STEM1'`STEM2'MA_PUMA_stacked.dta"
		gen year=int(quarter)
		replace quarter=quarter-year
		replace quarter=1 if quarter<0.2
		replace quarter=2 if quarter<0.4
		replace quarter=3 if quarter<0.8
		replace quarter=4 if quarter<1
		gen time=4*(year-2010)+quarter-3
		tsset puma time
		save "${path_data}`STEM1'`STEM2'MA_PUMA_stacked.dta", replace

		* Make Figure Showing Comparison of Seattle and Synthetic Seattle
		* omit individual Seattle PUMAs and 2994 continguous groups from `STEM1'`STEM2'MA_PUMA_stacked.dta
		preserve
		drop if inlist(puma, 11601, 11602, 11603, 11604, 11605)
		drop if puma>=1 & puma<=2994
		synth entrants_MA entrants_MA(1(1)15), trunit(0) trperiod(16)
		matrix A=e(Y_synthetic)
		matrix B=e(Y_treated)
		matrix C=[A, B]
		matrix list C
		matrix D0=B-A
		restore
		preserve
		clear
		svmat C
		rename C1 SyntheticSeattle
		rename C2 Seattle
		gen year=2010.875+0.25*(_n-1)
		label var Seattle "Seattle"
		label var SyntheticSeattle "Synthetic Seattle"
		graph set window fontface "HelveticaNeueforSAS"
		twoway (line Seattle year, lcolor(black)) || (line SyntheticSeattle year, lcolor(blue) lpatter(dash) xtitle("") xline(2014.5 2015.25 2016) ytitle("Number of New Entrants" "(4-Quarter Moving Average)" "(Normalized Such That 2014q2 = 100)" " ") yscale(range(70 120)) ylabel(80 90 100 110 120) xlabel(2010 2012 2014 2016) scheme(s1color) text(70 2014.7 "Minimum wage ordinance passed," "but not yet in force.", place(n) alignment(baseline) justification(left) orientation(vertical) color(black) size(small)) text(70 2015.35 "Top minimum wage in Seattle = $11.", place(n) orientation(vertical) alignment(baseline) justification(left) color(black) size(small)) text(70 2016.09 "Top minimum wage in Seattle = $13.", place(n) orientation(vertical) alignment(baseline) justification(left) color(black) size(small))) 
		graph export "${path_output}`STEM1'`STEM2'MA.png", replace width(3000)
		graph export "${path_output}`STEM1'`STEM2'MA.tif", replace 
		graph set window fontface default
		restore

				* Make Figure Showing Difference Between Seattle and Synthetic Seattle, and repeat for all combinations of 5 contiguous PUMAs
		* repeat for all combinations of 5 contiguous PUMAs
		quietly forvalues COMBO=1/2994 {
			if `COMBO'==1 | `COMBO'-(int(`COMBO'/100))*100==0 {
				noisily display "Combo `COMBO' of 2994 at $S_TIME  $S_DATE"
			}
			preserve
			local puma1=CONTIGUOUS[`COMBO',1]
			local puma2=CONTIGUOUS[`COMBO',2]
			local puma3=CONTIGUOUS[`COMBO',3]
			local puma4=CONTIGUOUS[`COMBO',4]
			local puma5=CONTIGUOUS[`COMBO',5]
			drop if inlist(puma, `puma1', `puma2', `puma3', `puma4', `puma5')
			drop if puma>=0 & puma<`COMBO'
			drop if puma>`COMBO' & puma<=2943
			if `COMBO'!=2994 {
				drop if puma==2994
			}
			synth entrants_MA entrants_MA(1(1)15), trunit(`COMBO') trperiod(16)
			matrix A=e(Y_synthetic)
			matrix B=e(Y_treated)
			matrix D`COMBO'=B-A
			restore
		}

		clear
		quietly forvalues D=0/2994 {
			svmat D`D'
			rename D`D'1 D`D'
		}
		gen year=2010.875+0.25*(_n-1)
		graph set window fontface "HelveticaNeueforSAS"
		label var D0 "Seattle"
		label var D1 "Comparison Region (5 Contiguous PUMAs)"
		twoway ///
		(line D1-D20 year,    lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D21-D40 year,   lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D41-D60 year,   lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D61-D80 year,   lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D81-D100 year,  lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D101-D120 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D121-D140 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D141-D160 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D161-D180 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D181-D200 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D201-D220 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D221-D240 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D241-D260 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D261-D280 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D281-D300 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D301-D320 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D321-D340 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D341-D360 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D361-D380 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D381-D400 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D401-D420 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D421-D440 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D441-D460 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D461-D480 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D481-D500 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D501-D520 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D521-D540 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D541-D560 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D561-D580 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D581-D600 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D601-D620 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D621-D640 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D641-D660 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D661-D680 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D681-D700 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D701-D720 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D721-D740 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D741-D760 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D761-D780 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D781-D800 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D801-D820 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D821-D840 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D841-D860 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D861-D880 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D881-D900 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D901-D920 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D921-D940 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D941-D960 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D961-D980 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D981-D1000 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1001-D1020 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1021-D1040 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1041-D1060 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1061-D1080 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1081-D1100 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1101-D1120 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1121-D1140 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1141-D1160 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1161-D1180 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1181-D1200 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1201-D1220 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1221-D1240 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1241-D1260 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1261-D1280 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1281-D1300 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1301-D1320 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1321-D1340 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1341-D1360 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1361-D1380 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1381-D1400 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1401-D1420 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1421-D1440 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1441-D1460 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1461-D1480 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1481-D1500 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1501-D1520 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1521-D1540 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1541-D1560 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1561-D1580 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1581-D1600 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1601-D1620 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1621-D1640 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1641-D1660 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1661-D1680 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1681-D1700 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1701-D1720 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1721-D1740 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1741-D1760 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1761-D1780 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1781-D1800 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1801-D1820 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1821-D1840 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1841-D1860 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1861-D1880 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1881-D1900 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1901-D1920 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1921-D1940 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1941-D1960 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1961-D1980 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D1981-D2000 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2001-D2020 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2021-D2040 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2041-D2060 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2061-D2080 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2081-D2100 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2101-D2120 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2121-D2140 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2141-D2160 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2161-D2180 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2181-D2200 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2201-D2220 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2221-D2240 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2241-D2260 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2261-D2280 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2281-D2300 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2301-D2320 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2321-D2340 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2341-D2360 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2361-D2380 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2381-D2400 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2401-D2420 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2421-D2440 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2441-D2460 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2461-D2480 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2481-D2500 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2501-D2520 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2521-D2540 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2541-D2560 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2561-D2580 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2581-D2600 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2601-D2620 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2621-D2640 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2641-D2660 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2661-D2680 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2681-D2700 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2701-D2720 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2721-D2740 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2741-D2760 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2761-D2780 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2781-D2800 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2801-D2820 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2821-D2840 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2841-D2860 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2861-D2880 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2881-D2900 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2901-D2920 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2921-D2940 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2941-D2960 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2961-D2980 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin)) || /// 
		(line D2981-D2994 year, lcolor(ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue ltblue) lwidth(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin) yline(0) xline(2014.5 2015.25 2016) ytitle("Difference Between This Set of Five" "Contiguous PUMA's Number of New Entrants" "(Year-Over-Year Percentage Change)" "and Synthetic Comparison Group" " ") yscale(range(-30 30)) ylabel(-30 -20 -10 0 10 20 30) xlabel(2010 2012 2014 2016) scheme(s1color) text(-30 2014.7 "Minimum wage ordinance passed," "but not yet in force.", place(n) alignment(baseline) justification(left) orientation(vertical) color(black) size(small)) text(-30 2015.35 "Top minimum wage in Seattle = $11.", place(n) orientation(vertical) alignment(baseline) justification(left) color(black) size(small)) text(-30 2016.09 "Top minimum wage in Seattle = $13.", place(n) orientation(vertical) alignment(baseline) justification(left) color(black) size(small))) || ///
		(line D0 year,        lcolor(black) lwidth(thick)) || /// 
		(line D1 D0 year if year==0, lcolor(ltblue black) lwidth(medium thick) xtitle("") legend(on order(2997 2996) col(1) label(1 "Seattle" 2 "Comparison Region (5 Contiguous PUMAs)")))
		graph export "${path_output}`STEM1'`STEM2'MA_difference.png", replace width(3000)
		graph export  "${path_output}`STEM1'`STEM2'MA_difference.tif", replace 
		graph set window fontface default

		* Assess the "significance" of Seattle
		* Following Abadie, Diamond, and Hainmueller, JASA, 2010, exclude comparison regions where the "preintervention mean squared prediction error (MSPE)" is 20, 5, and 2 times greater than the MSPE in Seattle, and reevaluate inference.
		forvalues Q=1/24 {
			local count1=0
			local count2=2994
			local count3=0
			local count4=2994
			local count5=0
			local count6=2994
			local count7=0
			local temp=D0[`Q']
			forvalues X=1/2994 {
				if abs(`temp')>abs(D`X'[`Q']) {
					local count1=`count1'+1
				}
				qui gen temp=D0^2 if _n<=16
				qui sum temp 
				local seattleMSPE=r(mean)
				drop temp
				qui gen temp=D`X'^2 if _n<=16
				qui sum temp 
				if r(mean)<20*`seattleMSPE' {
					local count2=`count2'-1
					if abs(`temp')>abs(D`X'[`Q']) {
						local count3=`count3'+1
					}
				}
				if r(mean)<5*`seattleMSPE' {
					local count4=`count4'-1
					if abs(`temp')>abs(D`X'[`Q']) {
						local count5=`count5'+1
					}
				}
				if r(mean)<2*`seattleMSPE' {
					local count6=`count6'-1
					if abs(`temp')>abs(D`X'[`Q']) {
						local count7=`count7'+1
					}
				}
				drop temp
			}
			if `Q'==1 {
				display "Number of comparison regions where the MSPE is 20 times greater than the MSPE in Seattle = `count2'"
				display "Number of comparison regions where the MSPE is 5 times greater than the MSPE in Seattle = `count4'"
				display "Number of comparison regions where the MSPE is 2 times greater than the MSPE in Seattle = `count6'"
			}
			local count2=2994-`count2'
			local count4=2994-`count4'
			local count6=2994-`count6'
			local p1=1-`count1'/2994
			local p2=1-`count3'/`count2'
			local p3=1-`count5'/`count4'
			local p4=1-`count7'/`count6'
			display "Seattle exceeds `count1' of 2994 comparison regions in quarter `Q'; two-tailed p-value = `p1'"
			display "Seattle exceeds `count3' of `count2' comparison regions in quarter `Q'; two-tailed p-value = `p2'"
			display "Seattle exceeds `count5' of `count4' comparison regions in quarter `Q'; two-tailed p-value = `p3'"
			display "Seattle exceeds `count7' of `count6' comparison regions in quarter `Q'; two-tailed p-value = `p4'"
		}
	}
}
clear
log close

		
	