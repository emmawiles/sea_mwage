
**** NOTE: THIS IS AN EDITED VERSION OF "workers_effect.do" AND CORRESPONDS TO THE FINAL ACCEPTED VERSION OF THE PAPER.


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
global path_output "${path_project}output\\"


****************************************************************************
* FIGURE 4
****************************************************************************

import excel "R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\output\app_table7_wage.xlsx",  cellrange("B17:G18")

mkmat B C D E F G, matrix(A)
matrix A=A'
clear
svmat A
gen time=2015.375 if _n==1
replace time=time[_n-1]+0.25 if _n>1
gen upper=A1+1.96*A2
gen lower=A1-1.96*A2
graph set window fontface "HelveticaNeueforSAS"
graph twoway (line A1 time, lcolor(black) lwidth(medthick) yline(0, lcolor(black) lpattern(dash))) (rcap upper lower time, lcolor(maroon) legend(size(vsmall) symysize(2.5) col(1) label(1 "Estimated Effect") label(2 "95% Confidence Interval")) ylabel(-2 "-$2" -1.50 "-$1.50" -1 "-$1" -0.5 "-$0.50" 0 "$0" 0.5 "$0.50" 1 "$1" 1.50 "$1.50" 2 "$2", angle(0)) scheme(s1color) title("Wages", size(large)) xtitle("") xlabel(2015.375 "2015.2" 2015.625 "2015.3" 2015.875 "2015.4" 2016.125 "2016.1" 2016.375 "2016.2" 2016.625 "2016.3") ytitle("") saving("${path_output}workers_effect_on_wages.gph", replace))
clear

import excel R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\output\app_table7_employed.xlsx, cellrange("B17:G18")
mkmat B C D E F G, matrix(A)
matrix A=A'
clear
svmat A
gen time=2015.375 if _n==1
replace time=time[_n-1]+0.25 if _n>1
gen upper=A1+1.96*A2
gen lower=A1-1.96*A2
graph twoway (line A1 time, lcolor(black) lwidth(medthick) yline(0, lcolor(black) lpattern(dash))) (rcap upper lower time, lcolor(maroon) legend(size(vsmall) symysize(2.5) col(1) label(1 "Estimated Effect") label(2 "95% Confidence Interval")) yscale(range(-0.022 0.022)) ylabel(-0.02 "-0.02" -0.01 "-0.01" 0 0.01 "0.01" 0.02 "0.02", angle(0)) scheme(s1color) title("Employment", size(large)) xtitle("") xlabel(2015.375 "2015.2" 2015.625 "2015.3" 2015.875 "2015.4" 2016.125 "2016.1" 2016.375 "2016.2" 2016.625 "2016.3") ytitle("") saving("${path_output}workers_effect_on_employment.gph", replace))
clear

import excel R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\output\app_table7_hours.xlsx, cellrange("B17:G18")
mkmat B C D E F G, matrix(A)
matrix A=A'
clear
svmat A
gen time=2015.375 if _n==1
replace time=time[_n-1]+0.25 if _n>1
gen upper=A1+1.96*A2
gen lower=A1-1.96*A2
graph twoway (line A1 time, lcolor(black) lwidth(medthick) yline(0, lcolor(black) lpattern(dash))) (rcap upper lower time, lcolor(maroon) legend(size(vsmall) symysize(2.5) col(1) label(1 "Estimated Effect") label(2 "95% Confidence Interval")) ylabel(20 "  20" 15 "  15" 10 "  10" 5 "   5" 0 "    0" -5 "   -5" -10 "  -10" -15 "  -15" -20 "  -20", angle(0)) scheme(s1color) title("Hours Worked", size(large)) xtitle("") xlabel(2015.375 "2015.2" 2015.625 "2015.3" 2015.875 "2015.4" 2016.125 "2016.1" 2016.375 "2016.2" 2016.625 "2016.3") ytitle("") saving("${path_output}workers_effect_on_hours.gph", replace))
clear

import excel R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\output\app_table7_earnings.xlsx, cellrange("B17:G18")
mkmat B C D E F G, matrix(A)
matrix A=A'
clear
svmat A
gen time=2015.375 if _n==1
replace time=time[_n-1]+0.25 if _n>1
gen upper=A1+1.96*A2
gen lower=A1-1.96*A2
graph twoway (line A1 time, lcolor(black) lwidth(medthick) yline(0, lcolor(black) lpattern(dash))) (rcap upper lower time, lcolor(maroon) legend(size(vsmall) symysize(2.5) col(1) label(1 "Estimated Effect") label(2 "95% Confidence Interval")) yscale(range(-310 310)) ylabel(-300 "-$300" -200 "-$200" -100 "-$100" 0 "$0" 100 "$100" 200 "$200" 300 "$300", angle(0)) scheme(s1color) title("Earnings", size(large)) xtitle("") xlabel(2015.375 "2015.2" 2015.625 "2015.3" 2015.875 "2015.4" 2016.125 "2016.1" 2016.375 "2016.2" 2016.625 "2016.3") ytitle("") saving("${path_output}workers_effect_on_earnings.gph", replace))
clear

grc1leg R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\output\workers_effect_on_wages.gph R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\output\workers_effect_on_employment.gph R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\output\workers_effect_on_hours.gph R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\output\workers_effect_on_earnings.gph, scheme(s1color) ring(6) imargin(small) altshrink
graph export R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\output\fig_4.tif, replace
graph export R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\output\fig_4.png, replace
erase R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\output\workers_effect_on_wages.gph
erase R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\output\workers_effect_on_employment.gph
erase R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\output\workers_effect_on_hours.gph
erase R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\output\workers_effect_on_earnings.gph
graph set window fontface default

