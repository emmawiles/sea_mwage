
clear
insheet using "${path_output}jobs_bins_all.csv" 
keep if method=="Synth_exclKing"
keep y0 variable_name t cicum_lower95 coefcum cicum_upper95
gen bin=.
forvalues X=9/24 {
	replace bin=`X' if variable_name=="d_bin_hours_flow`X'"
}
keep if bin~=.
sort t bin
drop variable_name
gen dhours=coefcum*y0/1000
label var dhours "Change in Hours for this Wage Bin"
gen dhours_lower=cicum_lower95*y0/1000
gen dhours_upper=cicum_upper95*y0/1000
forvalues X=4/9 {
	preserve
	keep if t==`X'
	replace bin=bin+0.5
	gen cumulative=dhours
	replace cumulative=dhours+cumulative[_n-1] if _n~=1
	label var cumulative "Cumulative Change in Hours"
	if `X'==4 {
		graph twoway (bar dhours bin, lcolor(black) fcolor(none) lwidth(medthick)) (rcap dhours_upper dhours_lower bin, lcolor(maroon)) (line cumulative bin, legend(col(1) size(vsmall) symysize(2.5) label(2 "95% Confidence Interval")) ylabel(-6000 -4000 -2000 0 2000) scheme(s1color) xscale(range(8 26)) xtitle("") xlabel(10 15 20 25, nolabels) title("$11 Minimum Wage" "2015.2", size(medium)) ytitle("Hours Worked (000s)" " ") saving("R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\revision_Spring2019\bins_figure_2015q2.gph", replace))
	}
	else if `X'==5 {
		graph twoway (bar dhours bin, lcolor(black) fcolor(none) lwidth(medthick)) (rcap dhours_upper dhours_lower bin, lcolor(maroon)) (line cumulative bin, legend(col(1) size(vsmall) symysize(2.5) label(2 "95% Confidence Interval")) ylabel(-6000 -4000 -2000 0 2000, nolabels) scheme(s1color) xscale(range(8 26)) xtitle("") xlabel(10 15 20 25, nolabels) title("$11 Minimum Wage" "2015.3", size(medium)) saving("R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\revision_Spring2019\bins_figure_2015q3.gph", replace))
	}
	else if `X'==6 {
		graph twoway (bar dhours bin, lcolor(black) fcolor(none) lwidth(medthick)) (rcap dhours_upper dhours_lower bin, lcolor(maroon)) (line cumulative bin, legend(col(1) size(vsmall) symysize(2.5) label(2 "95% Confidence Interval")) ylabel(-6000 -4000 -2000 0 2000, nolabels) scheme(s1color) xscale(range(8 26)) xtitle("") xlabel(10 15 20 25, nolabels) title("$11 Minimum Wage" "2015.4", size(medium)) saving("R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\revision_Spring2019\bins_figure_2015q4.gph", replace))
	}
	else if `X'==7 {
		graph twoway (bar dhours bin, lcolor(black) fcolor(none) lwidth(medthick)) (rcap dhours_upper dhours_lower bin, lcolor(maroon)) (line cumulative bin, legend(col(1) size(vsmall) symysize(2.5) label(2 "95% Confidence Interval")) ylabel(-6000 -4000 -2000 0 2000) scheme(s1color) xscale(range(8 26)) xlabel(10 15 20 25) title("$13 Minimum Wage" "2016.1", size(medium)) xtitle(" " "$1 Wage Bin" " ") ytitle("Hours Worked (000s)" " ") saving("R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\revision_Spring2019\bins_figure_2016q1.gph", replace))
	}
	else if `X'==8 {
		graph twoway (bar dhours bin, lcolor(black) fcolor(none) lwidth(medthick)) (rcap dhours_upper dhours_lower bin, lcolor(maroon)) (line cumulative bin, legend(col(1) size(vsmall) symysize(2.5) label(2 "95% Confidence Interval")) ylabel(-6000 -4000 -2000 0 2000, nolabels) scheme(s1color) xscale(range(8 26)) xlabel(10 15 20 25) title("$13 Minimum Wage" "2016.2", size(medium)) xtitle(" " "$1 Wage Bin" " ") saving("R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\revision_Spring2019\bins_figure_2016q2.gph", replace))
	}
	else {
		graph twoway (bar dhours bin, lcolor(black) fcolor(none) lwidth(medthick)) (rcap dhours_upper dhours_lower bin, lcolor(maroon)) (line cumulative bin, legend(col(1) size(vsmall) symysize(2.5) label(2 "95% Confidence Interval")) ylabel(-6000 -4000 -2000 0 2000, nolabels) scheme(s1color) xscale(range(8 26)) xlabel(10 15 20 25) title("$13 Minimum Wage" "2016.3", size(medium)) xtitle(" " "$1 Wage Bin" " ") saving("R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\revision_Spring2019\bins_figure_2016q3.gph", replace))
	}
	restore
}

grc1leg "${path_output}bins_figure_2015q2.gph" "${path_output}bins_figure_2015q3.gph" "${path_output}bins_figure_2015q4.gph" "${path_output}bins_figure_2016q1.gph" "${path_output}bins_figure_2016q2.gph" "${path_output}\bins_figure_2016q3.gph", scheme(s1color) ring(6) imargin(small) altshrink
graph export"${path_output}appendix_11.png", width(3000) replace


* clear
