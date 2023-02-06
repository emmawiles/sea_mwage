* Master file to run all STATA scripts used to create the tables and figures in 
* Minimum Wages and Low Wage Employment: Evidence from Seattle
* by Jardim, Long, Plotnick, Van Inwegen, Vigdor, Wething 2021
global path_project "R:\\Project\SeattleMinimumWage\\Stata\\AttenuationPaper\\dofiles\\AEJEP_submission_2020"
global path_data "${path_project}\\data\\data_confidential\\"
global path_code "${path_project}\\code\\"
global path_output "${path_project}\\output\\output_confidential\\"


do R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\packages.do

* Table 2, Appendix Table A1
* Note: Make sure data folder includes establishment_analysis_Spring_2018_100.dta
do R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\code_confidential\table_2_a1.do 


* Table 3.
* Note: Make sure data folder includes worker_analysis_Fall_2018_100.dta
do R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\code_confidential\table_3.do

* Table 4, 8, 9, Appendix Table 7, Appendix Figures 7, 8 
* Note: Make sure data folder includes mw2015_cohort_100.dta, mw2012_cohort_100.dta, ESDdata_matched_2015sample.dta and ESDdata_matched_2012sample.dta
do R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\code_confidential\tables_4_8_9_app_table_7_app_figs_7_8.do

* Figure 4.
* Note: Make sure data folder includes app_table7_wage.xlsx
do R:\Project\SeattleMinimumWage\Stata\Worker_analysis\code\fig_4.do

*Table 10 a, b, & c 
* Note: Make sure data folder includes ESDdata_matched_2015sample.dta and ESDdata_matched_2012sample.dta
do R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\code_confidential\table_10_panels_a_b_c.do

* Appendix Table A3
* Note: Make sure data folder includes establishment_analysis_Spring_2018_100.dta
do R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\code_confidential\table_a3.do

* Appendix Table A8 
* Note: Make sure data folder includes ESDdata_matched_2015sample.dta and ESDdata_matched_2012sample.dta
do R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\code_confidential\table_a8.do


* Appendix Figure A11
* Note: Make sure data folder includes jobs_bins_all.csv
do R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\code_confidential\figure_a11.do


