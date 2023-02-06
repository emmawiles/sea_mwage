
* Master file to run all STATA scripts used to create the tables and figures in 
* Minimum Wages and Low Wage Employment: Evidence from Seattle
* by Jardim, Long, Plotnick, Van Inwegen, Vigdor, Wething 2021
global path_project "R:\\Project\SeattleMinimumWage\\Stata\\AttenuationPaper\\dofiles\\AEJEP_submission_2020"
global path_data "${path_project}\\data\\"
global path_code "${path_project}\\code\\"
global path_output "${path_project}\\output\\"
global path_log "${path_project}\\log\\"

*** Install required packages

do "${path_code}\packages.do" 

*** Table 5 & Figure A12 
do "${path_code}\figure_5_a12.do" 

