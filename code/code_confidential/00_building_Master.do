* Master file to run all programs used to process data in
* Minimum Wages and Low Wage Employment: Evidence from Seattle
* by Jardim, Long, Plotnick, Van Inwegen, Vigdor, Wething 2021

***********************************************
***                                         ***
*** Process & Geocode ESD data              ***
***                                         ***
***********************************************

* 1. Get Unique Addresses to Geocode
do R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\code_confidential\get_unique_addresses_v6.do 

* 2.  Geocode Addresses

* 3.  Merge ESD address data with Geocoded data
do R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\code_confidential\Address_processing_v9.do

* 4. Build ESD data file
do R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\code_confidential\create_stata_file_v5.do

***********************************************
***                                         ***
*** Build Aggregate Analysis                ***
***                                         ***
***********************************************

* 1. Built Aggregate the ESD data at the county and PUMA level 
* Note: use an integer number X<100 to run on X% of the data for testing
do R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\code_confidential\A_Build_aggregate_jobs_v16.do 100

* 2. Aggregate the ESD data at PUMA level for designated wagegroups
do R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\code_confidential\A_Build_aggregate_groups_jobs.do 

***********************************************
***                                         ***
*** Build Worker-Level Analysis             ***
***                                         ***
***********************************************

* 1. Build Wage distribution in Seattle before and after the min wage passage 
* Note: use an integer number X<100 to run on X% of the data for testing
* Note: need to run A_Build_aggregate_jobs_v16.do first
do R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\code_confidential\M_Build_WageDistribution_v3.do 100

* 2. Calculate probability to transition to non-locatable firm,  and probability to transition to a job which pays >$19/hour 
* Note: use an integer number X<100 to run on X% of the data for testing
* Note: the 2nd argument controls the version number
* Note: need to run A_Build_aggregate_jobs_v16.do first
do R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\code_confidential\M_Micro_evidence_v8.do 100 6


* 3. Build longitudinal data and person level cohorts for worker analysis            
* Note: use an integer number X<100 to run on X% of the data for testing      
do R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\code_confidential\W_Build_workers_analysis_v3.do 100


*2. Program to create matches for nearest neighbor matching strategy
* NOTE: THIS IS AN R FILE: 
*R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\code_confidential\W_Find_matches_workers.R	

*3. Merge the matches onto the cohort data	
do R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\code_confidential\W_ESD_build_matched_sample_v1.do 

* 4. Build data for entrants  analysis             
do R:\Project\SeattleMinimumWage\Stata\AttenuationPaper\dofiles\AEJEP_submission_2020\code\code_confidential\M_gen_entrants_data.do 
