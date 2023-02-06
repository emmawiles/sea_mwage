
*** THIS PROGRAM IS INTENDED TO PROCESS ADDRESS ESD DATA & GIS FILES TO CREATE GEOCODED_ESD_DATA

set more off
set matsize 11000
capture log close

*******************************************************************************************************
* PREFIX TO SAVE RESULTS
*******************************************************************************************************

local Prefix "Geocode_address_processing_samplesize`sample_size'"
local datename = trim("$S_DATE")
log using "${path_log}`Prefix'`datename'.log", replace

*******************************************************************************************************
* CHOOSE WHAT TO RUN
*******************************************************************************************************

local do_address_cleaning = 1
local do_PUMA_clean = 1

local do_merge_data = 1


*******************************************************************************************************
* LOAD AND PROCESS ZCTA, PUMA, Tract BLOCK GROUP, & BLOCK GEOCODED DATA FILES
*******************************************************************************************************
if `do_address_cleaning'==1 {


	*use "R:\Project\SeattleMinimumWage\Data\State Data\ESD\AddressProcessing\ESD_XY_ZCTA_join_v1.dta"
	import delimited "R:\Project\SeattleMinimumWage\Data\State Data\ESD\AddressProcessing\2017_update_XY.csv"
	
	duplicates drop address_id, force
	drop  if address_id ==.

	*Create variable for badmatch. Geocoded to Address point and street address = good matches
	encode loc_name, gen(temp)
	gen loc_num = temp
	tabulate  loc_num
	tabulate temp
	recode loc_num (4=1)	
	gen badmatch = 0
	replace badmatch = 1 if loc_num > 1
	tabulate badmatch
	gen bad_data=badmatch
	
***Tabulate bad known bad data sequentially***
	*tabulate zip5
	count if zip5==0 
	replace bad_data = 1 if zip5==0  	
	**count bad cities
	encode city_1, gen(city_num)
	*tabulate  city_num
	sort city_num
	count if (city_num == 1751  )
	**count bad addresses : statewide, POBoxes, "unknown" "various"
	sort address
	egen address_num= group(address)
	count if (address_num >= 396664 & address_num <= 396666)
	count if (address_num >= 394616 & address_num <=394630)
	count if (address_num >=396972 &  address_num <=397008)
	count if (address_num >= 397539 & address_num <= 397632)
	count if (address_num >=395138 & address_num <= 395322) 
	count if (address_num >= 395412 & address_num <= 395894)
	replace bad_data = 0 if ((address_num >=395138 & address_num <= 395322) | (address_num >= 395412 & address_num <= 395894))

	tab bad_data
	preserve
	keep bad_data address_id 
	sort address_id
	save temp_baddata.dta, replace
	restore
	drop  badmatch valid_address temp city_num address_num objectid rank ref_id rank ref_id addnumto   ///
	addnumfrom streetid pntaddid distance langcode country addnum side stpredir stpretype stname sttype stdir 
	rename address ESD_address
	rename city_1 ESD_city
	rename zip5 ESD_zip5
	save "R:\Project\SeattleMinimumWage\Data\State Data\ESD\AddressProcessing\Geocodes_v4.dta", replace
	clear
}

***DROP UNNECESSARY VARIABLES 
if `do_PUMA_clean'==1 {
	import delimited "R:\Project\SeattleMinimumWage\Data\State Data\ESD\AddressProcessing\2017_update_PUMA3.csv"	
	duplicates drop address_id, force
	drop  if address_id ==.
	sort address_id

	destring pumace10, replace
	rename pumace10 puma_id
	tabulate puma_id,m
	merge address_id using temp_baddata.dta
	sort bad_data puma_id 

	keep puma_id address_id bad_data
			
	sort address_id
	save "R:\Project\SeattleMinimumWage\Data\State Data\ESD\AddressProcessing\2017_PUMA.dta",replace
	clear	
}



*MERGE DATA
if `do_merge_data'==1 {
	use "R:\Project\SeattleMinimumWage\Data\State Data\ESD\AddressProcessing\Geocodes_v4.dta"
	sort address_id
	merge address_id using "R:\Project\SeattleMinimumWage\Data\State Data\ESD\AddressProcessing\2017_PUMA.dta"
	drop _merge 	
	
	*Generate region from PUMAS
	drop region regionabbr subregion
	gen region = .
	* Seattle
	replace region = 1 if (puma_id >= 11601 & puma_id < 11606)
	* SeaTac (or at least some part of census tract in SeaTac)
	replace region = 2 if (county_tract_id == 33273 | county_tract_id == 33280 | county_tract_id == 33281)  & region==.
	replace region = 2 if (county_tract_id == 33283 | county_tract_id == 3328402 | county_tract_id == 3328403) & region==.
	replace region = 2 if (county_tract_id == 33285 | county_tract_id == 33287 | county_tract_id == 3328801) & region==.
	replace region = 2 if (county_tract_id == 3328802 | county_tract_id == 3329101)  & region==.
	* King County outside of Seattle and SeaTac
	replace region = 3 if (puma_id>= 11606 & puma_id < 11617) & region==.
	* Snohomish, Pierce, and Kitsap Counties
	replace region = 4 if  (puma_id >= 11801 & puma_id < 11803) & region==.
	replace region = 4 if  (puma_id >= 11501 & puma_id < 11508) & region==.
	replace region = 4 if (puma_id >= 11701 & puma_id < 11707) & region==.
	* Rest of Washington (outside of King / SKP)
	replace region = 5 if region == .
	* Not geocoded or invalid address
	replace region = 9 if  bad_data == 1	

drop if address_id==. 
drop if address_id==0

save "R:\Project\SeattleMinimumWage\Data\State Data\ESD\Geocodes_v4.dta", replace
clear
}







