

set more off

capture log close
log using "${path_log}get_unique_addresses_v5.log", replace

set max_memory .

* Append Data Files
forvalues X=2005/2016 {
	display ""
	display "***********************************************************************************"
	display "*** Year `X' **********************************************************************"
	display "***********************************************************************************"
	if `X' < 2016 {
	insheet using F:\data_pull_2\UW-`X'.csv, comma
	}
	if `X'==2016 {
	insheet using F:\Dec2016update\UW-`X'.csv, comma
	}
	keep employeraccountnumber address zipcode city
	
	***** Split zipcode into 5-digit and 9-digit ****
	gen zip9 = zipcode if zipcode > 1e5
	gen zip5 = zipcode if zipcode < 1e5
	replace zip5 = floor(zipcode/10000) if zipcode > 1e5
	replace zip5 = . if zip5 == 0
	replace zip9 = . if zip9 == 0

	***** Set zip5 to missing if it does not have 5 digits ****
	replace zip9 = . if zip5 < 1e4
	replace zipcode = . if zip5 < 1e4
	replace zip5 = . if zip5 < 1e4
	
	***** Set zipcode to missing if it does not start with 98 or 99 (WA zipcodes) ****
	replace zipcode = . if floor(zip5/1000)!=98 & floor(zip5/1000)!=99
	replace zip9 = . if floor(zip5/1000)!=98 & floor(zip5/1000)!=99
	replace zip5 = . if floor(zip5/1000)!=98 & floor(zip5/1000)!=99	

	* Fill in missing zip9 codes if zip5 is never missing
	egen zip9_min = min(zip9), by(employeraccountnumber address city)
	egen zip9_max = max(zip9), by(employeraccountnumber address city)
	replace zip9 = zip9_max if zip9 == . & zip9_min == zip9_max & zip5 < .

	drop zip9_min zip9_max zipcode 	

	***** Append from previous years ****
	if `X'>2005 {
		append using ESD_Acct_Address_List_v4.dta
		
		* Drop duplicates
		duplicates drop 
	
	}
   
	
	***** Save an updates list with EAN, address, zip *****
	save ESD_Acct_Address_List_v4.dta, replace

	clear
}
*new data
	insheet using F:\Oct2017update\UW-2016Q4.csv, comma
	keep employeraccountnumber address zipcode city
	
	***** Split zipcode into 5-digit and 9-digit ****
	gen zip9 = zipcode if zipcode > 1e5
	gen zip5 = zipcode if zipcode < 1e5
	replace zip5 = floor(zipcode/10000) if zipcode > 1e5
	replace zip5 = . if zip5 == 0
	replace zip9 = . if zip9 == 0

	***** Set zip5 to missing if it does not have 5 digits ****
	replace zip9 = . if zip5 < 1e4
	replace zipcode = . if zip5 < 1e4
	replace zip5 = . if zip5 < 1e4
	
	***** Set zipcode to missing if it does not start with 98 or 99 (WA zipcodes) ****
	replace zipcode = . if floor(zip5/1000)!=98 & floor(zip5/1000)!=99
	replace zip9 = . if floor(zip5/1000)!=98 & floor(zip5/1000)!=99
	replace zip5 = . if floor(zip5/1000)!=98 & floor(zip5/1000)!=99	

	* Fill in missing zip9 codes if zip5 is never missing
	egen zip9_min = min(zip9), by(employeraccountnumber address city)
	egen zip9_max = max(zip9), by(employeraccountnumber address city)
	replace zip9 = zip9_max if zip9 == . & zip9_min == zip9_max & zip5 < .

	drop zip9_min zip9_max zipcode 	

	***** Append from previous years ****
	append using ESD_Acct_Address_List_v4.dta	
	* Drop duplicates
	duplicates drop    	
	***** Save an updates list with EAN, address, zip *****
	save ESD_Acct_Address_List_v4.dta, replace
	clear
	
	*new data
	insheet using F:\Oct2017update\UW-2017Q1.csv, comma
	keep employeraccountnumber address zipcode city
	
	***** Split zipcode into 5-digit and 9-digit ****
	gen zip9 = zipcode if zipcode > 1e5
	gen zip5 = zipcode if zipcode < 1e5
	replace zip5 = floor(zipcode/10000) if zipcode > 1e5
	replace zip5 = . if zip5 == 0
	replace zip9 = . if zip9 == 0

	***** Set zip5 to missing if it does not have 5 digits ****
	replace zip9 = . if zip5 < 1e4
	replace zipcode = . if zip5 < 1e4
	replace zip5 = . if zip5 < 1e4
	
	***** Set zipcode to missing if it does not start with 98 or 99 (WA zipcodes) ****
	replace zipcode = . if floor(zip5/1000)!=98 & floor(zip5/1000)!=99
	replace zip9 = . if floor(zip5/1000)!=98 & floor(zip5/1000)!=99
	replace zip5 = . if floor(zip5/1000)!=98 & floor(zip5/1000)!=99	

	* Fill in missing zip9 codes if zip5 is never missing
	egen zip9_min = min(zip9), by(employeraccountnumber address city)
	egen zip9_max = max(zip9), by(employeraccountnumber address city)
	replace zip9 = zip9_max if zip9 == . & zip9_min == zip9_max & zip5 < .

	drop zip9_min zip9_max zipcode 	

	***** Append from previous years ****
	append using ESD_Acct_Address_List_v4.dta	
	* Drop duplicates
	duplicates drop    	
	***** Save an updates list with EAN, address, zip *****
	save ESD_Acct_Address_List_v4.dta, replace
	clear

*new data
	insheet using F:\Oct2017update\UW-2017Q2.csv, comma
	keep employeraccountnumber address zipcode city
	
	***** Split zipcode into 5-digit and 9-digit ****
	gen zip9 = zipcode if zipcode > 1e5
	gen zip5 = zipcode if zipcode < 1e5
	replace zip5 = floor(zipcode/10000) if zipcode > 1e5
	replace zip5 = . if zip5 == 0
	replace zip9 = . if zip9 == 0

	***** Set zip5 to missing if it does not have 5 digits ****
	replace zip9 = . if zip5 < 1e4
	replace zipcode = . if zip5 < 1e4
	replace zip5 = . if zip5 < 1e4
	
	***** Set zipcode to missing if it does not start with 98 or 99 (WA zipcodes) ****
	replace zipcode = . if floor(zip5/1000)!=98 & floor(zip5/1000)!=99
	replace zip9 = . if floor(zip5/1000)!=98 & floor(zip5/1000)!=99
	replace zip5 = . if floor(zip5/1000)!=98 & floor(zip5/1000)!=99	

	* Fill in missing zip9 codes if zip5 is never missing
	egen zip9_min = min(zip9), by(employeraccountnumber address city)
	egen zip9_max = max(zip9), by(employeraccountnumber address city)
	replace zip9 = zip9_max if zip9 == . & zip9_min == zip9_max & zip5 < .

	drop zip9_min zip9_max zipcode 	

	***** Append from previous years ****
	append using ESD_Acct_Address_List_v4.dta	
	* Drop duplicates
	duplicates drop    	
	***** Save an updates list with EAN, address, zip *****
	save ESD_Acct_Address_List_v4.dta, replace
	clear

/* Mark non-valid addresses */


use ESD_Acct_Address_List_v4.dta, clear


* Split into "STATEWIDE" amd "COUNTYWIDE" "UNKNOWN" and "MAIN ST"

gen valid_address = 0
replace valid_address = 1 if trim(address)==""
replace valid_address = 1 if trim(address)=="STATEWIDE"
replace valid_address = 2 if trim(address)=="UNKNOWN"
replace valid_address = 2 if trim(address)=="MAINST"
replace valid_address = 2 if trim(address)=="MAIN ST"
replace valid_address = 2 if trim(address)=="MAINSTREET"
replace valid_address = 2 if trim(address)=="MAIN"
replace valid_address = 2 if trim(address)=="VARIOUS"
replace valid_address = 2 if trim(address)=="NONE"
replace valid_address = 1 if trim(address)=="COUNTYWIDE"	

egen long address_id = group(address city zip5)
gen state = "WA"

*This saves the EAN, Address_ID and address, zip, state together
save ESD_Acct_Address_List_v4.dta, replace

/* Get unique "dirty" addresses */
drop  zip9 employeraccountnumber
duplicates drop


outsheet using ESD_Dirty_Address_List_v4.csv, comma replace

log close
clear




