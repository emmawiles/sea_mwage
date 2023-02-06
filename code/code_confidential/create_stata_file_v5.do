********************************************************************************************************
***THIS FILE WILL CREATE ESD_WAGES_V3 WITH DATA ENDING 2016Q3 & EXTERNAL DATAFILE 

********************************************************************************************************

set more off
capture log close
capture erase ESD_Wages_v4.dta
log using "${path_log}create_stata_file.log", replace
set max_memory .

*******************************************************************************************************
* CHOOSE WHAT TO RUN
*******************************************************************************************************

local do_build_data = 1


if `do_build_data'==1 {

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
		drop employeefirstname employeename
		tab naicscode if naicscode < 100000
		replace naicscode = naicscode*10 if naicscode < 100000
		gen naics2 = int(naicscode/10000)
	
		destring totalwageamount, replace ignore("$" ",")

     ***** Flag the observations with multi-establishment record ****
		gen multiest = 1 if county == "Multi Master" | county == "MULTI MASTER" | county == "multi master"
		replace multiest = 1 if county == "State-wide" | county == "STATE-WIDE" | county == "state-wide"
		replace multiest = 0 if multiest == .

	***** Flag the observations with unknown location (based on county) ****
		gen unknowncounty = 1 if county == "Unknown" | county == "UNKNOWN" | county == "unknown"
		replace unknowncounty = 1 if length(trim(county)) == 0
		replace unknowncounty = 1 if county == "Out of state" | county == "OUT OF STATE" | county == "out of state"
		replace unknowncounty = 0 if unknowncounty == .

		di _n "OBSERVATIONS WITH NON-WA LOCATION OR UNKNOWN LOCATION"
		tab unknowncounty
		di _n "OBSERVATIONS WITH MULTIPLE LOCATIONS"
		tab multiest
		di _n "OBSERVATIONS WITH MULTIPLE LOCATIONS IN WA"	
		tab multiest if unknowncounty == 0

	***** Check if any addresses have "PO BOX" in them ****
		count if regexm(address,"P\\.O\\.") == 1
		count if regexm(address,"Box") == 1	
		count if regexm(address,"0-90-90-90-90-9") == 1	

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

	***** Correct zipcodes is they are different within employer acc no. ****

		di _n "MISSING ZIPCODE BEFORE CORRECTION"
		count if zipcode == .

	* Fill in missing zip9 codes if zip5 is never missing
		egen zip9_min = min(zip9), by(employeracc)
		egen zip9_max = max(zip9), by(employeracc)
		replace zip9 = zip9_max if zip9 == . & zip9_min == zip9_max & zip5 < .

		drop zip9_min zip9_max

	* If have conflicting zipcode --> replace with the mode of zipcodes
		sort employeracc
		by employeracc: gen samezip5 = zip5[_n]==zip5[_n-1] if _n>1
		by employeracc: replace samezip5 = 1 if _n==1
		di _n "CONFLICTING ZIPCODES (0 = CONFLICT)"
		tab samezip5

		by employeracc: gen samezip9 = zip9[_n]==zip9[_n-1] if _n>1
		by employeracc: replace samezip9 = 1 if _n==1
		di _n "CONFLICTING ZIPCODES+4 (0 = CONFLICT)"
		tab samezip9	 

		egen samezip5_min = min(samezip5), by(employeracc)
		count if samezip5_min == 0
		if `r(N)' < 500 {
			list employeracc zip5 zip9 if samezip5_min == 0
			}

		egen samezip9_min = min(samezip9), by(employeracc)
		count if samezip9_min == 0
		if `r(N)' < 500 {
			list employeracc zip5 zip9 if samezip9_min == 0
			}

	* Change zipcode to the mode of the zipcode for these observations
		egen zip5_mode = mode(zip5) if samezip5_min == 0, by(employeracc) maxmode
		replace zip5 = zip5_mode if samezip5_min == 0

		egen zip9_mode = mode(zip9) if samezip9_min == 0, by(employeracc) maxmode
		replace zip9 = zip9_mode if samezip9_min == 0

		drop samezip5 samezip5_min zip5_mode 
		drop samezip9 samezip9_min zip9_mode

		di _n "NUMBER OF OBS WITH MISSING ZIPCODE"
		count if zip5 == .
		di _n "NUMBER OF OBS WITH MISSING ZIPCODE+4"
		count if zip9 == .
		di _n "TOTAL NUMBER OF OBSERVATIONS"
		count

	***** Flag observations with different account no but same address *****

		di _n "OBSERVATIONS WITH MISSING ADDRESS"
		count if trim(address)==""
		di _n "OBSERVATIONS WITH MISSING COUNTY"
		count if trim(county)==""
		count

		sort address yearquarter employeracc 
		by address yearquarter: gen new_acc_no = employeracc[_n]!=employeracc[_n-1] if trim(address)!=""
		by address yearquarter: egen no_acc_per_address = sum(new_acc_no) if trim(address)!=""

		di _n "NUMBER OF EMPLOYER ACCOUNTS PER ADDRESS"
		tab no_acc_per_address
		di _n "NUMBER OF EMPLOYER ACCOUNTS PER ADDRESS, SINGLE-ESTABLISHMENT FIRMS"
		tab no_acc_per_address if multiest == 0
		di _n "NUMBER OF EMPLOYER ACCOUNTS PER ADDRESS, SINGLE-ESTABLISHMENT FIRMS IN KING COUNTY"
		tab no_acc_per_address if multiest == 0 & (county=="King" | county=="KING" | county=="king")


		* DECISION RULE: FLAG MORE THAN 20 EMPLOYER ACCOUNTS PER ADDRESS
		* CAN REFINE THIS LATER, DROPS ABOUT 1.4% OF THE WORKERS
		gen nonunique_address = no_acc_per_address > 20 if no_acc_per_address < .

		drop new_acc_no no_acc_per_address

		di _n "OBSERVATIONS WITH SAME ADDRESS BUT DIFFERENT ACCOUNT NUMBERS"
		tab nonunique_address
		di _n "SINGLE and MULTI-ESTABLISHMENT OBSERVATIONS WITH SAME ADDRESS BUT DIFFERENT ACCOUNT NUMBERS"
		tab multiest nonunique_address, row

		qui sum
		if `X'>2005 {
			append using ESD_Wages_v4.dta
			}
		save ESD_Wages_v4.dta, replace
		clear
		}
	
	*** NEW DATA FOR 2016Q4, 2017Q1, 2017Q2	
		insheet using F:\Oct2017update\UW-2016Q4.csv, comma
		drop employeefirstname employeename
		tab naicscode if naicscode < 100000
		replace naicscode = naicscode*10 if naicscode < 100000
		gen naics2 = int(naicscode/10000)
	
		destring totalwageamount, replace ignore("$" ",")

     ***** Flag the observations with multi-establishment record ****
		gen multiest = 1 if county == "Multi Master" | county == "MULTI MASTER" | county == "multi master"
		replace multiest = 1 if county == "State-wide" | county == "STATE-WIDE" | county == "state-wide"
		replace multiest = 0 if multiest == .

	***** Flag the observations with unknown location (based on county) ****
		gen unknowncounty = 1 if county == "Unknown" | county == "UNKNOWN" | county == "unknown"
		replace unknowncounty = 1 if length(trim(county)) == 0
		replace unknowncounty = 1 if county == "Out of state" | county == "OUT OF STATE" | county == "out of state"
		replace unknowncounty = 0 if unknowncounty == .

		di _n "OBSERVATIONS WITH NON-WA LOCATION OR UNKNOWN LOCATION"
		tab unknowncounty
		di _n "OBSERVATIONS WITH MULTIPLE LOCATIONS"
		tab multiest
		di _n "OBSERVATIONS WITH MULTIPLE LOCATIONS IN WA"	
		tab multiest if unknowncounty == 0

	***** Check if any addresses have "PO BOX" in them ****
		count if regexm(address,"P\\.O\\.") == 1
		count if regexm(address,"Box") == 1	
		count if regexm(address,"0-90-90-90-90-9") == 1	

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

	***** Correct zipcodes is they are different within employer acc no. ****

		di _n "MISSING ZIPCODE BEFORE CORRECTION"
		count if zipcode == .

	* Fill in missing zip9 codes if zip5 is never missing
		egen zip9_min = min(zip9), by(employeracc)
		egen zip9_max = max(zip9), by(employeracc)
		replace zip9 = zip9_max if zip9 == . & zip9_min == zip9_max & zip5 < .

		drop zip9_min zip9_max

	* If have conflicting zipcode --> replace with the mode of zipcodes
		sort employeracc
		by employeracc: gen samezip5 = zip5[_n]==zip5[_n-1] if _n>1
		by employeracc: replace samezip5 = 1 if _n==1
		di _n "CONFLICTING ZIPCODES (0 = CONFLICT)"
		tab samezip5

		by employeracc: gen samezip9 = zip9[_n]==zip9[_n-1] if _n>1
		by employeracc: replace samezip9 = 1 if _n==1
		di _n "CONFLICTING ZIPCODES+4 (0 = CONFLICT)"
		tab samezip9	 

		egen samezip5_min = min(samezip5), by(employeracc)
		count if samezip5_min == 0
		if `r(N)' < 500 {
			list employeracc zip5 zip9 if samezip5_min == 0
			}

		egen samezip9_min = min(samezip9), by(employeracc)
		count if samezip9_min == 0
		if `r(N)' < 500 {
			list employeracc zip5 zip9 if samezip9_min == 0
			}

	* Change zipcode to the mode of the zipcode for these observations
		egen zip5_mode = mode(zip5) if samezip5_min == 0, by(employeracc) maxmode
		replace zip5 = zip5_mode if samezip5_min == 0

		egen zip9_mode = mode(zip9) if samezip9_min == 0, by(employeracc) maxmode
		replace zip9 = zip9_mode if samezip9_min == 0

		drop samezip5 samezip5_min zip5_mode 
		drop samezip9 samezip9_min zip9_mode

		di _n "NUMBER OF OBS WITH MISSING ZIPCODE"
		count if zip5 == .
		di _n "NUMBER OF OBS WITH MISSING ZIPCODE+4"
		count if zip9 == .
		di _n "TOTAL NUMBER OF OBSERVATIONS"
		count

	***** Flag observations with different account no but same address *****

		di _n "OBSERVATIONS WITH MISSING ADDRESS"
		count if trim(address)==""
		di _n "OBSERVATIONS WITH MISSING COUNTY"
		count if trim(county)==""
		count
		
		sort address yearquarter employeracc 
		by address yearquarter: gen new_acc_no = employeracc[_n]!=employeracc[_n-1] if trim(address)!=""
		by address yearquarter: egen no_acc_per_address = sum(new_acc_no) if trim(address)!=""

		di _n "NUMBER OF EMPLOYER ACCOUNTS PER ADDRESS"
		tab no_acc_per_address
		di _n "NUMBER OF EMPLOYER ACCOUNTS PER ADDRESS, SINGLE-ESTABLISHMENT FIRMS"
		tab no_acc_per_address if multiest == 0
		di _n "NUMBER OF EMPLOYER ACCOUNTS PER ADDRESS, SINGLE-ESTABLISHMENT FIRMS IN KING COUNTY"
		tab no_acc_per_address if multiest == 0 & (county=="King" | county=="KING" | county=="king")

		* DECISION RULE: FLAG MORE THAN 20 EMPLOYER ACCOUNTS PER ADDRESS
		* CAN REFINE THIS LATER, DROPS ABOUT 1.4% OF THE WORKERS
		gen nonunique_address = no_acc_per_address > 20 if no_acc_per_address < .

		drop new_acc_no no_acc_per_address

		di _n "OBSERVATIONS WITH SAME ADDRESS BUT DIFFERENT ACCOUNT NUMBERS"
		tab nonunique_address
		di _n "SINGLE and MULTI-ESTABLISHMENT OBSERVATIONS WITH SAME ADDRESS BUT DIFFERENT ACCOUNT NUMBERS"
		tab multiest nonunique_address, row

		append using ESD_Wages_v4.dta
		save ESD_Wages_v4.dta, replace
		clear
	
		insheet using F:\Oct2017update\UW-2017Q1.csv, comma
		drop employeefirstname employeename
		tab naicscode if naicscode < 100000
		replace naicscode = naicscode*10 if naicscode < 100000
		gen naics2 = int(naicscode/10000)
	
		destring totalwageamount, replace ignore("$" ",")

     ***** Flag the observations with multi-establishment record ****
		gen multiest = 1 if county == "Multi Master" | county == "MULTI MASTER" | county == "multi master"
		replace multiest = 1 if county == "State-wide" | county == "STATE-WIDE" | county == "state-wide"
		replace multiest = 0 if multiest == .

	***** Flag the observations with unknown location (based on county) ****
		gen unknowncounty = 1 if county == "Unknown" | county == "UNKNOWN" | county == "unknown"
		replace unknowncounty = 1 if length(trim(county)) == 0
		replace unknowncounty = 1 if county == "Out of state" | county == "OUT OF STATE" | county == "out of state"
		replace unknowncounty = 0 if unknowncounty == .

		di _n "OBSERVATIONS WITH NON-WA LOCATION OR UNKNOWN LOCATION"
		tab unknowncounty
		di _n "OBSERVATIONS WITH MULTIPLE LOCATIONS"
		tab multiest
		di _n "OBSERVATIONS WITH MULTIPLE LOCATIONS IN WA"	
		tab multiest if unknowncounty == 0

	***** Check if any addresses have "PO BOX" in them ****
		count if regexm(address,"P\\.O\\.") == 1
		count if regexm(address,"Box") == 1	
		count if regexm(address,"0-90-90-90-90-9") == 1	

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

	***** Correct zipcodes is they are different within employer acc no. ****

		di _n "MISSING ZIPCODE BEFORE CORRECTION"
		count if zipcode == .

	* Fill in missing zip9 codes if zip5 is never missing
		egen zip9_min = min(zip9), by(employeracc)
		egen zip9_max = max(zip9), by(employeracc)
		replace zip9 = zip9_max if zip9 == . & zip9_min == zip9_max & zip5 < .

		drop zip9_min zip9_max

	* If have conflicting zipcode --> replace with the mode of zipcodes
		sort employeracc
		by employeracc: gen samezip5 = zip5[_n]==zip5[_n-1] if _n>1
		by employeracc: replace samezip5 = 1 if _n==1
		di _n "CONFLICTING ZIPCODES (0 = CONFLICT)"
		tab samezip5

		by employeracc: gen samezip9 = zip9[_n]==zip9[_n-1] if _n>1
		by employeracc: replace samezip9 = 1 if _n==1
		di _n "CONFLICTING ZIPCODES+4 (0 = CONFLICT)"
		tab samezip9	 

		egen samezip5_min = min(samezip5), by(employeracc)
		count if samezip5_min == 0
		if `r(N)' < 500 {
			list employeracc zip5 zip9 if samezip5_min == 0
			}

		egen samezip9_min = min(samezip9), by(employeracc)
		count if samezip9_min == 0
		if `r(N)' < 500 {
			list employeracc zip5 zip9 if samezip9_min == 0
			}

	* Change zipcode to the mode of the zipcode for these observations
		egen zip5_mode = mode(zip5) if samezip5_min == 0, by(employeracc) maxmode
		replace zip5 = zip5_mode if samezip5_min == 0

		egen zip9_mode = mode(zip9) if samezip9_min == 0, by(employeracc) maxmode
		replace zip9 = zip9_mode if samezip9_min == 0

		drop samezip5 samezip5_min zip5_mode 
		drop samezip9 samezip9_min zip9_mode

		di _n "NUMBER OF OBS WITH MISSING ZIPCODE"
		count if zip5 == .
		di _n "NUMBER OF OBS WITH MISSING ZIPCODE+4"
		count if zip9 == .
		di _n "TOTAL NUMBER OF OBSERVATIONS"
		count

	***** Flag observations with different account no but same address *****

		di _n "OBSERVATIONS WITH MISSING ADDRESS"
		count if trim(address)==""
		di _n "OBSERVATIONS WITH MISSING COUNTY"
		count if trim(county)==""
		count

		sort address yearquarter employeracc 
		by address yearquarter: gen new_acc_no = employeracc[_n]!=employeracc[_n-1] if trim(address)!=""
		by address yearquarter: egen no_acc_per_address = sum(new_acc_no) if trim(address)!=""

		di _n "NUMBER OF EMPLOYER ACCOUNTS PER ADDRESS"
		tab no_acc_per_address
		di _n "NUMBER OF EMPLOYER ACCOUNTS PER ADDRESS, SINGLE-ESTABLISHMENT FIRMS"
		tab no_acc_per_address if multiest == 0
		di _n "NUMBER OF EMPLOYER ACCOUNTS PER ADDRESS, SINGLE-ESTABLISHMENT FIRMS IN KING COUNTY"
		tab no_acc_per_address if multiest == 0 & (county=="King" | county=="KING" | county=="king")


		* DECISION RULE: FLAG MORE THAN 20 EMPLOYER ACCOUNTS PER ADDRESS
		* CAN REFINE THIS LATER, DROPS ABOUT 1.4% OF THE WORKERS
		gen nonunique_address = no_acc_per_address > 20 if no_acc_per_address < .

		drop new_acc_no no_acc_per_address

		di _n "OBSERVATIONS WITH SAME ADDRESS BUT DIFFERENT ACCOUNT NUMBERS"
		tab nonunique_address
		di _n "SINGLE and MULTI-ESTABLISHMENT OBSERVATIONS WITH SAME ADDRESS BUT DIFFERENT ACCOUNT NUMBERS"
		tab multiest nonunique_address, row

		append using ESD_Wages_v4.dta
		save ESD_Wages_v4.dta, replace
		clear
		
		insheet using F:\Oct2017update\UW-2017Q2.csv, comma
		drop employeefirstname employeename
		tab naicscode if naicscode < 100000
		replace naicscode = naicscode*10 if naicscode < 100000
		gen naics2 = int(naicscode/10000)
	
		destring totalwageamount, replace ignore("$" ",")

     ***** Flag the observations with multi-establishment record ****
		gen multiest = 1 if county == "Multi Master" | county == "MULTI MASTER" | county == "multi master"
		replace multiest = 1 if county == "State-wide" | county == "STATE-WIDE" | county == "state-wide"
		replace multiest = 0 if multiest == .

	***** Flag the observations with unknown location (based on county) ****
		gen unknowncounty = 1 if county == "Unknown" | county == "UNKNOWN" | county == "unknown"
		replace unknowncounty = 1 if length(trim(county)) == 0
		replace unknowncounty = 1 if county == "Out of state" | county == "OUT OF STATE" | county == "out of state"
		replace unknowncounty = 0 if unknowncounty == .

		di _n "OBSERVATIONS WITH NON-WA LOCATION OR UNKNOWN LOCATION"
		tab unknowncounty
		di _n "OBSERVATIONS WITH MULTIPLE LOCATIONS"
		tab multiest
		di _n "OBSERVATIONS WITH MULTIPLE LOCATIONS IN WA"	
		tab multiest if unknowncounty == 0

	***** Check if any addresses have "PO BOX" in them ****
		count if regexm(address,"P\\.O\\.") == 1
		count if regexm(address,"Box") == 1	
		count if regexm(address,"0-90-90-90-90-9") == 1	

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

	***** Correct zipcodes is they are different within employer acc no. ****

		di _n "MISSING ZIPCODE BEFORE CORRECTION"
		count if zipcode == .

	* Fill in missing zip9 codes if zip5 is never missing
		egen zip9_min = min(zip9), by(employeracc)
		egen zip9_max = max(zip9), by(employeracc)
		replace zip9 = zip9_max if zip9 == . & zip9_min == zip9_max & zip5 < .

		drop zip9_min zip9_max

	* If have conflicting zipcode --> replace with the mode of zipcodes
		sort employeracc
		by employeracc: gen samezip5 = zip5[_n]==zip5[_n-1] if _n>1
		by employeracc: replace samezip5 = 1 if _n==1
		di _n "CONFLICTING ZIPCODES (0 = CONFLICT)"
		tab samezip5

		by employeracc: gen samezip9 = zip9[_n]==zip9[_n-1] if _n>1
		by employeracc: replace samezip9 = 1 if _n==1
		di _n "CONFLICTING ZIPCODES+4 (0 = CONFLICT)"
		tab samezip9	 

		egen samezip5_min = min(samezip5), by(employeracc)
		count if samezip5_min == 0
		if `r(N)' < 500 {
			list employeracc zip5 zip9 if samezip5_min == 0
			}

		egen samezip9_min = min(samezip9), by(employeracc)
		count if samezip9_min == 0
		if `r(N)' < 500 {
			list employeracc zip5 zip9 if samezip9_min == 0
			}

	* Change zipcode to the mode of the zipcode for these observations
		egen zip5_mode = mode(zip5) if samezip5_min == 0, by(employeracc) maxmode
		replace zip5 = zip5_mode if samezip5_min == 0

		egen zip9_mode = mode(zip9) if samezip9_min == 0, by(employeracc) maxmode
		replace zip9 = zip9_mode if samezip9_min == 0

		drop samezip5 samezip5_min zip5_mode 
		drop samezip9 samezip9_min zip9_mode

		di _n "NUMBER OF OBS WITH MISSING ZIPCODE"
		count if zip5 == .
		di _n "NUMBER OF OBS WITH MISSING ZIPCODE+4"
		count if zip9 == .
		di _n "TOTAL NUMBER OF OBSERVATIONS"
		count

	***** Flag observations with different account no but same address *****

		di _n "OBSERVATIONS WITH MISSING ADDRESS"
		count if trim(address)==""
		di _n "OBSERVATIONS WITH MISSING COUNTY"
		count if trim(county)==""
		count

		sort address yearquarter employeracc 
		by address yearquarter: gen new_acc_no = employeracc[_n]!=employeracc[_n-1] if trim(address)!=""
		by address yearquarter: egen no_acc_per_address = sum(new_acc_no) if trim(address)!=""

		di _n "NUMBER OF EMPLOYER ACCOUNTS PER ADDRESS"
		tab no_acc_per_address
		di _n "NUMBER OF EMPLOYER ACCOUNTS PER ADDRESS, SINGLE-ESTABLISHMENT FIRMS"
		tab no_acc_per_address if multiest == 0
		di _n "NUMBER OF EMPLOYER ACCOUNTS PER ADDRESS, SINGLE-ESTABLISHMENT FIRMS IN KING COUNTY"
		tab no_acc_per_address if multiest == 0 & (county=="King" | county=="KING" | county=="king")


		* DECISION RULE: FLAG MORE THAN 20 EMPLOYER ACCOUNTS PER ADDRESS
		* CAN REFINE THIS LATER, DROPS ABOUT 1.4% OF THE WORKERS
		gen nonunique_address = no_acc_per_address > 20 if no_acc_per_address < .

		drop new_acc_no no_acc_per_address

		di _n "OBSERVATIONS WITH SAME ADDRESS BUT DIFFERENT ACCOUNT NUMBERS"
		tab nonunique_address
		di _n "SINGLE and MULTI-ESTABLISHMENT OBSERVATIONS WITH SAME ADDRESS BUT DIFFERENT ACCOUNT NUMBERS"
		tab multiest nonunique_address, row

		append using ESD_Wages_v4.dta
		save ESD_Wages_v4.dta, replace
		clear
		
	* Create PERSONID and strip off name and SSN
	**Generate person ID
		use "R:\Project\SeattleMinimumWage\Data\State Data\ESD\ESD_Wages_v4.dta"
		sort ssn yearquarter
		gen temp=ssn~=ssn[_n-1]
		gen personid=1
		replace personid=personid[_n-1]+temp if _n>1
		sum personid
		drop ssn temp
	**Generate employer ID	
		sort employeraccountnumber yearquarter
		gen temp=employeraccountnumber~=employeraccountnumber[_n-1]
		gen employerid=1
		replace employerid=employerid[_n-1]+temp if _n>1
		sum employerid
		preserve
			keep employerid employeraccountnumber
			duplicates drop
			save "R:\Project\SeattleMinimumWage\Data\State Data\ESD\crosswalk_Employerid_Wages_v4.dta", replace
		restore
		drop  temp	
	**Generate tax ID	
		sort fein yearquarter
		gen temp=fein~=fein[_n-1]
		gen taxid=1
		replace taxid=taxid[_n-1]+temp if _n>1
		sum taxid
		preserve
			keep taxid fein
			duplicates drop
			save "R:\Project\SeattleMinimumWage\Data\State Data\ESD\crosswalk_Taxid_Wages_v4.dta", replace
		restore
		drop fein temp
	**Create addressid
		egen long address_id = group(address city zip5)	
		replace address_id =0 if address_id==.
		sort address_id
		save "R:\Project\SeattleMinimumWage\Data\State Data\ESD\ESD_Wages_v4.dta", replace
		clear
	}
	

clear
log close

