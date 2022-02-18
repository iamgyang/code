// 0. Preliminaries

clear all 
set more off
set varabbrev off
set scheme s1mono
set type double, perm

// CHANGE THIS!! --- Define your own directories:
global root        "C:/Users/gyang/Dropbox/CGD/Projects/wb-award-database"
global code        "$root/code"
global input       "$root/input"
global output      "$root/output"
global raw         "$root/raw_data"

// CHANGE THIS!! --- Do we want to install user-defined functions?
loc install_user_defined_functions "No"

if ("`install_user_defined_functions'" == "Yes") {
	foreach i in rangestat wbopendata kountry mmerge outreg2 somersd ///
	asgen moss reghdfe ftools fillmissing eventdd matsort ranktest ivreg2 sepscatter gtools ///
	ivreghdfe{
		capture ssc install `i'
	}
}

// =====================================================
// START CODE ==========================================
// =====================================================

import delimited "$raw/Major_Contract_Awards.csv", delimiter(comma) bindquote(strict) varnames(1) clear

// get a list of countries and their codes:
preserve
keep suppliercountrycode suppliercountry
gduplicates drop
foreach i of numlist 1/20 {
	replace suppliercountry = subinstr(suppliercountry, "  ", " ",.)    
}
replace suppliercountry = strtrim(suppliercountry)
sort suppliercountrycode
gduplicates drop suppliercountrycode, force
tempfile codes_
save `codes_'
restore

// Sort on country codes to exclude any where there isn’t a country code for both borrower and contractor
drop if mi(borrowercountrycode) | mi(suppliercountrycode)
foreach i in "1W" "3T" {
    drop if borrowercountrycode == "`i'" | suppliercountrycode == "`i'"
}

// Exclude all cases where borrower country code is equal to contractor country code (leaving us with contracts won by non-borrower-country firms)
drop if borrowercountrycode == suppliercountrycode

// generate new variable that merges procurement categories
gen procurementcategory2 = procurementcategory
replace procurementcategory2 = "Other" if procurementcategory == "Non-consulting Services"
replace procurementcategory2 = "Other" if procurementcategory == "Consultant Services"

// group time periods together
drop if fiscalyear == 2022
gen     fy_group = "2001-2004" if inlist(fiscalyear, 2001, 2002, 2003, 2004)
replace fy_group = "2005-2009" if inlist(fiscalyear, 2005, 2006, 2007, 2008, 2009)
replace fy_group = "2010-2014" if inlist(fiscalyear, 2010, 2011, 2012, 2013, 2014)
replace fy_group = "2015-2019" if inlist(fiscalyear, 2015, 2016, 2017, 2018, 2019)
replace fy_group = "2020-2021" if inlist(fiscalyear, 2020, 2021)
assert !mi(fy_group)

// generate an "overall" category for procurement
gen count_v = _n
summ count_v
loc max_count = `r(max)'
drop count_v
save "$input/temp1.dta", replace
append using "$input/temp1.dta"
gen count_v = _n
replace procurementcategory2 = "Overall" if count_v > `max_count'
save "$input/temp1.dta", replace
levelsof procurementcategory2, local(procurementcategory2)

// check_dup_id "procurementcategory2 fiscalyear wbcontractnumber totalcontractamountusd borrowercountrycode contractdescription contractsigningdate suppliercountrycode"
// bys fiscalyear wbcontractnumber totalcontractamountusd borrowercountrycode contractdescription contractsigningdate suppliercountrycode: gen n = _N

// Calculate *contractor* (not borrower) country share of total contract values by year over time
// Calculate contractor country share of goods contract values by year over time
// Calculate contractor country share of civil works contract values by year over time
// Calculate contractor country share of [everything that isn’t goods and civil works] contract values by year over time

// create table to store output
clear
set obs 1
gen temp = "N/A"
tempfile base
save `base'

// for each of the groups of procurement category, go through this loop:
foreach i in `procurementcategory2' {
	use "$input/temp1.dta", clear
	keep if procurementcategory2 == "`i'"
	bys fy_group: gegen tot_wb_contract_value = sum(totalcontractamountusd)
	bys suppliercountrycode fy_group: gegen tot_suppl_contract_value = sum(totalcontractamountusd)
	gen supplier_country_share = tot_suppl_contract_value / tot_wb_contract_value

	// check that our shares add up to 1.
	keep supplier_country_share fy_group suppliercountrycode procurementcategory2 tot_suppl_contract_value tot_wb_contract_value
	gduplicates drop
	bys fy_group: gegen check = sum(supplier_country_share)
	assert abs(check - 1) < 0.0001
	drop check
	
	// save file
	append using `base'
	save `base', replace
}

clear
use `base'
drop if temp == "N/A"
drop temp

// erase the temporary file
noisily capture erase "$input/temp1.dta"

// merge back in the codes:
mmerge suppliercountrycode using `codes_'
drop if _merge == 2

// export to R for graphing
save "$input/output.dta", replace

// Produce graphs tracking share of US, Japan, China, Germany, UK, France, 
// India, Russia, Canada, Italy in 4/5/6/7 above over time



