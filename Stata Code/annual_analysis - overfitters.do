****************************************************************
*Figure 1: Fraction of Price Change Attributable to Oil Prices and Taxes
****************************************************************
clear all
cd "C:\Users\anabe\Documents\Gas project\Web_materials"


use annual_regression_data, clear
xi: reg taxin_gas_price nom_oilprice gas_tax_all i.fips year, cluster(fips)
predict residual1, resid
predict pricehat1, xb
lincom nom_oilprice
gen nom_oilprice_portion1 = r(estimate)*nom_oilprice
lincom gas_tax_all
gen tax_portion1 = r(estimate)*gas_tax_all
lincom year
gen year_portion1 = r(estimate)*(year-1968)
gen state_portion1 = pricehat1 - nom_oilprice_portion1 - tax_portion1 - year_portion1

save annual_regoutput, replace


*graph of portion of national prices explained by each portion
use annual_regoutput, clear
collapse (mean) taxin_gas_price *portion* pricehat*, by(year)
label var taxin_gas_price "Average US Gas Price"

gen nom_oil1 = pricehat1
gen tax1 = pricehat1 - nom_oilprice_portion1
gen state1 = pricehat1 - nom_oilprice_portion1 - tax_portion1
label var nom_oil1 "Crude Oil"
label var tax1 "Gas Taxes"
label var state1 "State FE and Time Trend"

sort year
graph twoway (area nom_oil1 tax1 state1 year),xtitle("Year") ytitle("Cents/Gal") note("Specification includes state FE and a common linear time trend for all states." "State-level data aggregated nationally.") title("Decomposition of U.S. Gasoline Price")
graph save annual_crude_tax_portions1.gph, replace


*************************************************
*Figure 2: Maps of nominal tax increases, '66-'08
*************************************************

use annual_regression_data, clear
keep if year==1966 | year==1987 | year==2008
gsort state year


gen growth6608=round(sgastax[_n]-sgastax[_n-2],.1) if year==2008


drop if year~=2008

spmap growth6608 using Us-Coordinates.dta,  title("State Tax Increase (cpg), 1966 to 2008") fcolor(Blues) clm(custom) clb(0 5 10 15 20 25 30 ) id(id) label(label(growth6608) xcoord(x_coord) ycoord(y_coord))
graph save statetaxgrowth_66_08.gph, replace

********************************************************
*Figure 3: Mean, Median, Max, Min tax rates over time
********************************************************

use annual_regression_data, clear
gen gastax_all=sgastax+fgastax
collapse (mean) sgastax fgastax gastax_all taxin_gas_price  (max) max_gas_tax=sgastax (min) min_gas_tax=sgastax, by(year)

graph twoway (line sgastax year, cmissing(n) legend(label(1 "Mean State Gas Tax"))) (line min_gas_tax year, cmissing(n) legend(label(2 "Min. State Gas Tax")))(line max_gas_tax year, cmissing(n) legend(label(3 "Max. State Gas Tax"))) (line fgastax year, cmissing(n) lp(dash) legend(label(4 "Federal Gas Tax"))), xtitle("Year") ytitle("Cents/gal") title("State and Federal Gasoline Taxes") note("")
graph save annual_gas_tax_dist.gph, replace


********************************************************
*Figure 4: Gasoline Taxes as a Fraction of Retail Prices
********************************************************

*Taxes as a fraction of total annual prices
use annual_regression_data, clear

gen tax_fraction =  (fgastax + sgastax) / taxin_gas_price

sum tax_fraction, detail

collapse (median) median=tax_fraction taxin_gas_price (p95)perc95=tax_fraction (p5) perc5=tax_fraction, by(year)
label var median "Median Tax Fraction"
label var perc95 "95th Percentile"
label var perc5 "5th Percentile"
label var taxin_gas_price "Median Tax-Inclusive Gas Price"

graph twoway (line median year,cmissing(n)) (line perc95 year,cmissing(n)) (line perc5 year,cmissing(n)) (line taxin_gas_price year,cmissing(n) yaxis(2) lp(dash)), legend(size(small)) title("Distribution of Tax Fraction over Time") ytitle("Tax Fraction of Retail Price") xtitle("Year") ytitle("Gasoline Price (cpg)", axis(2))
graph save annual_tax_frac_histogram_overtime.gph, replace


**************************************************
*Figure 5: Change in Gas Consumption, pre- and post-tax
**************************************************

use annual_regression_data, clear

* create regression variables
gen lnvma = ln(vmt_state/apop_adj)
gen lngca = ln(hug/apop_adj)
gen lngpinc = ln(taxin_gas_price) // total gas price
gen exclgp = taxin_gas_price - fgastax - sgastax
gen lngp = ln(exclgp)
gen lntr = ln(1 + (fgastax + sgastax)/exclgp)
sort state year
gen lnvma_l1 = lnvma[_n-1] if state==state[_n-1] & year==year[_n-1] + 1
gen lngca_l1 = lngca[_n-1] if state==state[_n-1] & year==year[_n-1] + 1
gen trend = year - 1966
gen trend2 = trend*trend
gen trend3 = trend^3
gen trend4 = trend^4
gen lnrma = ln(road_mileage/apop_adj)
gen lngspcap = ln(gsp_capita)
gen lncars = ln(no_auto)
gen lntrks = ln(no_truck)
gen lndrivers = ln(lic_driver)
gen lncarscap = ln(no_auto/apop_adj)
gen lntrkscap = ln(no_truck/apop_adj)
gen lndriverscap = ln(lic_driver/apop_adj)
gen lnincpop = ln(realincome_state/pop_state_adj)
xi i.year i.state*trend i.state*trend2

* insure balanced panel
sort state year
gen obs = lnvma!=. & lngp!=. & lntr!=. & state!="" & (state!=state[_n-1] | (state==state[_n-1] & year==year[_n-1] + 1))
by state: egen stateobs = sum(obs)
tab stateobs
keep if stateobs==43

* normalize population weights by US population for corresponding year
sort year state
by year: egen sumapop_adj = sum(apop_adj)
gen normapop_adj = apop_adj/sumapop_adj

* set panel variables
egen statenum = group(state)
tsset statenum year

* dummy variables for increase and decrease in state gas tax
sort state year
gen taxincr = state==state[_n-1] & sgastax>sgastax[_n-1]
gen taxdecr = state==state[_n-1] & sgastax<sgastax[_n-1]
for X in num 1/4: gen lXtaxincr = taxincr[_n-X]==1 & state==state[_n-X]
for X in num 1/4: gen fXtaxincr = taxincr[_n+X]==1 & state==state[_n+X]
for X in num 1/4: gen lXtaxdecr = taxdecr[_n-X]==1 & state==state[_n-X]
for X in num 1/4: gen fXtaxdecr = taxdecr[_n+X]==1 & state==state[_n+X]

* estimation; coefficient estimates copied into excel for plotting
reg lngca lngp f4taxincr f3taxincr f2taxincr f1taxincr taxincr l1taxincr l2taxincr l3taxincr l4taxincr f4taxdecr f3taxdecr f2taxdecr f1taxdecr taxdecr l1taxdecr l2taxdecr l3taxdecr l4taxdecr fsize lnrma lnincpop lncarscap lntrkscap lndriverscap urbanization railpop _Iyear* _Istate* _IstaXtren_* _IstaXtrena*, cluster(state)




***********************************************************
*TABLE 1
***********************************************************

use annual_regression_data, clear

* create regression variables
gen lnvma = ln(vmt_state/apop_adj)
gen lngca = ln(hug/apop_adj)
gen lngpinc = ln(taxin_gas_price)
gen exclgp = taxin_gas_price - fgastax - sgastax
gen lngp = ln(exclgp)
gen lntr = ln(1 + (fgastax + sgastax)/exclgp)
sort state year
gen lnvma_l1 = lnvma[_n-1] if state==state[_n-1] & year==year[_n-1] + 1
gen lngca_l1 = lngca[_n-1] if state==state[_n-1] & year==year[_n-1] + 1
gen trend = year - 1966
gen trend2 = trend*trend
gen trend3 = trend^3
gen trend4 = trend^4
gen lnrma = ln(road_mileage/apop_adj)
gen lngspcap = ln(gsp_capita)
gen lncars = ln(no_auto)
gen lntrks = ln(no_truck)
gen lndrivers = ln(lic_driver)
gen lncarscap = ln(no_auto/apop_adj)
gen lntrkscap = ln(no_truck/apop_adj)
gen lndriverscap = ln(lic_driver/apop_adj)
gen lnincpop = ln(realincome_state/pop_state_adj)
xi i.year i.state*trend i.state*trend2 i.state*trend3 i.state*trend4

* insure balanced panel
sort state year
gen obs = lnvma!=. & lngp!=. & lntr!=. & state!="" & (state!=state[_n-1] | (state==state[_n-1] & year==year[_n-1] + 1))
by state: egen stateobs = sum(obs)
tab stateobs
keep if stateobs==43

* normalize population weights by US population for corresponding year
sort year state
by year: egen sumapop_adj = sum(apop_adj)
gen normapop_adj = apop_adj/sumapop_adj


********************************************************
* TABLE 1: Gasoline Consumption Regressions - Models 1-8
********************************************************

*Column 1
reg lngca lngpinc _Istate* , cluster(state)
di e(r2_a)

*Column 2
reg lngca lngp lntr  _Istate*  , cluster(state)
lincom lngp - lntr
di e(r2_a)

*Column 3
reg lngca lngpinc _Iyear* _Istate* , cluster(state)
di e(r2_a)

*Column 4
reg lngca lngp lntr  _Iyear* _Istate*  , cluster(state)
lincom lngp - lntr
di e(r2_a)

*Column 5
reg lngca lngpinc fsize lnrma lnincpop lncarscap lntrkscap lndriverscap urbanization railpop  _Iyear* _Istate* , cluster(state)
di e(r2_a)

*Column 6
reg lngca lngp lntr fsize lnrma lnincpop lncarscap lntrkscap lndriverscap urbanization railpop _Iyear* _Istate* , cluster(state)
lincom lngp - lntr
di e(r2_a)

*Column 7
reg lngca lngpinc fsize lnrma lnincpop lncarscap lntrkscap lndriverscap urbanization railpop  _Iyear* _Istate* _IstaXtren_* _IstaXtrena* , cluster(state)
di e(r2_a)

*Column 8
reg lngca lngp lntr fsize lnrma lnincpop lncarscap lntrkscap lndriverscap urbanization railpop _Iyear* _Istate* _IstaXtren_* _IstaXtrena* , cluster(state)
lincom lngp - lntr
di e(r2_a)

******************************************************************
* TABLE 2: Coefficient Estimates: Lagged Prices and Taxes (3 lags)
******************************************************************

* 3 lags
sort state year
for X in num 1/3: gen lXlngpinc = lngpinc[_n-X] if state==state[_n-X] & year==year[_n-X] + X
for X in num 1/3: gen lXlngp = lngp[_n-X] if state==state[_n-X] & year==year[_n-X] + X
for X in num 1/3: gen lXlntr = lntr[_n-X] if state==state[_n-X] & year==year[_n-X] + X

* 1) retail gas price
//reg lngca lngpinc l1lngpinc l2lngpinc l3lngpinc fsize lnrma lnincpop lncarscap lntrkscap lndriverscap urbanization railpop _Iyear* _Istate* _IstaXtren_* _IstaXtrena*, cluster(state)
di e(r2_a)

* 2) tax and tax-exclusive gas price
reg lngca lngp l1lngp l2lngp l3lngp lntr l1lntr l2lntr l3lntr fsize lnrma lnincpop lncarscap lntrkscap lndriverscap urbanization railpop _Iyear* _Istate* _IstaXtren_* _IstaXtrena*, cluster(state)
di e(r2_a)

***********************************************************************************
* NEW ANALYSIS 1: TABLE 3: Coefficient Estimates: Lagged Prices and Taxes (10 lags) 
***********************************************************************************
*re-run previous code excluding Table 2

* 10 lags  
sort state year
for X in num 1/10: gen lXlngpinc = lngpinc[_n-X] if state==state[_n-X] & year==year[_n-X] + X
for X in num 1/10: gen lXlngp = lngp[_n-X] if state==state[_n-X] & year==year[_n-X] + X
for X in num 1/10: gen lXlntr = lntr[_n-X] if state==state[_n-X] & year==year[_n-X] + X

* 1) retail gas price
reg lngca lngpinc l1lngpinc l2lngpinc l3lngpinc l4lngpinc l5lngpinc l6lngpinc l7lngpinc l8lngpinc l9lngpinc l10lngpinc fsize lnrma lnincpop lncarscap lntrkscap lndriverscap urbanization railpop _Iyear* _Istate* _IstaXtren_* _IstaXtrena*, cluster(state)
di e(r2_a)

* 2) tax and tax-exclusive gas price
reg lngca lngp l1lngp l2lngp l3lngp l4lngp l5lngp l6lngp l7lngp l8lngp l9lngp l10lngp lntr l1lntr l2lntr l3lntr l4lntr l5lntr l6lntr l7lntr l8lntr l9lntr l10lntr fsize lnrma lnincpop lncarscap lntrkscap lndriverscap urbanization railpop _Iyear* _Istate* _IstaXtren_* _IstaXtrena*, cluster(state)
di e(r2_a)

******************************************************************************************
* NEW ANALYSIS 2: TABLE 4: Long differences method (Log values for consumption, tax and prices)
******************************************************************************************


gen yeargroup="a66_70" if year > 1965 & year < 1971
replace yeargroup="b71_75" if year > 1970 & year < 1976
replace yeargroup="c76_80" if year > 1975 & year < 1981
replace yeargroup="d81_85" if year > 1980 & year < 1986
replace yeargroup="e86_90" if year > 1985 & year < 1991
replace yeargroup="f91_95" if year > 1990 & year < 1996
replace yeargroup="g96_00" if year > 1995 & year < 2001
replace yeargroup="h01_05" if year > 2000 & year < 2006
replace yeargroup="i06_08" if year > 2005 & year < 2011

bysort state yeargroup: egen mean_tax_rate = mean(lntr)
bysort state yeargroup: egen mean_exclgp = mean(lngp)
bysort state yeargroup: egen mean_log_consumption = mean(lngca) // ln(hug/apop_adj)

sort state yeargroup 
*keep one observation/ year group/ state/

quietly by  state yeargroup: gen dup=cond(_N==1,0,_n)
drop if dup>1

********* Taxes lags and differences *********

forval X = 1/8 {
    local l_mean_tax_rate = "l`X'_mean_tax_rate`X'"
	
	by state (yeargroup): gen `l_mean_tax_rate' = mean_tax_rate[_n-`X'] if state == state[_n-`X']
}

forval X = 1/8 {
    local l_mean_tax_rate = "l`X'_mean_tax_rate`X'"
	local difference = "difference_`X'"
	
	by state (yeargroup): gen `difference' = mean_tax_rate - `l_mean_tax_rate' if state == state[_n-`X']
}

********* Tax exclusive price lags and differences *********

forval X = 1/8 {
    local l_mean_exclgp = "l`X'_mean_exclgp`X'"
	
	by state (yeargroup): gen `l_mean_exclgp' = mean_exclgp[_n-`X'] if state == state[_n-`X']	
}

forval X = 1/8 {
    local l_mean_exclgp = "l`X'_mean_exclgp`X'"
	local difference = "difference_exclgp_`X'"
	
	by state (yeargroup): gen `difference' = mean_exclgp - `l_mean_exclgp' if state == state[_n-`X']
}


********* Log Consumption lags and differences *********

forval X = 1/8 {
    local l_mean_log_consumption = "l`X'mean_log_consumption`X'"
	
	by state (yeargroup): gen `l_mean_log_consumption' = mean_log_consumption[_n-`X'] if state == state[_n-`X']
}

forval X = 1/8 {
    local l_mean_log_consumption = "l`X'mean_log_consumption`X'"
	local difference = "difference_log_consumption_`X'"
	
	by state (yeargroup): gen `difference' = mean_log_consumption - `l_mean_log_consumption' if state == state[_n-`X']
}

********* Selecting diagonal of the difference between 1st and other periods

* Generate a new variable diagonal_tax to store diagonal elements
gen tax_diagonal = .
* Loop through each observation and assign diagonal elements to diagonal_tax
forval i = 1/8 {
    by state (yeargroup): replace tax_diagonal = difference_`i' if _n == `i'+1
}


gen exclgp_diagonal = .
forval i = 1/8 {
    by state (yeargroup): replace exclgp_diagonal = difference_exclgp_`i' if _n == `i'+1
}


gen log_consumption_diagonal = .
forval i = 1/8 {
    by state (yeargroup): replace log_consumption_diagonal = difference_log_consumption_`i' if _n == `i'+1
}


* Create variables to store the last row values for each state
//drop tax_diagonal_* exclgp_diagonal_* consumption_diagonal_* 


forvalues i = 2/9{
	local tax_diagonal_i = "tax_diagonal_`i'"
	local exclgp_diagonal_i = "exclgp_diagonal_`i'"
	local log_consumption_diagonal_i = "log_consumption_diagonal_`i'"
	
	gen `tax_diagonal_i' = .
	gen `exclgp_diagonal_i' = .
	gen `log_consumption_diagonal_i' = .
	
	by state (yeargroup): replace `tax_diagonal_i' = tax_diagonal[`i'] if _n == `i'
    by state (yeargroup): replace `exclgp_diagonal_i' = exclgp_diagonal[`i'] if _n == `i'
    by state (yeargroup): replace `log_consumption_diagonal_i' = log_consumption_diagonal[`i'] if _n == `i'
	
	** Regress
	reg log_consumption_diagonal_`i' exclgp_diagonal_`i' tax_diagonal_`i', cluster(state)
	
}


******************** End of the Analysis ******************** 


