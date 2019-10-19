clear
set more off
set mem 10m

cd "C:\Users\mikew\Desktop\University\TilburgUniversity\Master\panel_data_analysis\assignment_1"
global data "data"
global output "output"

// Load data
use "$data\soep.dta", clear

// Label variables
label variable s_life "life satisfaction, general"
label variable s_health "health satisfaction"
label variable s_work "job satisfaction"
label variable s_housework "housework satisfaction"
label variable s_fin "income satisfaction"
label variable s_house "dwelling satisfaction"
label variable s_leisure "leisure satisfaction"
label variable s_env "environment satisfaction"
label variable s_std "standard living satisfaction"

label variable yedu "years education"
label variable couple "living as couple"
label variable work "paid work"

label variable lndoctor "log doc visits"
label variable degreehandic "degree handicapped"
label variable hhincome "household income"

// Declare this data to be panel data
xtset persnr year

// Exercise 1
// a
gen cohort = year - age
label variable cohort "cohort (birth year)"

// Grouping 5 cohorts together
forvalues i = 1914(5)1963 {
	by age, sort: egen mean_s_life_`i'_c5 = ///
		mean(cond(inrange(cohort, `i', `i'+4), s_life, .))
}

graph twoway line mean_s_life_1914_c5 age|| line mean_s_life_1919_c5 age ///
	|| line mean_s_life_1924_c5 age || line mean_s_life_1929_c5 age ///
	|| line mean_s_life_1934_c5 age || line mean_s_life_1939_c5 age ///
	|| line mean_s_life_1944_c5 age || line mean_s_life_1949_c5 age ///
	|| line mean_s_life_1954_c5 age || line mean_s_life_1959_c5 age, ///
	legend(order(1 "1914-1918" 2 "1919-1923" ///
	3 "1924-1928" 4 "1929-1933" 5 "1934-1938" ///
	6 "1939-1943" 7 "1944-1948" 8 "1949-1953" ///
	9 "1954-1958"  10 "1959-1963") subtitle("Cohort (birth year)")) ///
	title("Life satisfaction versus age per cohort") ///
	ytitle("Mean life satisfaction") graphregion(color(white))

gr export "$output/spaghetti_plot_5_cohorts.eps", as(eps) preview(off) replace
	
// Grouping 10 cohorts together
forvalues i = 1914(10)1963 {
	by age, sort: egen mean_s_life_`i'_c10 = ///
		mean(cond(inrange(cohort, `i', `i'+9), s_life, .))
}

graph twoway line mean_s_life_1914_c10 age|| line mean_s_life_1924_c10 age ///
	|| line mean_s_life_1934_c10 age || line mean_s_life_1944_c10 age ///
	|| line mean_s_life_1954_c10 age, ///
	legend(order(1 "1914-1923"  2 "1924-1933" 3 "1934-1943" ///
	4 "1944-1953" 5 "1954-1963") subtitle("Cohort")) ///
	title("Life satisfaction versus age per cohort") ///
	ytitle("Mean life satisfaction") graphregion(color(white))

gr export "$output/spaghetti_plot_10_cohorts.eps", as(eps) preview(off) replace
