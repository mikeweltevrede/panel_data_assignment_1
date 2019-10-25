clear
est clear
set more off
set mem 10m

cd "C:\Users\mikew\Desktop\University\TilburgUniversity\Master\panel_data_analysis\linear_models\assignment_1"
global datapath "data"
global output "output"

// Load data
use "$datapath\soep.dta", clear

// Declare this data to be panel data
xtset persnr year

// Exercise 1a
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

// Exercise 1c
// Create dummies for cohorts 
forvalues i = 1914(5)1963 {
	gen cohort_`i'_c5 = cohort>=`i' & cohort<=`i'+4
}

forvalues i = 1914(10)1963 {
	gen cohort_`i'_c10 = cohort>=`i' & cohort<=`i'+9
}

// Create macro
global cohort_5_dummies "cohort_1914_c5 cohort_1919_c5 cohort_1924_c5 cohort_1929_c5 cohort_1934_c5 cohort_1939_c5 cohort_1944_c5 cohort_1949_c5 cohort_1954_c5 cohort_1959_c5"
global cohort_10_dummies "cohort_1914_c10 cohort_1924_c10 cohort_1934_c10 cohort_1944_c10 cohort_1954_c10"

// Run the regression
reg s_life age year $cohort_5_dummies $cohort_10_dummies // all dummies
reg s_life age year $cohort_5_dummies
reg s_life age year $cohort_10_dummies

// Exercise 2
clear
set seed 345398
drawnorm alpha_i, n(200)
expand 5

drawnorm nu_it e_it, n(1000)
gen x_it = nu_it + alpha_i
drop nu_it
gen y_it = 3 + alpha_i + 2*x_it + e_it

// 2b
pwcorr,sig

// Exercise 3
clear
set seed 345398
capture program drop mcprog
program mcprog
	clear
	drawnorm alpha_i, n(200)
	expand 5
	
	drawnorm nu_it e_it, n(1000)
	gen x_it = nu_it + alpha_i
	drop nu_it
	gen y_it = 3 + alpha_i + 2*x_it + e_it
	
	regress y_it x_it
end
simulate _b _se, reps(100): mcprog
sum

// a
clear
set seed 345398
capture program drop mcprog_cluster
program mcprog_cluster
	clear
	drawnorm alpha_i, n(200)
	egen persnr = seq(), f(1) t(200)
	expand 5
	
	drawnorm nu_it e_it, n(1000)
	gen x_it = nu_it + alpha_i
	drop nu_it
	gen y_it = 3 + alpha_i + 2*x_it + e_it
	
	regress y_it x_it, cluster(persnr)
end
simulate _b _se, reps(100): mcprog_cluster
sum

// Exercise 4
// Fixed Effects
clear
set seed 345398
capture program drop mc_fixed_effects
program mc_fixed_effects
	clear
	drawnorm alpha_i, n(200)
	egen persnr = seq(), f(1) t(200)
	expand 5
	bysort persnr: gen t = _n-1
	
	drawnorm nu_it e_it, n(1000)
	gen x_it = nu_it + alpha_i
	drop nu_it
	gen y_it = 3 + alpha_i + 2*x_it + e_it
	
	xtset persnr t
	
	xtreg y_it x_it, i(persnr) fe
end
simulate _b _se, reps(100): mc_fixed_effects
sum

// First Difference
clear
set seed 345398
capture program drop mc_first_difference
program mc_first_difference
	clear
	drawnorm alpha_i, n(200)
	egen persnr = seq(), f(1) t(200)
	expand 5
	bysort persnr: gen t = _n-1
	
	drawnorm nu_it e_it, n(1000)
	gen x_it = nu_it + alpha_i
	drop nu_it
	gen y_it = 3 + alpha_i + 2*x_it + e_it
	
	xtset persnr t
	xtreg d.y_it d.x_it
end
simulate _b _se, reps(100): mc_first_difference
sum

// Exercise 5
// For t=5
clear
set seed 345398
capture program drop mc_lag_5
program mc_lag_5
	clear
	drawnorm alpha_i, n(200)
	egen persnr = seq(), f(1) t(200)
	expand 5
	bysort persnr: gen t = _n-1
	
	drawnorm nu_it e_it, n(1000)
	gen x_it = nu_it + alpha_i
	drop nu_it
	gen y_it = 3 + alpha_i + 2*x_it + e_it
	replace y_it = 3 + alpha_i + 2*x_it + e_it + 0.5*y_it[_n-1] if t!=0
	gen y_it_lag_1 = y_it[_n-1]
	
	xtset persnr t
	xtreg y_it x_it y_it_lag_1, fe
end

simulate _b _se, reps(100): mc_lag_5
gen bias_lag = _b_y_it_lag_1 - 0.5
sum bias_lag

// For t=10
clear
set seed 345398
capture program drop mc_lag_10
program mc_lag_10
	clear
	drawnorm alpha_i, n(200)
	egen persnr = seq(), f(1) t(200)
	expand 10
	bysort persnr: gen t = _n-1
	
	drawnorm nu_it e_it, n(2000)
	gen x_it = nu_it + alpha_i
	drop nu_it
	gen y_it = 3 + alpha_i + 2*x_it + e_it
	replace y_it = 3 + alpha_i + 2*x_it + e_it + 0.5*y_it[_n-1] if t!=0
	gen y_it_lag_1 = y_it[_n-1]
	
	xtset persnr t
	xtreg y_it x_it y_it_lag_1, fe
end

simulate _b _se, reps(100): mc_lag_10
gen bias_lag = _b_y_it_lag_1 - 0.5
sum bias_lag

// For t=20
clear
set seed 345398
capture program drop mc_lag_20
program mc_lag_20
	clear
	drawnorm alpha_i, n(200)
	egen persnr = seq(), f(1) t(200)
	expand 20
	bysort persnr: gen t = _n-1
	
	drawnorm nu_it e_it, n(4000)
	gen x_it = nu_it + alpha_i
	drop nu_it
	gen y_it = 3 + alpha_i + 2*x_it + e_it
	replace y_it = 3 + alpha_i + 2*x_it + e_it + 0.5*y_it[_n-1] if t!=0
	gen y_it_lag_1 = y_it[_n-1]
	
	xtset persnr t
	xtreg y_it x_it y_it_lag_1, fe
end

simulate _b _se, reps(100): mc_lag_20
gen bias_lag = _b_y_it_lag_1 - 0.5
sum bias_lag

// For t=50
clear
set seed 345398
capture program drop mc_lag_50
program mc_lag_50
	clear
	drawnorm alpha_i, n(200)
	egen persnr = seq(), f(1) t(200)
	expand 50
	bysort persnr: gen t = _n-1
	
	drawnorm nu_it e_it, n(10000)
	gen x_it = nu_it + alpha_i
	drop nu_it
	gen y_it = 3 + alpha_i + 2*x_it + e_it
	replace y_it = 3 + alpha_i + 2*x_it + e_it + 0.5*y_it[_n-1] if t!=0
	gen y_it_lag_1 = y_it[_n-1]
	
	xtset persnr t
	xtreg y_it x_it y_it_lag_1, fe
end

simulate _b _se, reps(100): mc_lag_50
gen bias_lag = _b_y_it_lag_1 - 0.5
sum bias_lag

// Exercise 6
clear
set seed 345398
capture program drop mc_arellano_bond
program mc_arellano_bond
	clear
	drawnorm alpha_i, n(200)
	egen persnr = seq(), f(1) t(200)
	expand 5
	bysort persnr: gen t = _n-1
	
	drawnorm nu_it e_it, n(1000)
	gen x_it = nu_it + alpha_i
	drop nu_it
	gen y_it = 3 + alpha_i + 2*x_it + e_it
	replace y_it = 3 + alpha_i + 2*x_it + e_it + 0.5*y_it[_n-1] if t!=0
	gen y_it_lag_1 = y_it[_n-1]
	
	xtset persnr t
	xtabond y_it x_it y_it_lag_1
end

simulate _b _se, reps(100): mc_arellano_bond
sum
