** Do institutions affect the expansion of illicit crops? Empirical evidence from Colombia
** UniversitŽ catholique de Louvain
** Santiago Tob—n Zapata
** Original data: institutions_coca.dta / municipal_development_index.dta / health_coverage.dta
** July, 2012

cap restore
clear all
set mem 100m
set more off
set logtype smcl
set matsize 800
pause on

**** WORKING DIRECTORY
********************************************************

cd "/Users/santiagotobon/Documents/UCL/Thesis/Data_Final/"
use "data/institutions_coca.dta", clear

**** DATASET
********************************************************

**** Information for health coverage for social controls

sort codmpio year
merge 1:1 codmpio year using "data/health_coverage.dta"
drop if _merge==2
drop _merge

**** Information for the municipal development index (political institutions)

sort codmpio year
merge 1:1 codmpio year using "data/municipal_development_index.dta"
drop if _merge==2

**** Drop years after 2008

drop if year>2008
	
**** Determining investment per capita in each municipality

foreach i in inv_salud inv_educacion inv_justicia {
gen `i'_cp=`i'/pobla
}

**** Adjusting final variables in proportions

gen ba_nu1000 = ba_nu / pobla * 1000
gen new_hom_rate = homicidio / pobla * 100000
gen rurality_index = pobl_rur / pobla
gen alturakm = altura / 1000
replace htapropietario = htapropietario/10
gen coca_prop1000 = H_coca / areasuphe * 1000
rename D_Coca coca_presence

**** Main vector of covariates for the models

global geo "alturakm dismdo discapital aptitud erosion"
global pol "inv_educacion_cp inv_salud_cp inv_justicia_cp ba_nu1000"
global land "g_uaf htapropietario"
global social "health_coverage rurality_index"

**** Final variables to keep in the dataset

keep coca_presence aptitud erosion discapital rurality_index mdi depto municipio codmpio coddepto year inv_educacion_cp inv_salud_cp inv_justicia_cp ba_nu1000 g_uaf htapropietario health_coverage informalidad new_hom_rate coca_prop1000 alturakm dismdo 

**** Labels

label variable coca_prop1000 "Proportion of coca fields per 1000 hectares"
label variable informalidad "Informality index land property"
label variable new_hom_rate "Homicide rate per 100000 inhabitants"
label variable mdi "Municipal development index"
label variable alturakm "Altitude (km.)"
label variable dismdo "Distance to the nearest market (km.)"
label variable discapital "Distance to the capital of the department (km.)"
label variable aptitud "Suitability of land for farming"
label variable erosion "Soil erosion"
label variable inv_educacion_cp "Public expenditures per capita in education"
label variable inv_salud_cp  "Public expenditures per capita in health"
label variable inv_justicia_cp "Public expenditures per capita in justice"
label variable ba_nu1000 "Number of agricultural loans per 1000 inhabitants"
label variable g_uaf "Land quality gini index"
label variable htapropietario "Number of hectares per landowner"
label variable health_coverage "Health coverage"
label variable rurality_index "Proportion of people living in rural areas"
label variable coca_presence "Presence of coca fields (yes==1)"

**** Dropping municipalities with inconsistent data on public expenditures

drop if codmpio==85315 | codmpio==52287 | codmpio==8372 | codmpio==8675 | codmpio==19450 | codmpio==52699 | codmpio==13657 | codmpio==8078 | codmpio==52621 | codmpio==52473 | codmpio==52520 | codmpio==52019 | codmpio==13062 | codmpio==19513 | codmpio==13074 | codmpio==13667 | codmpio==50150 | codmpio==13160 | codmpio==73347 | codmpio==8558 | codmpio==70400 | codmpio==41349 | codmpio==41206 | codmpio==47980 | codmpio==85230 | codmpio==50110

**** Filling missing data with departamental and national means

foreach i in coca_prop1000 informalidad mdi new_hom_rate $geo $land $pol $social {
bys codmpio: egen mpio_`i'=mean(`i')
bys coddepto: egen dpto_`i'=mean(`i')
egen n_`i'=mean(`i')
replace `i'= mpio_`i' if `i'==.
replace `i'= dpto_`i' if `i'==.
replace `i'=  n_`i' if `i'==.
drop mpio_`i' dpto_`i'  n_`i' 
}

**** Declaring the dataset and creating time dummies

sort codmpio year
xtset codmpio year
xi i.year

***** SUMMARY STATISTICS
********************************************************

**** Summary statistics by year (mean and st. dev) for time-varying variables

sort year
eststo clear
by year: eststo: quietly estpost summarize coca_prop1000 informalidad mdi new_hom_rate $pol $land $social, listwise
esttab using "summary_stats/sum_by_year_tvar.tex", cells(mean(fmt(%9.3f)) sd(fmt(%9.3f)par([ ])) ) label nodepvar title("Summary statistics by year for time-varying variables") nonum replace addnotes("Mean and [Standard deviation] of municipal characteristics") collabels(none)

**** Summary statistics (mean and st. dev) for time-invariant variables

eststo clear
eststo: quietly estpost summarize $geo, listwise
esttab using "summary_stats/sum_tinv.tex", cells(mean(fmt(%9.3f)) sd(fmt(%9.3f)par([ ])) ) label nodepvar title("Summary statistics for time-invariant variables") nonum replace addnotes("Mean and [Standard deviation] of municipal characteristics") collabels(none) nomtitles

**** Transition probabilities on the presence of coca fiels

xttrans2 coca_presence, prob matcell(T)
outtable using "summary_stats/tab_coca_presence", caption("Transition probabilities on the presence of coca fields (yes==1)") nobox center mat(T) format(%9.2f) replace

**** Tabulations on the presence of coca fields

xttab coca_presence
matrix xttab_results = r(results)
outtable using "summary_stats/tabul_coca_presence", caption("Tabulations on the presence of coca fields (yes==1)") nobox center mat(xttab_results) format(%9.2f) replace

**** Correlations

eststo clear
quietly estpost correlate coca_prop1000 informalidad new_hom_rate mdi $geo $pol $land $social, matrix
esttab using "summary_stats/correlations.tex", b(%9.3f) not unstack compress noobs star(* 0.10 ** 0.05 *** 0.01) replace label title("Correlation coefficients") nomtitles nonum

**** Linear fit

xtile inf_q=informalidad, nq(100)
twoway lfitci coca_prop1000 inf_q, clcolor(gs1) plotregion(fcolor(white)) graphregion(fcolor(white)) ytitle("Proportion of coca fields per 1000 hectares") xtitle("100 quantiles of informalty index land property")
graph export "graphs/l_fit.pdf", width(3000) replace
window manage close graph
drop inf_q

**** Between and within variation

matrix xtsum_results = J(36,5,1)
matrix colnames xtsum_results = Mean SD Min Max N/n/T
matrix rownames xtsum_results = overall between within overall between within overall between within overall between within overall between within overall between within overall between within overall between within overall between within overall between within overall between within overall between within
local j = 1

foreach i in coca_prop1000 informalidad mdi new_hom_rate $pol $land $social{
quietly xtsum  `i'
*first row
matrix xtsum_results[`j',1] = r(mean)
matrix xtsum_results[`j',2] = r(sd)
matrix xtsum_results[`j',3] = r(min)
matrix xtsum_results[`j',4] = r(max)
matrix xtsum_results[`j',5] = r(N)
local j = `j' + 1 
*second row
matrix xtsum_results[`j',1] = r(mean_b)
matrix xtsum_results[`j',2] = r(sd_b)
matrix xtsum_results[`j',3] = r(min_b)
matrix xtsum_results[`j',4] = r(max_b)
matrix xtsum_results[`j',5] = r(n)
local j = `j' + 1 
*third row
matrix xtsum_results[`j',1] = r(mean_w)
matrix xtsum_results[`j',2] = r(sd_w)
matrix xtsum_results[`j',3] = r(min_w)
matrix xtsum_results[`j',4] = r(max_w)
matrix xtsum_results[`j',5] = r(Tbar)
local j = `j' + 1 
}

outtable using "summary_stats/xtsum_results", caption("Between and within variations") nobox mat(xtsum_results) format(%9.4f %9.4f %9.4f %9.4f %9.0f) replace

**** Sort again for regressions

sort codmpio year

**** Re-scaling public expenditures variables (for the sake of visibility in significant results)

foreach i in inv_salud_cp inv_educacion_cp inv_justicia_cp {
replace `i'=`i'/100000
}

***** REGRESSIONS SYSTEM GMM TWOSTEP ESTIMATOR
********************************************************

**** ECONOMIC INSTITUTIONS (Informality index land property)

**** 1. Model without violence

eststo clear
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 .) collapse) iv(informalidad _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad $geo _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 .) collapse) iv(informalidad $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad $geo $pol _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 .) collapse) iv(informalidad $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad $geo $pol $land _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 .) collapse) iv(informalidad $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad $geo $pol $land $social _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 .) collapse) iv(informalidad $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
esttab using "results/1907_Model_1.tex", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N N_clust n_inst Ftest ab_ar1 ab_ar2 han difs_h1 difs_h2, fmt(%9.0f %9.0f %9.0f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f) labels("No. of observations" "No. of groups" "No. of instruments" "p-value F test of joint significance" "p-value Arellano-Bond test for AR(1) in first differences" "p-value Arellano-Bond test for AR(2) in first differences" "p-value Hansen J test of overidentifying restrictions" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (unrestricted)" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (difference)")) addnotes("Two-step System GMM estimator. First difference instruments transformation" "Windmeijer-corrected cluster-robust errors" "Collapsed lags (2-.) used as instruments for endogenous variables") drop(_cons) order(informalidad "Geographic controls" $geo "Political controls" $pol "Land controls" $land "Social controls" $social "Lagged dependent variable") indicate("Time dummies=_Iy*") title("Dynamic panel data results")

**** 2. Model with violence not instrumented
 
eststo clear
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 .) collapse) iv(informalidad new_hom_rate _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate $geo _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 .) collapse) iv(informalidad new_hom_rate $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate $geo $pol _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 .) collapse) iv(informalidad new_hom_rate $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate $geo $pol $land _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 .) collapse) iv(informalidad new_hom_rate $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate $geo $pol $land $social _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 .) collapse) iv(informalidad new_hom_rate $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
esttab using "results/1907_Model_2.tex", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N N_clust n_inst Ftest ab_ar1 ab_ar2 han difs_h1 difs_h2, fmt(%9.0f %9.0f %9.0f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f) labels("No. of observations" "No. of groups" "No. of instruments" "p-value F test of joint significance" "p-value Arellano-Bond test for AR(1) in first differences" "p-value Arellano-Bond test for AR(2) in first differences" "p-value Hansen J test of overidentifying restrictions" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (unrestricted)" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (difference)")) addnotes("Two-step System GMM estimator. First difference instruments transformation" "Windmeijer-corrected cluster-robust errors" "Collapsed lags (2-.) used as instruments for endogenous variables") drop(_cons) order(informalidad new_hom_rate "Geographic controls" $geo "Political controls" $pol "Land controls" $land "Social controls" $social "Lagged dependent variable") indicate("Time dummies=_Iy*") title("Dynamic panel data results controlling for violence")

**** 3. Model with violence instrumented

eststo clear
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(informalidad _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(informalidad $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo $pol _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(informalidad $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo $pol $land _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(informalidad $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo $pol $land $social _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(informalidad $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
esttab using "results/1907_Model_3.tex", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N N_clust n_inst Ftest ab_ar1 ab_ar2 han difs_h1 difs_h2, fmt(%9.0f %9.0f %9.0f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f) labels("No. of observations" "No. of groups" "No. of instruments" "p-value F test of joint significance" "p-value Arellano-Bond test for AR(1) in first differences" "p-value Arellano-Bond test for AR(2) in first differences" "p-value Hansen J test of overidentifying restrictions" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (unrestricted)" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (difference)")) addnotes("Two-step System GMM estimator. First difference instruments transformation" "Windmeijer-corrected cluster-robust errors" "Collapsed lags (2-.) used as instruments for endogenous variables") drop(_cons) order(informalidad new_hom_rate "Geographic controls" $geo "Political controls" $pol "Land controls" $land "Social controls" $social "Lagged dependent variable") indicate("Time dummies=_Iy*") title("Dynamic panel data results controlling for violence instrumented")

**** ROBUSTNESS - POLITICAL INSTITUTIONS (Municipal development index)

**** Model with violence instrumented

eststo clear
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(mdi _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate $geo _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(mdi $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate $geo $pol _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(mdi $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate $geo $pol $land _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(mdi $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate $geo $pol $land $social _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(mdi $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
esttab using "results/1907_Model_R3.tex", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N N_clust n_inst Ftest ab_ar1 ab_ar2 han difs_h1 difs_h2, fmt(%9.0f %9.0f %9.0f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f) labels("No. of observations" "No. of groups" "No. of instruments" "p-value F test of joint significance" "p-value Arellano-Bond test for AR(1) in first differences" "p-value Arellano-Bond test for AR(2) in first differences" "p-value Hansen J test of overidentifying restrictions" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (unrestricted)" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (difference)")) addnotes("Two-step System GMM estimator. First difference instruments transformation" "Windmeijer-corrected cluster-robust errors" "Collapsed lags (2-.) used as instruments for endogenous variables") drop(_cons) order(mdi new_hom_rate "Geographic controls" $geo "Political controls" $pol "Land controls" $land "Social controls" $social "Lagged dependent variable") indicate("Time dummies=_Iy*") title("Robustness check - Dynamic panel data results controlling for violence instrumented")



**** CAUSALITY TESTS
********************************************************

**** 1. informality -> coca plantations

eststo clear
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad l.informalidad _Iy*, twostep robust  gmm(l.coca_prop1000, lag(2 .) collapse) iv(informalidad l.informalidad _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
test informalidad l.informalidad
estadd scalar causality = r(p)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad l.informalidad l2.informalidad _Iy*, twostep robust  gmm(l.coca_prop1000, lag(2 .) collapse) iv(informalidad l.informalidad l2.informalidad _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
test informalidad l.informalidad l2.informalidad
estadd scalar causality = r(p)
esttab using "results/0608_Causality_1.tex", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N N_clust n_inst Ftest ab_ar1 ab_ar2 han difs_h1 difs_h2 causality, fmt(%9.0f %9.0f %9.0f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f) labels("No. of observations" "No. of groups" "No. of instruments" "p-value F test of joint significance" "p-value Arellano-Bond test for AR(1) in first differences" "p-value Arellano-Bond test for AR(2) in first differences" "p-value Hansen J test of overidentifying restrictions" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (unrestricted)" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (difference)" "p-value F test Granger causality")) addnotes("Two-step System GMM estimator. First difference instruments transformation" "Windmeijer-corrected cluster-robust errors" "Collapsed lags (2-.) used as instruments for endogenous variables") drop(_cons) order(l.coca_prop1000 informalidad l.informalidad l2.informalidad) indicate("Time dummies=_Iy*") title("System GMM estimation for Granger causality test")

**** 2. coca plantations -> informality

eststo clear
eststo: xtabond2 informalidad l.informalidad coca_prop1000 l.coca_prop1000 _Iy*, twostep robust  gmm(l.informalidad, lag(2 .) collapse) iv(coca_prop1000 l.coca_prop1000 _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
test coca_prop1000 l.coca_prop1000
estadd scalar causality = r(p)
eststo: xtabond2 informalidad l.informalidad coca_prop1000 l.coca_prop1000 l2.coca_prop1000 _Iy*, twostep robust  gmm(l.informalidad, lag(2 .) collapse) iv(coca_prop1000 l.coca_prop1000 l2.coca_prop1000 _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
test coca_prop1000 l.coca_prop1000 l2.coca_prop1000
estadd scalar causality = r(p)
esttab using "results/0608_Causality_2.tex", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N N_clust n_inst Ftest ab_ar1 ab_ar2 han difs_h1 difs_h2 causality, fmt(%9.0f %9.0f %9.0f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f) labels("No. of observations" "No. of groups" "No. of instruments" "p-value F test of joint significance" "p-value Arellano-Bond test for AR(1) in first differences" "p-value Arellano-Bond test for AR(2) in first differences" "p-value Hansen J test of overidentifying restrictions" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (unrestricted)" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (difference)" "p-value F test Granger causality")) addnotes("Two-step System GMM estimator. First difference instruments transformation" "Windmeijer-corrected cluster-robust errors" "Collapsed lags (2-.) used as instruments for endogenous variables") drop(_cons) order(l.informalidad coca_prop1000 l.coca_prop1000 l2.coca_prop1000) indicate("Time dummies=_Iy*") title("System GMM estimation for Granger causality test")

**** GRAPHS FOR THE INTRODUCTION

clear

**** WORKING DIRECTORY
********************************************************

cd "/Users/santiagotobon/Documents/UCL/Thesis/Data_Final/"
use "data/coca_andean_region.dta", clear

graph twoway (connected total_area_thousands year if country=="Bolivia", lcolor(gs8) mcolor(gs1) msymbol(o)) (connected total_area_thousands year if country=="Colombia", lcolor(gs8) mcolor(gs1) msymbol(d)) (connected total_area_thousands year if country=="Peru", lcolor(gs8) mcolor(gs1) msymbol(t)), ytitle("Thousands of hectares") plotregion(fcolor(white)) graphregion(fcolor(white)) legend(lab(1 "Bolivia") lab(2 "Colombia") lab(3 "Peru"))
graph export "graphs/coca_andean_region.pdf", width(3000) replace
window manage close graph
