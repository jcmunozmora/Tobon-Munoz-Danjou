**** TIME SERIES PROPERTIES

**** Harris-Tzavalis Unit Root test

matrix ht_results = J(12,3,1)
matrix colnames ht_results = p_value p_value_trend Time_periods
matrix rownames ht_results = coca_prop1000 informalidad mdi new_hom_rate $pol $land $social
local h = 1

foreach i in coca_prop1000 informalidad mdi new_hom_rate $pol $land $social{
xtunitroot ht `i', demean altt
matrix ht_results[`h',1] = r(p)
xtunitroot ht `i', demean altt trend
matrix ht_results[`h',2] = r(p)
matrix ht_results[`h',3] = r(N_t)
local h = `h' + 1 
}

outtable using "summary_stats/ht_results", caption("Harris-Tzavalis unit root test") nobox mat(ht_results) format(%9.4f %9.4f %9.0f) replace

foreach i in coca_prop1000 informalidad mdi new_hom_rate $pol $land $social{
xtunitroot ht `i', demean
}

**** UNIT ROOT TESTS

**** Breitung test

xtunitroot breitung coca_prop1000, lags(1) demean

**** Im-Pesaran-Shin test (because Breitung assumes that all panels have the same value of rho)

xtunitroot ips coca_prop1000, lags(1) demean

**** INTERVAL FOR THE COEFFICIENT OF THE LAG OF THE DEPENDENT VARIABLE

gen new_hom_rateL2 = l2.new_hom_rate

foreach i in coca_prop1000 informalidad mdi new_hom_rate $geo $land $pol $social {
xtunitroot breitung `i', lags(1) demean
}

foreach i in coca_prop1000 informalidad mdi new_hom_rate $land $pol $social {
xtunitroot ips `i', lags(1) demean
}

foreach i in coca_prop1000 informalidad mdi new_hom_rate $land $pol $social {
xtunitroot breitung `i', lags(1)
}

foreach i in coca_prop1000 informalidad mdi new_hom_rate $land $pol $social {
xtunitroot ips `i', lags(2) demean
}

*OTHER TESTS

****************** TESTS GOODNESS OF FIT ************************

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
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad $geo _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 .) collapse) iv(informalidad $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad $geo $pol _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 .) collapse) iv(informalidad $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad $geo $pol $land _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 .) collapse) iv(informalidad $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad $geo $pol $land $social _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 .) collapse) iv(informalidad $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
esttab using "results/2807_Model_1.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N N_clust n_inst Ftest ab_ar1 ab_ar2 han difs_h1 difs_h2 pseudoR2, fmt(%9.0f %9.0f %9.0f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f) labels("No. of observations" "No. of groups" "No. of instruments" "p-value F test of joint significance" "p-value Arellano-Bond test for AR(1) in first differences" "p-value Arellano-Bond test for AR(2) in first differences" "p-value Hansen J test of overidentifying restrictions" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (unrestricted)" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (difference)" "Goodness of fit")) addnotes("Two-step System GMM estimator. First difference instruments transformation" "Windmeijer-corrected cluster-robust errors" "Collapsed lags (2-.) used as instruments for endogenous variables") drop(_cons) order(informalidad "Geographic controls" $geo "Political controls" $pol "Land controls" $land "Social controls" $social "Lagged dependent variable") indicate("Time dummies=_Iy*") title("Dynamic panel data results")

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
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate $geo _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 .) collapse) iv(informalidad new_hom_rate $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate $geo $pol _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 .) collapse) iv(informalidad new_hom_rate $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate $geo $pol $land _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 .) collapse) iv(informalidad new_hom_rate $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate $geo $pol $land $social _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 .) collapse) iv(informalidad new_hom_rate $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
esttab using "results/2807_Model_2.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N N_clust n_inst Ftest ab_ar1 ab_ar2 han difs_h1 difs_h2 pseudoR2, fmt(%9.0f %9.0f %9.0f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f) labels("No. of observations" "No. of groups" "No. of instruments" "p-value F test of joint significance" "p-value Arellano-Bond test for AR(1) in first differences" "p-value Arellano-Bond test for AR(2) in first differences" "p-value Hansen J test of overidentifying restrictions" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (unrestricted)" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (difference)" "Goodness of fit")) addnotes("Two-step System GMM estimator. First difference instruments transformation" "Windmeijer-corrected cluster-robust errors" "Collapsed lags (2-.) used as instruments for endogenous variables") drop(_cons) order(informalidad new_hom_rate "Geographic controls" $geo "Political controls" $pol "Land controls" $land "Social controls" $social "Lagged dependent variable") indicate("Time dummies=_Iy*") title("Dynamic panel data results controlling for violence")

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
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(informalidad $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo $pol _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(informalidad $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo $pol $land _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(informalidad $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo $pol $land $social _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(informalidad $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
esttab using "results/2807_Model_3.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N N_clust n_inst Ftest ab_ar1 ab_ar2 han difs_h1 difs_h2 pseudoR2, fmt(%9.0f %9.0f %9.0f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f) labels("No. of observations" "No. of groups" "No. of instruments" "p-value F test of joint significance" "p-value Arellano-Bond test for AR(1) in first differences" "p-value Arellano-Bond test for AR(2) in first differences" "p-value Hansen J test of overidentifying restrictions" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (unrestricted)" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (difference)" "Goodness of fit")) addnotes("Two-step System GMM estimator. First difference instruments transformation" "Windmeijer-corrected cluster-robust errors" "Collapsed lags (2-.) used as instruments for endogenous variables") drop(_cons) order(informalidad new_hom_rate "Geographic controls" $geo "Political controls" $pol "Land controls" $land "Social controls" $social "Lagged dependent variable") indicate("Time dummies=_Iy*") title("Dynamic panel data results controlling for violence instrumented")

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
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate $geo _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(mdi $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate $geo $pol _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(mdi $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate $geo $pol $land _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(mdi $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate $geo $pol $land $social _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(mdi $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
estadd scalar Ftest=e(F_p)
matrix difs = e(diffsargan)
estadd scalar difs_h1 = difs[4,1]
estadd scalar difs_h2 = difs[5,1]
estadd scalar n_inst = e(j)
predict pred, xb
correlate coca_prop1000 pred
estadd scalar pseudoR2 = (r(rho))^2
drop pred
esttab using "results/2807_Model_R3.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N N_clust n_inst Ftest ab_ar1 ab_ar2 han difs_h1 difs_h2 pseudoR2, fmt(%9.0f %9.0f %9.0f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f) labels("No. of observations" "No. of groups" "No. of instruments" "p-value F test of joint significance" "p-value Arellano-Bond test for AR(1) in first differences" "p-value Arellano-Bond test for AR(2) in first differences" "p-value Hansen J test of overidentifying restrictions" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (unrestricted)" "p-value Difference-in-Hansen test of exogeneity for instrument subsets (difference)" "Goodness of fit")) addnotes("Two-step System GMM estimator. First difference instruments transformation" "Windmeijer-corrected cluster-robust errors" "Collapsed lags (2-.) used as instruments for endogenous variables") drop(_cons) order(mdi new_hom_rate "Geographic controls" $geo "Political controls" $pol "Land controls" $land "Social controls" $social "Lagged dependent variable") indicate("Time dummies=_Iy*") title("Robustness check - Dynamic panel data results controlling for violence instrumented")



***** TESTS

/*

*Serial Autocorrelation
xtserial coca_prop informalidad new_hom_rate $pol $land $social

*Heteroskedasticity

xtgls coca_prop informalidad new_hom_rate $pol $land $social, igls panels(heteroskedastic)
estimates store heterosk
xtgls coca_prop informalidad new_hom_rate $pol $land $social
estimates store homosk
local df = e(N_g) - 1
lrtest heterosk homosk, df(`df')

*/


/*tests poverty

bys codmpio: egen mpio_pobreza =mean(pobreza)
bys coddepto: egen dpto_pobreza=mean(pobreza)
replace pobreza =mpio_pobreza if pobreza ==.
replace pobreza =dpto_pobreza if pobreza ==.
drop mpio_pobreza
drop dpto_pobreza

xtile pobreza_q=pobreza, nq(2)


drop if pobreza_q!=1
sort codmpio year
xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo $pol $land $social _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(informalidad $geo $pol $land $social _Iy*) small



drop if pobreza_q!=2
sort codmpio year
xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo $pol $land $social _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 .) collapse) iv(informalidad $geo $pol $land $social _Iy*) small




/*TWOSTEP ESTIMATOR

**** DYNAMIC MODELS COMPLETE - ECONOMIC INSTITUTIONS (informality)

*system gmm estimator:

*1. No violence
eststo clear
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad $geo _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad $geo $pol _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad $geo $pol $land _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad $geo $pol $land $social _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
*esttab using "results/1407_1_Model_AB_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps stats(N han ab_ar1 ab_ar2, labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") keep(informalidad) order(informalidad) indicate("Year fixed effects=_Iy*" "Geographic Controls=$geo" "Political Controls=$pol" "Land Controls=$land" "Social Controls=$social") title("Dynamic Panel Data Model 1")
esttab using "results/1607_Model_1.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N han ab_ar1 ab_ar2, fmt(%9.0f %9.3f %9.3f %9.3f) labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities with Windmeijer's finite-sample correction.") drop(_cons) order(informalidad "Geographic controls" $geo "Political controls" $pol "Land controls" $land "Social controls" $social "Lagged dependent variable") indicate("Year fixed effects=_Iy*") title("Dynamic panel data results")

*2. No endogeneity of violence: 
eststo clear
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad new_hom_rate _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate $geo _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad new_hom_rate $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate $geo $pol _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad new_hom_rate $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate $geo $pol $land _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad new_hom_rate $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate $geo $pol $land $social _Iy*, twostep robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad new_hom_rate $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
*esttab using "results/1407_2_Model_AB_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps stats(N han ab_ar1 ab_ar2, labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") keep(informalidad new_hom_rate) order(informalidad new_hom_rate) indicate("Year fixed effects=_Iy*" "Geographic Controls=$geo" "Political Controls=$pol" "Land Controls=$land" "Social Controls=$social") title("Dynamic Panel Data Model 2")
esttab using "results/1607_Model_2.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N han ab_ar1 ab_ar2, fmt(%9.0f %9.3f %9.3f %9.3f) labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") drop(_cons) order(informalidad new_hom_rate "Geographic controls" $geo "Political controls" $pol "Land controls" $land "Social controls" $social "Lagged dependent variable") indicate("Year fixed effects=_Iy*") title("Dynamic panel data results controlling for violence")

*3. Endogeneity: 
eststo clear
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(informalidad _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(informalidad $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo $pol _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(informalidad $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo $pol $land _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(informalidad $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo $pol $land $social _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(informalidad $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
*esttab using "results/1407_3_Model_AB_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps stats(N han ab_ar1 ab_ar2, labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") keep(informalidad new_hom_rate) order(informalidad new_hom_rate) indicate("Year fixed effects=_Iy*" "Geographic Controls=$geo" "Political Controls=$pol" "Land Controls=$land" "Social Controls=$social") title("Dynamic Panel Data Model 3")
esttab using "results/1607_Model_3.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N han ab_ar1 ab_ar2, fmt(%9.0f %9.3f %9.3f %9.3f) labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") drop(_cons) order(informalidad new_hom_rate "Geographic controls" $geo "Political controls" $pol "Land controls" $land "Social controls" $social "Lagged dependent variable") indicate("Year fixed effects=_Iy*") title("Dynamic panel data results controlling for violence instrumented")

**** ROBUSTNESS - POLITICAL INSTITUTIONS (municipal development index)

*system gmm estimator:

/*
*1. No violence
eststo clear
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi $geo _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi $geo $pol _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi $geo $pol $land _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi $geo $pol $land $social _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
*esttab using "results/1407_ROB_1_Model_AB_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps stats(N han ab_ar1 ab_ar2, labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") keep(mdi) order(mdi) indicate("Year fixed effects=_Iy*" "Geographic Controls=$geo" "Political Controls=$pol" "Land Controls=$land" "Social Controls=$social") title("Dynamic Panel Data Model 1")
esttab using "results/1407_3_Model_AB_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.5f) b(%9.5f) stats(N han ab_ar1 ab_ar2, fmt(%9.0f %9.3f %9.3f %9.3f) labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") drop(_cons) order(informalidad new_hom_rate "Geographic controls" $geo "Political controls" $pol "Land controls" $land "Social controls" $social "Lagged dependent variable") indicate("Year fixed effects=_Iy*") title("Dynamic panel data results controlling for violence instrumented")

*2. No endogeneity of violence: 
eststo clear
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi new_hom_rate _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi new_hom_rate _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi new_hom_rate $geo _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi new_hom_rate $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi new_hom_rate $geo $pol _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi new_hom_rate $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi new_hom_rate $geo $pol $land _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi new_hom_rate $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi new_hom_rate $geo $pol $land $social _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi new_hom_rate $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
esttab using "results/1407_ROB_2_Model_AB_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps stats(N han ab_ar1 ab_ar2, labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") keep(mdi new_hom_rate) order(mdi new_hom_rate) indicate("Year fixed effects=_Iy*" "Geographic Controls=$geo" "Political Controls=$pol" "Land Controls=$land" "Social Controls=$social") title("Dynamic Panel Data Model 2")


*3. Endogeneity: 
eststo clear
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(mdi _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate $geo _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(mdi $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate $geo $pol _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(mdi $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate $geo $pol $land _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(mdi $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate $geo $pol $land $social _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(mdi $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
*esttab using "results/1407_ROB_3_Model_AB_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps stats(N han ab_ar1 ab_ar2, labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") keep(mdi new_hom_rate) order(mdi new_hom_rate) indicate("Year fixed effects=_Iy*" "Geographic Controls=$geo" "Political Controls=$pol" "Land Controls=$land" "Social Controls=$social") title("Dynamic Panel Data Model 3")
esttab using "results/1607_Model_R1.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N han ab_ar1 ab_ar2, fmt(%9.0f %9.3f %9.3f %9.3f) labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") drop(_cons) order(mdi new_hom_rate "Geographic controls" $geo "Political controls" $pol "Land controls" $land "Social controls" $social "Lagged dependent variable") indicate("Year fixed effects=_Iy*") title("Robustness check - Dynamic panel data results controlling for violence instrumented")


/*tests with logs:

foreach i in coca_prop1000 informalidad new_hom_rate $geo $pol $land $social{
gen ln_`i' = ln(`i')
}

global ln_geo "ln_alturakm ln_dismdo ln_discapital ln_aptitud ln_erosion"
global ln_pol "ln_inv_educacion_cp ln_inv_salud_cp ln_inv_justicia_cp ln_ba_nu1000"
global ln_land "ln_g_uaf ln_htapropietario"
global ln_social "ln_health_coverage ln_rurality_index"

xtabond2 ln_coca_prop1000 l.ln_coca_prop1000 ln_informalidad ln_new_hom_rate $ln_geo $ln_pol $ln_land $ln_social _Iy*, twostep robust gmm(l.ln_coca_prop1000 ln_new_hom_rate, lag(2 2)) iv(ln_informalidad $ln_geo $ln_pol $ln_land $ln_social _Iy*) small


/*tests armed group

bys codmpio: egen mpio_AUC =mean(AUC)
bys codmpio: egen mpio_FARC =mean(FARC)
bys codmpio: egen mpio_ELN =mean(ELN)

gen some_AUC=(mpio_AUC!=0)
gen some_FARC=(mpio_FARC!=0)
gen some_ELN=(mpio_ELN!=0)


drop if some_AUC!=1
sort codmpio year
xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo $pol $land $social _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(informalidad $geo $pol $land $social _Iy*) small

drop if some_FARC!=1
sort codmpio year
xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo $pol $land $social _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(informalidad $geo $pol $land $social _Iy*) small

drop if some_ELN!=1
sort codmpio year
xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo $pol $land $social _Iy*, twostep robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(informalidad $geo $pol $land $social _Iy*) small

/*

*Estimations other tests

*One step estimator:

**** DYNAMIC MODELS COMPLETE - ECONOMIC INSTITUTIONS (informality)

*system gmm estimator:

*1. No violence
eststo clear
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad $geo _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad $geo $pol _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad $geo $pol $land _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad $geo $pol $land $social _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
*esttab using "results/1407_1_Model_AB_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps stats(N han ab_ar1 ab_ar2, labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") keep(informalidad) order(informalidad) indicate("Year fixed effects=_Iy*" "Geographic Controls=$geo" "Political Controls=$pol" "Land Controls=$land" "Social Controls=$social") title("Dynamic Panel Data Model 1")
esttab using "results/1407_1_Model_AB_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N han ab_ar1 ab_ar2, fmt(%9.0f %9.3f %9.3f %9.3f) labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") drop(_cons) order(informalidad "Geographic controls" $geo "Political controls" $pol "Land controls" $land "Social controls" $social "Lagged dependent variable") indicate("Year fixed effects=_Iy*") title("Dynamic panel data results")

*2. No endogeneity of violence: 
eststo clear
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad new_hom_rate _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate $geo _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad new_hom_rate $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate $geo $pol _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad new_hom_rate $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate $geo $pol $land _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad new_hom_rate $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 informalidad new_hom_rate $geo $pol $land $social _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(informalidad new_hom_rate $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
*esttab using "results/1407_2_Model_AB_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps stats(N han ab_ar1 ab_ar2, labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") keep(informalidad new_hom_rate) order(informalidad new_hom_rate) indicate("Year fixed effects=_Iy*" "Geographic Controls=$geo" "Political Controls=$pol" "Land Controls=$land" "Social Controls=$social") title("Dynamic Panel Data Model 2")
esttab using "results/1407_2_Model_AB_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N han ab_ar1 ab_ar2, fmt(%9.0f %9.3f %9.3f %9.3f) labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") drop(_cons) order(informalidad new_hom_rate "Geographic controls" $geo "Political controls" $pol "Land controls" $land "Social controls" $social "Lagged dependent variable") indicate("Year fixed effects=_Iy*") title("Dynamic panel data results controlling for violence")

*3. Endogeneity: 
eststo clear
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate _Iy*, robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(informalidad _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo _Iy*, robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(informalidad $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo $pol _Iy*, robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(informalidad $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo $pol $land _Iy*, robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(informalidad $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 informalidad new_hom_rate $geo $pol $land $social _Iy*, robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(informalidad $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
*esttab using "results/1407_3_Model_AB_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps stats(N han ab_ar1 ab_ar2, labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") keep(informalidad new_hom_rate) order(informalidad new_hom_rate) indicate("Year fixed effects=_Iy*" "Geographic Controls=$geo" "Political Controls=$pol" "Land Controls=$land" "Social Controls=$social") title("Dynamic Panel Data Model 3")
esttab using "results/1407_3_Model_AB_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N han ab_ar1 ab_ar2, fmt(%9.0f %9.3f %9.3f %9.3f) labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") drop(_cons) order(informalidad new_hom_rate "Geographic controls" $geo "Political controls" $pol "Land controls" $land "Social controls" $social "Lagged dependent variable") indicate("Year fixed effects=_Iy*") title("Dynamic panel data results controlling for violence instrumented")

**** ROBUSTNESS - POLITICAL INSTITUTIONS (municipal development index)

*system gmm estimator:

/*
*1. No violence
eststo clear
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi $geo _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi $geo $pol _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi $geo $pol $land _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi $geo $pol $land $social _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
*esttab using "results/1407_ROB_1_Model_AB_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps stats(N han ab_ar1 ab_ar2, labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") keep(mdi) order(mdi) indicate("Year fixed effects=_Iy*" "Geographic Controls=$geo" "Political Controls=$pol" "Land Controls=$land" "Social Controls=$social") title("Dynamic Panel Data Model 1")
esttab using "results/1407_3_Model_AB_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.5f) b(%9.5f) stats(N han ab_ar1 ab_ar2, fmt(%9.0f %9.3f %9.3f %9.3f) labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") drop(_cons) order(informalidad new_hom_rate "Geographic controls" $geo "Political controls" $pol "Land controls" $land "Social controls" $social "Lagged dependent variable") indicate("Year fixed effects=_Iy*") title("Dynamic panel data results controlling for violence instrumented")

*2. No endogeneity of violence: 
eststo clear
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi new_hom_rate _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi new_hom_rate _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi new_hom_rate $geo _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi new_hom_rate $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi new_hom_rate $geo $pol _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi new_hom_rate $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi new_hom_rate $geo $pol $land _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi new_hom_rate $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 L.coca_prop1000 mdi new_hom_rate $geo $pol $land $social _Iy*, robust gmm(L.coca_prop1000, lag(2 2)) iv(mdi new_hom_rate $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
esttab using "results/1407_ROB_2_Model_AB_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps stats(N han ab_ar1 ab_ar2, labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") keep(mdi new_hom_rate) order(mdi new_hom_rate) indicate("Year fixed effects=_Iy*" "Geographic Controls=$geo" "Political Controls=$pol" "Land Controls=$land" "Social Controls=$social") title("Dynamic Panel Data Model 2")


*3. Endogeneity: 
eststo clear
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate _Iy*, robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(mdi _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate $geo _Iy*, robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(mdi $geo _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate $geo $pol _Iy*, robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(mdi $geo $pol _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate $geo $pol $land _Iy*, robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(mdi $geo $pol $land _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
eststo: xtabond2 coca_prop1000 l.coca_prop1000 mdi new_hom_rate $geo $pol $land $social _Iy*, robust gmm(l.coca_prop1000 new_hom_rate, lag(2 2)) iv(mdi $geo $pol $land $social _Iy*) small
estadd scalar han=e(hansenp)
estadd scalar ab_ar1=e(ar1p)
estadd scalar ab_ar2=e(ar2p)
*esttab using "results/1407_ROB_3_Model_AB_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps stats(N han ab_ar1 ab_ar2, labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") keep(mdi new_hom_rate) order(mdi new_hom_rate) indicate("Year fixed effects=_Iy*" "Geographic Controls=$geo" "Political Controls=$pol" "Land Controls=$land" "Social Controls=$social") title("Dynamic Panel Data Model 3")
esttab using "results/1407_ROB_3_Model_AB_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) stats(N han ab_ar1 ab_ar2, fmt(%9.0f %9.3f %9.3f %9.3f) labels("Observations" "p-value Hansen J statistic" "p-value Arellano-Bond test for AR(1)" "p-value Arellano-Bond test for AR(2)")) addnotes("Robust standard errors to heteroskedasticity and autocorrelation within municipalities.") drop(_cons) order(mdi new_hom_rate "Geographic controls" $geo "Political controls" $pol "Land controls" $land "Social controls" $social "Lagged dependent variable") indicate("Year fixed effects=_Iy*") title("Robustness check - Dynamic panel data results controlling for violence instrumented")






*Fixed Effects Static Model

eststo clear
eststo: xtreg coca_prop informalidad new_hom_rate, cluster(coddepto) fe i(codmpio)
estadd scalar r22=e(r2_w)
eststo: xtreg coca_prop informalidad new_hom_rate $pol, cluster(coddepto) fe i(codmpio)
estadd scalar r22=e(r2_w)
eststo: xtreg coca_prop informalidad new_hom_rate $pol $land, cluster(coddepto) fe i(codmpio)
estadd scalar r22=e(r2_w)
eststo: xtreg coca_prop informalidad new_hom_rate $pol $land $social, cluster(coddepto) fe i(codmpio)
estadd scalar r22=e(r2_w)
esttab using "results/1307Table_Panel_Static.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps stats(N r22 chi2, labels("Observations" "R square" "Chi-Square")) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad) order(informalidad) indicate("Land Controls=$land" "Political Controls=$pol" "Social Controls=$social") title("Static Panel Data Model")

*Fixed Effects Dynamic Model

eststo clear
eststo: xtabond coca_prop informalidad new_hom_rate, pre(informalidad) endogenous(new_hom_rate) maxlags(2) lags(2)
estadd scalar r22=e(r2_w)
eststo: xtabond coca_prop informalidad new_hom_rate $pol, pre(informalidad) endogenous(new_hom_rate) maxlags(2) lags(2)
estadd scalar r22=e(r2_w)
eststo: xtabond coca_prop informalidad new_hom_rate $pol $land, pre(informalidad) endogenous(new_hom_rate) maxlags(2) lags(2)
estadd scalar r22=e(r2_w)
eststo: xtabond coca_prop informalidad new_hom_rate $pol $land $social, pre(informalidad) endogenous(new_hom_rate) maxlags(2) lags(2)
estadd scalar r22=e(r2_w)
esttab using "results/1307Table_Panel_Dynamic.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps stats(N r22 chi2, labels("Observations" "R square" "Chi-Square")) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad new_hom_rate) order(informalidad new_hom_rate) indicate("Land Controls=$land" "Political Controls=$pol" "Social Controls=$social") title("Static Panel Data Model")

*Fixed Effects IV Model

eststo clear
eststo: xtivreg coca_prop informalidad (new_hom_rate=l.new_hom_rate), fe i(codmpio)
estadd scalar r22=e(r2_w)
eststo: xtivreg coca_prop informalidad (new_hom_rate=l.new_hom_rate) $pol, fe i(codmpio)
estadd scalar r22=e(r2_w)
eststo: xtivreg coca_prop informalidad (new_hom_rate=l.new_hom_rate) $pol $land, fe i(codmpio)
estadd scalar r22=e(r2_w)
eststo: xtivreg coca_prop informalidad (new_hom_rate=l.new_hom_rate) $pol $land $social, fe i(codmpio)
estadd scalar r22=e(r2_w)
esttab using "results/1307Table_Panel_Static.csv", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps stats(N r22 chi2, labels("Observations" "R square" "Chi-Square")) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad) order(informalidad) indicate("Land Controls=$land" "Political Controls=$pol" "Social Controls=$social") title("Static Panel Data Model")



*/




*/

*El período de tiempo no es suficiente para un panel dinámico / Pruebas de especificación / Risk function

/*
Hausman specification test

xtreg coca_prop informalidad new_hom_rate $pol $land $social, fe
estimates store fixed_e
xtreg coca_prop informalidad new_hom_rate $pol $land $social, re
estimates store random_e
hausman fixed_e random_e
*/


**** END PANEL


**** PROBIT CROSS SECTION MODEL
/*

collapse (mean) pobreza aniosestudioprom discapital dismdo inv_salud_cp inv_vivienda_cp inv_educacion_cp inv_vias_cp inv_agrario_cp inv_justicia_cp g_terreno g_prop g_uaf RSsisben inv_per_docente inv_calidad petotalins htapropietario ba_nu partelectoral prod_oro prod_plata H_coca areasuphe informalidad areasupkm altura indrural gandina goriental gpacifica gcaribe tasa_homicidios tasahom tasamas Violencia_48_a_53 desplazados imr ataques litrate homicidio new_hom_rate (sum) D_Coca, by(codmpio coddepto)
gen d_coca=(D_Coca!=0)

**** Labels

label variable altura "Altitude (mts)"
label variable areasupkm "Extension (km2)"
label variable gandina "Andina Region (yes=1)"
label variable gpacifica "Pacific Region (yes=1)"
label variable dismdo "Distance to the nearest market"
label variable inv_educacion_cp "Education investment per capita"
label variable inv_justicia_cp "Justice investment per capita"
label variable ba_nu "Number of agricultural loans"
label variable g_uaf "Land quality gini index"
label variable htapropietario "Average size plot"
label variable RSsisben "Health coverage"
label variable pobreza "Percentage of people below poverty line"
label variable aniosestudioprom "Average year of education"
label variable informalidad "Informality index land property (average 2000-2008)"
label variable tasa_homicidios "Homicide rate (per 100000 habitants)"
label variable imr "Infant mortality rate (per 1000 live births)"
label variable homicidio "Average number of homicides per year"

**** Vectors of final covariates

global geo "altura areasupkm gandina gpacifica dismdo"
global pol "inv_educacion_cp inv_justicia_cp inv_vias_cp ba_nu partelectoral"
global land "g_uaf htapropietario"
global social "pobreza imr"

********** VERSIÓN NUEVA, CON VARIABLE TASA_HOMICIDIOS

**** Populating the Dataset with departamental and national means

foreach i in informalidad tasa_homicidios new_hom_rate $geo $land $pol $social aniosestudioprom {
bys coddepto: egen dpto_`i'=mean(`i')
egen n_`i'=mean(`i')
replace `i'= dpto_`i' if `i'==.
replace `i'=  n_`i' if `i'==.
drop dpto_`i'  n_`i'
}

**** Predicting Pr(d_coca)

xi i.coddepto
dprobit d_coca informalidad _Ic*, robust cluster(coddepto)
predict xi
drop if x==.

**** DESCRIPTIVE STATISTICS

**** Descriptive Statistics (Table I)

*myttests informalidad total $geo $land $pol $social, by(d_coca)
*esttab using "results/Table_1.tex", replace nomtitle nonumbers noobs label  cells("N_1(label(Sample)) mu_1(fmt(%9.3f) label(Mean)) N_2(label(Sample)) mu_2(fmt(%9.3f) label(Mean)) d(fmt(%9.3f) star pvalue(d_p) label(Difference))" ". sd_1(par fmt(%9.3f) label(std.dev)) . sd_2(par fmt(%9.3f) label( )) d_se(par fmt(a3) label( ))" ) title("Descriptive statistic") addnotes("Standard errors in brackets" "* p$\prec$0.05, ** p$\prec$0.01, *** p$\prec$0.001" "Two-sided mean test reported.")        

**** First test for Regression Discontinuity Design

xtile inf_2=informalidad, nq(50)
preserve
collapse (mean) informalidad  H_coca D_Coca (sum) d_coca, by(inf_2)
*rd D_Coca inf_2, gr mbw(100 50 200) z0(35)
rd D_Coca inf_2, gr mbw(100) z0(35)
graph export "graphs/g_rd.png", width(3000) replace
restore
window manage close graph

**** Descriptive Statistics (Graph I)

xtile inf_1=informalidad, nq(100)
preserve
gen pert_co= H_coca/areasuphe
collapse (mean) informalidad  H_coca D_Coca (sum) d_coca, by(inf_1)
twoway (lfit D_Coca inf_1) (scatter informalidad inf_1, yaxis(2) mcolor(gs1)), ytitle("Number of years (mean by quantile)", axis(1)) ytitle("Informalty Index", axis(2)) plotregion(fcolor(white)) graphregion(fcolor(white)) xtitle("Informalty Index's quantile")  legend(lab(1 "Number of years with coca crops") lab(2 "Informality Index"))
graph export "graphs/g_1.pdf", width(3000) replace
restore
window manage close graph


**** REGRESSIONS

**** Model I (Baseline Probit)

xi i.coddepto
eststo clear
eststo: dprobit d_coca informalidad _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad new_hom_rate _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/1107Model1.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2_p,  fmt(%9.0f %9.3f) labels("Observations" "Pseudo-R2" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad) order(informalidad) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

**** Model II (Probit with Violence (homicide rate))

gen intarc=informalidad* new_hom_rate
label variable intarc "Informality*Homicide rate"
xi i.coddepto
eststo clear
eststo: dprobit d_coca informalidad new_hom_rate _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad new_hom_rate $geo _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad new_hom_rate $geo $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad new_hom_rate $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad new_hom_rate intarc $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad new_hom_rate intarc $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/1107Model2.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2_p,  fmt(%9.0f %9.3f) labels("Observations" "Pseudo-R2" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad new_hom_rate intarc) order(informalidad new_hom_rate intarc) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

**** Model III (IV-Probit Violence (homicide rate) instrumented with average years of education)

* First Step
eststo clear
eststo: reg new_hom_rate aniosestudioprom informalidad _Ic*, robust cluster(coddepto)
eststo: reg new_hom_rate aniosestudioprom informalidad $geo _Ic*, robust cluster(coddepto)
eststo: reg new_hom_rate aniosestudioprom informalidad $geo $pol _Ic*, robust cluster(coddepto)
eststo: reg new_hom_rate aniosestudioprom informalidad $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: reg new_hom_rate aniosestudioprom informalidad $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/1107ModelINST.csv", star(* 0.10 ** 0.05 *** 0.01) replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2 F ,  fmt(%9.0f %9.3f) labels("Observations" "R2" "F statistic" )) addnotes("Robust standard error estimated by cluster per Department.") keep(aniosestudioprom informalidad) order(aniosestudioprom informalidad) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social" )  title("First Step (IV Violence) ")

eststo clear
eststo: ivprobit d_coca informalidad (new_hom_rate = aniosestudioprom) _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
eststo: ivprobit d_coca informalidad (new_hom_rate = aniosestudioprom) $geo _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
eststo: ivprobit d_coca informalidad (new_hom_rate = aniosestudioprom) $geo $pol _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
eststo: ivprobit d_coca informalidad (new_hom_rate = aniosestudioprom) $geo $land $pol _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
eststo: ivprobit d_coca informalidad (new_hom_rate = aniosestudioprom) $geo $pol $land $social _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
esttab using "results/1107Model3.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N wald,  fmt(%9.0f %9.3f) labels("Observations" "p-value Wald chi-squared test of exogeneity" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad new_hom_rate aniosestudioprom) order(aniosestudioprom informalidad new_hom_rate) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

**** END PROBIT CROSS SECTION MODEL




/**** OTHER MODELS NOT CONSIDERED:

/**** REGRESSIONS CON LITRATE

**** Model I (Baseline Probit)

xi i.coddepto
eststo clear
eststo: dprobit d_coca informalidad _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad police_military_operations _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/1107Model1.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2_p,  fmt(%9.0f %9.3f) labels("Observations" "Pseudo-R2" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad) order(informalidad) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

**** Model II (Probit with Violence (Police and military operations))

gen intarc=informalidad* police_military_operations
label variable intarc "Informality*Police and military operations"
xi i.coddepto
eststo clear
eststo: dprobit d_coca informalidad police_military_operations _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad police_military_operations $geo _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad police_military_operations $geo $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad police_military_operations $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad police_military_operations intarc $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad police_military_operations intarc $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/1107Model2.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2_p,  fmt(%9.0f %9.3f) labels("Observations" "Pseudo-R2" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad police_military_operations intarc) order(informalidad police_military_operations intarc) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

**** Model III (IV-Probit Violence (Police and military operations) instrumented with literacy rate)

* First Step
eststo clear
eststo: reg police_military_operations litrate informalidad _Ic*, robust cluster(coddepto)
eststo: reg police_military_operations litrate informalidad $geo _Ic*, robust cluster(coddepto)
eststo: reg police_military_operations litrate informalidad $geo $pol _Ic*, robust cluster(coddepto)
eststo: reg police_military_operations litrate informalidad $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: reg police_military_operations litrate informalidad $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/1107Model3a.csv", star(* 0.10 ** 0.05 *** 0.01) replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2 F ,  fmt(%9.0f %9.3f) labels("Observations" "R2" "F statistic" )) addnotes("Robust standard error estimated by cluster per Department.") keep(litrate informalidad) order(litrate informalidad) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social" )  title("First Step (IV Violence) ")

eststo clear
eststo: ivprobit d_coca informalidad (police_military_operations=litrate) _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
eststo: ivprobit d_coca informalidad (police_military_operations=litrate) $geo _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
eststo: ivprobit d_coca informalidad (police_military_operations=litrate) $geo $pol _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
eststo: ivprobit d_coca informalidad (police_military_operations=litrate) $geo $land $pol _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
eststo: ivprobit d_coca informalidad (police_military_operations=litrate) $geo $land $pol $social _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
esttab using "results/1107Model3.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N wald,  fmt(%9.0f %9.3f) labels("Observations" "p-value Wald chi-squared test of exogeneity" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad police_military_operations litrate) order(litrate informalidad police_military_operations) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

**** REGRESSIONS CON LITRATE VIOLENCE=HOMICIDIOS

**** Model I (Baseline Probit)

xi i.coddepto

**** Model II (Probit with Violence (Number of homicides))

gen intarc2=informalidad* homicidio
label variable intarc2 "Informality*Homicides"
xi i.coddepto
eststo clear
eststo: dprobit d_coca informalidad homicidio _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad homicidio $geo _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad homicidio $geo $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad homicidio $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad homicidio intarc2 $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad homicidio intarc2 $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/hom_1107Model2.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2_p,  fmt(%9.0f %9.3f) labels("Observations" "Pseudo-R2" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad homicidio intarc2) order(informalidad homicidio intarc2) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

**** Model III (IV-Probit Violence (Number of homicides) instrumented with literacy rate)

* First Step
eststo clear
eststo: reg homicidio litrate informalidad _Ic*, robust cluster(coddepto)
eststo: reg homicidio litrate informalidad $geo _Ic*, robust cluster(coddepto)
eststo: reg homicidio litrate informalidad $geo $pol _Ic*, robust cluster(coddepto)
eststo: reg homicidio litrate informalidad $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: reg homicidio litrate informalidad $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/hom_1107Model3a.csv", star(* 0.10 ** 0.05 *** 0.01) replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2 F ,  fmt(%9.0f %9.3f) labels("Observations" "R2" "F statistic" )) addnotes("Robust standard error estimated by cluster per Department.") keep(litrate informalidad) order(litrate informalidad) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social" )  title("First Step (IV Violence) ")

eststo clear
eststo: ivprobit d_coca informalidad (homicidio =litrate) _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
eststo: ivprobit d_coca informalidad (homicidio =litrate) $geo _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
eststo: ivprobit d_coca informalidad (homicidio =litrate) $geo $pol _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
eststo: ivprobit d_coca informalidad (homicidio =litrate) $geo $land $pol _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
eststo: ivprobit d_coca informalidad (homicidio =litrate) $geo $land $pol $social _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
esttab using "results/hom_1107Model3.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N wald,  fmt(%9.0f %9.3f) labels("Observations" "p-value Wald chi-squared test of exogeneity" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad homicidio litrate) order(litrate informalidad homicidio) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

**** REGRESSIONS WITH new HOMICIDE rate

**** Model III (IV-Probit Violence (Police and military operations) instrumented with literacy rate)

* First Step
eststo clear
eststo: reg new_hom_rate litrate informalidad _Ic*, robust cluster(coddepto)
eststo: reg new_hom_rate litrate informalidad $geo _Ic*, robust cluster(coddepto)
eststo: reg new_hom_rate litrate informalidad $geo $pol _Ic*, robust cluster(coddepto)
eststo: reg new_hom_rate litrate informalidad $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: reg new_hom_rate litrate informalidad $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/new_hom_rate_1107Model3a.csv", star(* 0.10 ** 0.05 *** 0.01) replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2 F ,  fmt(%9.0f %9.3f) labels("Observations" "R2" "F statistic" )) addnotes("Robust standard error estimated by cluster per Department.") keep(litrate informalidad) order(litrate informalidad) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social" )  title("First Step (IV Violence) ")

eststo clear
eststo: ivprobit d_coca informalidad (new_hom_rate =litrate) _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
eststo: ivprobit d_coca informalidad (new_hom_rate =litrate) $geo _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
eststo: ivprobit d_coca informalidad (new_hom_rate =litrate) $geo $pol _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
eststo: ivprobit d_coca informalidad (new_hom_rate =litrate) $geo $land $pol _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
eststo: ivprobit d_coca informalidad (new_hom_rate =litrate) $geo $land $pol $social _Ic*, robust cluster(coddepto)
estadd scalar wald=e(p_exog)
esttab using "results/new_hom_rate_1107Model3.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N wald,  fmt(%9.0f %9.3f) labels("Observations" "p-value Wald chi-squared test of exogeneity" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad new_hom_rate litrate) order(litrate informalidad new_hom_rate) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")



**** Model I (Baseline Probit)

xi i.coddepto
eststo clear
eststo: dprobit d_coca informalidad _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad tasa_homicidios _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/vTable_2.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2_p,  fmt(%9.0f %9.3f) labels("Observations" "Pseudo-R2" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad) order(informalidad) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

**** Model II (Probit with Violence (Homicide Rate))

gen intarc=informalidad*tasa_homicidios
label variable intarc "Informality*Homicide Rate"
xi i.coddepto
eststo clear
eststo: dprobit d_coca informalidad _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad tasa_homicidios _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad tasa_homicidios $geo _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad tasa_homicidios $geo $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad tasa_homicidios $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad tasa_homicidios intarc $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad tasa_homicidios intarc $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/vTable_3.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2_p,  fmt(%9.0f %9.3f) labels("Observations" "Pseudo-R2" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad tasa_homicidios intarc) order(informalidad tasa_homicidios intarc) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

**** Model III (IV-Probit Violence (Homicide Rate) instrumented with literacy rate)
sort codmpio 
merge 1:1 codmpio using "data/inst_usar.dta"
drop if _merge==2
eststo clear
eststo: ivprobit d_coca informalidad (tasa_homicidios=violencia_48_53) _Ic*, robust cluster(coddepto)
estadd scalar wald=e(chi2_exog)
eststo: ivprobit d_coca informalidad (tasa_homicidios=violencia_48_53) $geo _Ic*, robust cluster(coddepto)
estadd scalar wald=e(chi2_exog)
eststo: ivprobit d_coca informalidad (tasa_homicidios=violencia_48_53) $geo $pol _Ic*, robust cluster(coddepto)
estadd scalar wald=e(chi2_exog)
eststo: ivprobit d_coca informalidad (tasa_homicidios=violencia_48_53) $geo $land $pol _Ic*, robust cluster(coddepto)
estadd scalar wald=e(chi2_exog)
eststo: ivprobit d_coca informalidad (tasa_homicidios=violencia_48_53) $geo $land $pol $social _Ic*, robust cluster(coddepto)
estadd scalar wald=e(chi2_exog)
esttab using "results/vTable_4.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N wald,  fmt(%9.0f %9.3f) labels("Observations" "Wald chi-squared test of exogeneity" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad tasa_homicidios violencia_48_53) order(violencia_48_53 informalidad tasa_homicidios) indicate("Province Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

/********** VERSIÓN ORIGINAL JUAN CARLOS, CON VARIABLE TOTAL EN HOMICIDIOS

**** Populating the Dataset with departamental and national means

foreach i in informalidad total $geo $land $pol $social {
bys coddepto: egen dpto_`i'=mean(`i')
egen n_`i'=mean(`i')
replace `i'= dpto_`i' if `i'==.
replace `i'=  n_`i' if `i'==.
drop dpto_`i'  n_`i'
}

**** Predicting Pr(d_coca)

xi i.coddepto
dprobit d_coca informalidad _Ic*, robust cluster(coddepto)
predict x
drop if x==.

**** Descriptive Statistics (Table I)

*myttests informalidad total $geo $land $pol $social, by(d_coca)
*esttab using "results/Table_1.tex", replace nomtitle nonumbers noobs label  cells("N_1(label(Sample)) mu_1(fmt(%9.3f) label(Mean)) N_2(label(Sample)) mu_2(fmt(%9.3f) label(Mean)) d(fmt(%9.3f) star pvalue(d_p) label(Difference))" ". sd_1(par fmt(%9.3f) label(std.dev)) . sd_2(par fmt(%9.3f) label( )) d_se(par fmt(a3) label( ))" ) title("Descriptive statistic") addnotes("Standard errors in brackets" "* p$\prec$0.05, ** p$\prec$0.01, *** p$\prec$0.001" "Two-sided mean test reported.")        
    
**** Descriptive Statistics (Graph I)

xtile inf_1=informalidad, nq(100)
preserve
gen pert_co= H_coca/areasuphe
collapse (mean) informalidad  H_coca D_Coca (sum) d_coca, by(inf_1)
twoway (bar D_Coca inf_1, barwidth(.7) color(gs9)) (scatter informalidad inf_1, yaxis(2) mcolor(gs1)), ytitle("Number of years (mean by quantile)", axis(1)) ytitle("Informalty Index", axis(2)) plotregion(fcolor(white)) graphregion(fcolor(white)) xtitle("Informalty Index' quantile")  legend(lab(1 "Municipalities") lab(2 "Informality Index"))
graph export "graphs/g_1.png", width(3000) replace
restore

**** REGRESSIONS

**** Model I (Baseline Probit)

xi i.coddepto
eststo clear
eststo: dprobit d_coca informalidad _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad total _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/Table_2.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2_p,  fmt(%9.0f %9.3f) labels("Observations" "Pseudo-R2" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad) order(informalidad) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

**** Model II (Probit with Violence (Homicide Rate))

gen intarc=informalidad*total
label variable intarc "Informality*Homicide Rate"
xi i.coddepto
eststo clear
eststo: dprobit d_coca informalidad _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad total _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad total $geo _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad total $geo $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad total $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad total intarc $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad total intarc $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/Table_3.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2_p,  fmt(%9.0f %9.3f) labels("Observations" "Pseudo-R2" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad total intarc) order(informalidad total intarc) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

**** Model III (IV-Probit Violence (Homicide Rate) instrumented with literacy rate)
sort codmpio 
merge 1:1 codmpio using "data/inst_usar.dta"
drop if _merge==2
eststo clear
eststo: ivprobit d_coca informalidad (total=litrate) _Ic*, robust cluster(coddepto)
estadd scalar wald=e(chi2_exog)
eststo: ivprobit d_coca informalidad (total=litrate) $geo _Ic*, robust cluster(coddepto)
estadd scalar wald=e(chi2_exog)
eststo: ivprobit d_coca informalidad (total=litrate) $geo $pol _Ic*, robust cluster(coddepto)
estadd scalar wald=e(chi2_exog)
eststo: ivprobit d_coca informalidad (total=litrate) $geo $land $pol _Ic*, robust cluster(coddepto)
estadd scalar wald=e(chi2_exog)
eststo: ivprobit d_coca informalidad (total=litrate) $geo $land $pol $social _Ic*, robust cluster(coddepto)
estadd scalar wald=e(chi2_exog)
esttab using "results/Table_4.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N wald,  fmt(%9.0f %9.3f) labels("Observations" "Wald chi-squared test of exogeneity" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad total litrate) order(litrate informalidad total) indicate("Province Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

* */



**** OTHER TESTS

/*

For model III???

/* 
* First Step
eststo clear
eststo: reg total litrate informalidad _Ic*, robust cluster(coddepto)
eststo: reg total litrate informalidad $geo _Ic*, robust cluster(coddepto)
eststo: reg total litrate informalidad $geo $pol _Ic*, robust cluster(coddepto)
eststo: reg total litrate informalidad $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: reg total litrate informalidad $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/Table_4a.csv", star(* 0.10 ** 0.05 *** 0.01) replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2 F ,  fmt(%9.0f %9.3f) labels("Observations" "R2" "F statistic" )) addnotes("Robust standard error estimated by cluster per Department.") keep(litrate informalidad ) order(litrate informalidad ) indicate("Province Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social" )  title("First Step (IV Violence) ")
*/

** Population of the instrumental variable
/*
bys coddepto: egen dpto_litrate=mean(litrate)
egen n_litrate=mean(litrate)
replace litrate= dpto_litrate if litrate==.
replace litrate=  n_litrate if litrate==.
drop dpto_litrate  n_litrate



* IV-Probit twostep Newey
eststo clear
eststo: ivprobit d_coca informalidad (total=litrate) _Ic*, twostep
estadd scalar wald=e(chi2_exog)
eststo: ivprobit d_coca informalidad (total=litrate) $geo _Ic*, twostep
estadd scalar wald=e(chi2_exog)
eststo: ivprobit d_coca informalidad (total=litrate) $geo $pol _Ic*, twostep
estadd scalar wald=e(chi2_exog)
eststo: ivprobit d_coca informalidad (total=litrate) $geo $land $pol _Ic*, twostep
estadd scalar wald=e(chi2_exog)
eststo: ivprobit d_coca informalidad (total=litrate) $geo $land $pol $social _Ic*, twostep
estadd scalar wald=e(chi2_exog)
esttab using "results/Table_4_2st.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N wald,  fmt(%9.0f %9.3f) labels("Observations" "Wald chi-squared test of exogeneity" )) addnotes("Newey's two-step estimator.") keep(informalidad total litrate) order(litrate informalidad total) indicate("Province Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

**** Model IV (IV-Probit Violence (Homicide Rate) instrumented with violence in 48-53)
 
* First Step
eststo clear
eststo: reg total Violencia_48_a_53 informalidad _Ic*, robust cluster(coddepto)
eststo: reg total Violencia_48_a_53 informalidad $geo _Ic*, robust cluster(coddepto)
eststo: reg total Violencia_48_a_53 informalidad $geo $pol _Ic*, robust cluster(coddepto)
eststo: reg total Violencia_48_a_53 informalidad $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: reg total Violencia_48_a_53 informalidad $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/Table_5a.csv", star(* 0.10 ** 0.05 *** 0.01) replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2 F ,  fmt(%9.0f %9.3f) labels("Observations" "R2" "F statistic" )) addnotes("Robust standard error estimated by cluster per Department.") keep(Violencia_48_a_53 informalidad ) order(Violencia_48_a_53 informalidad ) indicate("Province Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social" )  title("First Step (IV Violence) ")

* IV-Probit
eststo clear
eststo: ivprobit d_coca informalidad (total=Violencia_48_a_53) _Ic*, robust cluster(coddepto)
eststo: ivprobit d_coca informalidad (total=Violencia_48_a_53) $geo _Ic*, robust cluster(coddepto)
eststo: ivprobit d_coca informalidad (total=Violencia_48_a_53) $geo $pol _Ic*, robust cluster(coddepto)
eststo: ivprobit d_coca informalidad (total=Violencia_48_a_53) $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: ivprobit d_coca informalidad (total=Violencia_48_a_53) $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/Table_5.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2_p,  fmt(%9.0f %9.3f) labels("Observations" "Pseudo-R2" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad total Violencia_48_a_53) order(Violencia_48_a_53 informalidad total) indicate("Province Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

**** Model V (IV-Probit Violence (Homicide Rate) instrumented with literacy rates and violence in 48-53)
 
* First Step
eststo clear
eststo: reg total litrate Violencia_48_a_53 informalidad _Ic*, robust cluster(coddepto)
eststo: reg total litrate Violencia_48_a_53 informalidad $geo _Ic*, robust cluster(coddepto)
eststo: reg total litrate Violencia_48_a_53 informalidad $geo $pol _Ic*, robust cluster(coddepto)
eststo: reg total litrate Violencia_48_a_53 informalidad $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: reg total litrate Violencia_48_a_53 informalidad $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/Table_6a.csv", star(* 0.10 ** 0.05 *** 0.01) replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2 F ,  fmt(%9.0f %9.3f) labels("Observations" "R2" "F statistic" )) addnotes("Robust standard error estimated by cluster per Department.") keep(litrate Violencia_48_a_53 informalidad ) order(litrate Violencia_48_a_53 informalidad ) indicate("Province Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social" )  title("First Step (IV Violence) ")

* IV-Probit
eststo clear
eststo: ivprobit d_coca informalidad (total=litrate Violencia_48_a_53) _Ic*, robust cluster(coddepto)
eststo: ivprobit d_coca informalidad (total=litrate Violencia_48_a_53) $geo _Ic*, robust cluster(coddepto)
eststo: ivprobit d_coca informalidad (total=litrate Violencia_48_a_53) $geo $pol _Ic*, robust cluster(coddepto)
eststo: ivprobit d_coca informalidad (total=litrate Violencia_48_a_53) $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: ivprobit d_coca informalidad (total=litrate Violencia_48_a_53) $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/Table_6.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2_p,  fmt(%9.0f %9.3f) labels("Observations" "Pseudo-R2" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad total litrate Violencia_48_a_53) order(litrate Violencia_48_a_53 informalidad total) indicate("Province Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")
*/

*eststo: ivprobit d_coca informalidad (total=litrate) intarc $geo $land $pol $social _Ic*, robust cluster(coddepto)
*esttab using "results/Table_5.tex", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2_p,  fmt(%9.0f %9.3f) labels("Observations" "Pseudo-R2" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad total litrate) order(litrate  informalidad ) indicate("Province Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

*Prueba RD
*xtile inf_2=informalidad, nq(50)
*collapse (mean) informalidad  H_coca D_Coca (sum) d_coca, by(inf_2)
*rd D_Coca inf_2, gr mbw(100 50 200) z0(35) x($geo $land $pol $social _Ic*)


 
