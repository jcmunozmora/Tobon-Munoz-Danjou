** Do institutions affect the expansion of illicit crops? Empirical evidence from Colombia
** Université catholique de Louvain
** Santiago Tobón Zapata
** Original data: V1_informality.dta
** July, 2012

cap restore
clear all
set mem 100m
set more off
set logtype smcl
set matsize 800
pause on

**** WORKING DIRECTORY

cd "/Users/santiagotobon/Documents/UCL/Thesis/Data_Final/"
use "data/V1_informality.dta", clear
*use "data/informality_2012.dta", clear
*global mysintaxis "/data/mysintaxis"
**		Revisar uso de dataset con Antioquia y mysintaxis

**** DATASET

**** Drop years after 2008

drop if year>2008
	
**** Determining investment per capita in each municipality

foreach i in inv_salud inv_vivienda inv_educacion inv_vias inv_agrario inv_justicia {
gen `i'_cp=`i'/pobla
}

sort codmpio year
xtset codmpio year
gen coca_prop = hcoca / areasuphe

**** Creating new dataset with summary data
 
*collapse (mean) pobreza aniosestudioprom discapital dismdo inv_salud_cp inv_vivienda_cp inv_educacion_cp inv_vias_cp inv_agrario_cp inv_justicia_cp g_terreno g_prop g_uaf RSsisben inv_per_docente inv_calidad petotalins htapropietario ba_nu partelectoral prod_oro prod_plata H_coca areasuphe informalidad areasupkm altura indrural gandina goriental gpacifica gcaribe tasa_homicidios Violencia_48_a_53 desplazados total ataques (sum) D_Coca, by(codmpio coddepto)
*gen d_coca=(D_Coca!=0)

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
label variable total "Homicide rate (per 100000 habitants)"

**** Vectors of final covariates

*global geo "altura areasupkm gandina gpacifica gcaribe dismdo"
*global geo "altura areasupkm dismdo"
global pol "inv_educacion_cp inv_justicia_cp ba_nu"
global land "g_uaf htapropietario"
global social "RSsisben aniosestudioprom"
*global social "RSsisben pobreza aniosestudioprom"
*qui include $mysintaxis/myttests

xtsum coca_prop informalidad
correlate coca_prop informalidad tasa_homicidios

** REGRESIONES

* Year Fixed Effects

xi i.year

*Estimations

eststo clear
eststo: xtreg coca_prop informalidad, cluster(coddepto) fe i(codmpio)
estadd scalar r22=e(r2_w)
eststo: xtreg coca_prop informalidad $pol, cluster(coddepto) fe i(codmpio)
estadd scalar r22=e(r2_w)
eststo: xtreg coca_prop informalidad $pol $land, cluster(coddepto) fe i(codmpio)
estadd scalar r22=e(r2_w)
eststo: xtreg coca_prop informalidad $pol $land $social, cluster(coddepto) fe i(codmpio)
estadd scalar r22=e(r2_w)
esttab using "results/Table_Panel.tex", star(* 0.10 ** 0.05 *** 0.01) nomtitles replace brackets label se compress nogaps stats(N r22 chi2, labels("Observations" "R square" "Chi-Square")) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad) order(informalidad) indicate("Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social") title("Static Panel Data Model")

*** END



/********** VERSIÓN NUEVA, CON VARIABLE TASA_HOMICIDIOS

**** Populating the Dataset with departamental and national means

foreach i in informalidad tasa_homicidios $geo $land $pol $social {
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

**** First test for Regression Discontinuity Design

xtile inf_2=informalidad, nq(50)
preserve
collapse (mean) informalidad  H_coca D_Coca (sum) d_coca, by(inf_2)
*rd D_Coca inf_2, gr mbw(100 50 200) z0(35)
rd D_Coca inf_2, gr mbw(100) z0(35)
graph export "graphs/g_rd.png", width(3000) replace
restore

**** Descriptive Statistics (Graph I)

xtile inf_1=informalidad, nq(100)
preserve
gen pert_co= H_coca/areasuphe
collapse (mean) informalidad  H_coca D_Coca (sum) d_coca, by(inf_1)
twoway (lfit D_Coca inf_1) (scatter informalidad inf_1, yaxis(2) mcolor(gs1)), ytitle("Number of years (mean by quantile)", axis(1)) ytitle("Informalty Index", axis(2)) plotregion(fcolor(white)) graphregion(fcolor(white)) xtitle("Informalty Index's quantile")  legend(lab(1 "Number of years with coca crops") lab(2 "Informality Index"))
graph export "graphs/g_1.pdf", width(3000) replace
restore

**** REGRESSIONS CON LITRATE

**** Model II (Baseline Probit)

xi i.coddepto
eststo clear
eststo: dprobit d_coca informalidad _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad tasa_homicidios _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: dprobit d_coca informalidad $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/Table_2.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2_p,  fmt(%9.0f %9.3f) labels("Observations" "Pseudo-R2" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad) order(informalidad) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

**** Model III (Probit with Violence (Homicide Rate))

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
esttab using "results/Table_3.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2_p,  fmt(%9.0f %9.3f) labels("Observations" "Pseudo-R2" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad tasa_homicidios intarc) order(informalidad tasa_homicidios intarc) indicate("Department Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")

**** Model IV (IV-Probit Violence (Homicide Rate) instrumented with literacy rate)
sort codmpio 
merge 1:1 codmpio using "data/inst_usar.dta"
drop if _merge==2

* First Step
eststo clear
eststo: reg tasa_homicidios litrate informalidad _Ic*, robust cluster(coddepto)
eststo: reg tasa_homicidios litrate informalidad $geo _Ic*, robust cluster(coddepto)
eststo: reg tasa_homicidios litrate informalidad $geo $pol _Ic*, robust cluster(coddepto)
eststo: reg tasa_homicidios litrate informalidad $geo $land $pol _Ic*, robust cluster(coddepto)
eststo: reg tasa_homicidios litrate informalidad $geo $land $pol $social _Ic*, robust cluster(coddepto)
esttab using "results/Table_4a.csv", star(* 0.10 ** 0.05 *** 0.01) replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N r2 F ,  fmt(%9.0f %9.3f) labels("Observations" "R2" "F statistic" )) addnotes("Robust standard error estimated by cluster per Department.") keep(litrate informalidad ) order(litrate informalidad ) indicate("Province Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social" )  title("First Step (IV Violence) ")

eststo clear
eststo: ivprobit d_coca informalidad (tasa_homicidios=litrate) _Ic*, robust cluster(coddepto)
estadd scalar wald=e(chi2_exog)
eststo: ivprobit d_coca informalidad (tasa_homicidios=litrate) $geo _Ic*, robust cluster(coddepto)
estadd scalar wald=e(chi2_exog)
eststo: ivprobit d_coca informalidad (tasa_homicidios=litrate) $geo $pol _Ic*, robust cluster(coddepto)
estadd scalar wald=e(chi2_exog)
eststo: ivprobit d_coca informalidad (tasa_homicidios=litrate) $geo $land $pol _Ic*, robust cluster(coddepto)
estadd scalar wald=e(chi2_exog)
eststo: ivprobit d_coca informalidad (tasa_homicidios=litrate) $geo $land $pol $social _Ic*, robust cluster(coddepto)
estadd scalar wald=e(chi2_exog)
esttab using "results/Table_4.csv", star(* 0.10 ** 0.05 *** 0.01) margin replace nomtitles brackets label se(%9.3f) b(%9.3f) compress nogaps stats(N wald,  fmt(%9.0f %9.3f) labels("Observations" "Wald chi-squared test of exogeneity" )) addnotes("Robust standard error estimated by cluster per Department.") keep(informalidad tasa_homicidios litrate) order(litrate informalidad tasa_homicidios) indicate("Province Fixed Effect=_Ic*" "Geographic Controls=$geo" "Land Controls=$land" "Political Controls=$pol" "Socioeconomic Controls=$social")  title("probabilistic model (Probit) ")


/**** END ----- OTHER MODELS NOT CONSIDERED

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


 
