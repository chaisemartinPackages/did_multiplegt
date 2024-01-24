*﻿*Author: Clément de Chaisemartin
**1st version: November 8th 2019
**This version: May 2nd 2023
** Adding double at lines 214 and 980.

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///// Program #1: Does sanity checks and time consuming data manipulations, calls did_multiplegt_results, and stores estimates and standard errors in e() and puts them on a graph //////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


capture program drop did_multiplegt
program did_multiplegt, eclass 
	version 12.0
	syntax varlist(min=4 numeric) [if] [in]  [, RECAt_treatment(varlist numeric) THRESHold_stable_treatment(real 0) trends_nonparam(varlist numeric) trends_lin(varlist numeric) controls(varlist numeric) weight(varlist numeric) placebo(integer 0) dynamic(integer 0) breps(integer 50) cluster(varlist numeric) covariances AVerage_effect SAVe_results(string) robust_dynamic switchers(string) LONGdiff_placebo FIRSTdiff_placebo count_switchers_tot count_switchers_contr graphoptions(string) discount(real 1) seed(integer 0) JOINTtestplacebo if_first_diff(string) drop_larger_lower always_trends_nonparam always_trends_lin]
qui{

preserve

** AJOUT MELITINE **************************************************************

// Making covariances the option specified by default
if "`covariances'"=="" local covariances "covariances"

// Computing the jointtestplacebo by default
if "`breps'"!="0"&"`covariances'"!=""&"`jointtestplacebo'"==""&`placebo'>1 local jointtestplacebo "jointtestplacebo"

// Requesting average effect by default when robust_dynamic specified and at least one dynamic effect requested
if "`robust_dynamic'"!=""&`dynamic'>0 local average_effect "average_effect"

// If user did not specify neither firstdiff_placebo, longdiff_placebo, neither robust_dynamic, specify firstdiff_placebo.
if "`placebo'"!="0"&"`firstdiff_placebo'"==""&"`longdiff_placebo'"==""&"`robust_dynamic'"=="" local firstdiff_placebo "firstdiff_placebo"


// Checking the necessary packages are installed when using some options, and if not, install them
if "`controls'"!=""|"`trends_nonparam'"!=""|"`trends_lin'"!=""{
cap which ftools
if _rc ssc install ftools
cap which reghdfe
if _rc ssc install reghdfe
cap which moremata.hlp
if _rc ssc install moremata	
}
********************************************************************************

// Globals determining whether we see the results from all the intermediate regressions 

*global no_header_no_table "vce(ols)"
global no_header_no_table "nohea notab"

*global noisily "noisily"
global noisily ""

// Dropping variables that get created later

capture drop outcome_XX
capture drop group_XX
capture drop time_XX
capture drop treatment_XX
** AJOUT MELITINE **************************************************************
capture drop d_sq_XX
********************************************************************************
capture drop D_cat_XX
capture drop d_cat_group_XX
capture drop diff_y_XX
capture drop diff_d_XX
capture drop ZZ_*
capture drop lag_d_cat_group_XX
** AJOUT MELITINE **************************************************************
capture drop diff_from_sq_XX
capture drop ever_strict_increase_XX
capture drop ever_strict_decrease_XX
********************************************************************************

** AJOUT MELITINE **************************************************************
// Dropping matrices which will be created on Mauricio's advice
local didmgt_matrices: all matrices "didmgt_*"
if `:list sizeof didmgt_matrices' matrix drop `didmgt_matrices'
********************************************************************************

// Performing sanity checks on command requested by user

if "`if'" !=""{
did_multiplegt_check `varlist' `if', recat_treatment(`recat_treatment') threshold_stable_treatment(`threshold_stable_treatment') trends_nonparam(`trends_nonparam') trends_lin(`trends_lin') controls(`controls') weight(`weight') placebo(`placebo') dynamic(`dynamic') breps(`breps') cluster(`cluster') `covariances' `average_effect' `longdiff_placebo' `firstdiff_placebo' `robust_dynamic' if_first_diff(`if_first_diff') `drop_larger_lower' `always_trends_nonparam' `always_trends_lin'
}

if "`if'"==""{
did_multiplegt_check `varlist', recat_treatment(`recat_treatment') threshold_stable_treatment(`threshold_stable_treatment') trends_nonparam(`trends_nonparam') trends_lin(`trends_lin') controls(`controls') weight(`weight') placebo(`placebo') dynamic(`dynamic') breps(`breps') cluster(`cluster') `covariances' `average_effect' `longdiff_placebo' `firstdiff_placebo' `robust_dynamic' if_first_diff(`if_first_diff') `drop_larger_lower' `always_trends_nonparam' `always_trends_lin'
}

if did_multiplegt_check==1 {

// Selecting the sample
	if "`if'" !=""{
	keep `if'
	}
	if "`weight'" !=""{
	drop if `weight'==.
	}	
	tokenize `varlist'
	drop if `2'==.|`3'==.|`4'==.
	if "`controls'" !=""{
	foreach var of varlist `controls'{
	drop if `var'==.
	}
	}
	if "`cluster'" !=""{
	drop if `cluster'==.
	}
	if "`recat_treatment'" !=""{
	drop if `recat_treatment'==.
	}
	if "`weight'" !=""{
	drop if `weight'==.
	}	
	

// If the weight option is not specified, collapse data set at the (g,t) level. 

tempvar counter counter2


bys `2' `3': egen `counter2'=count(`4')
sum `counter2'
scalar aggregated_data=0
if r(max)==1{
scalar aggregated_data=1
}

if "`weight'"==""{
gen `counter'=1
}
if "`weight'"!=""{
gen `counter'=`weight'
}

if aggregated_data==0{

replace `counter'=0 if `1'==. 

//Collapsing the data, ensuring that group variable is not the clustering or the trend_lin variable

if "`1'"!="`4'"{

if ("`cluster'"!=""&"`cluster'"!="`2'")&("`trends_lin'"!=""&"`trends_lin'"!="`2'"){	
collapse (mean) `1' `4' `controls' `trends_nonparam' `trends_lin' `cluster' `recat_treatment' (count) `counter' [pw=`counter'], by(`2' `3')
}

if ("`cluster'"==""|"`cluster'"=="`2'")&("`trends_lin'"!=""&"`trends_lin'"!="`2'"){
collapse (mean) `1' `4' `controls' `trends_nonparam' `trends_lin' `recat_treatment' (count) `counter' [pw=`counter'], by(`2' `3')
}

if ("`cluster'"!=""&"`cluster'"!="`2'")&("`trends_lin'"==""|"`trends_lin'"=="`2'"){
collapse (mean) `1' `4' `controls' `trends_nonparam' `cluster' `recat_treatment' (count) `counter' [pw=`counter'], by(`2' `3')
}

if ("`cluster'"==""|"`cluster'"=="`2'")&("`trends_lin'"==""|"`trends_lin'"=="`2'"){
collapse (mean) `1' `4' `controls' `trends_nonparam' `recat_treatment' (count) `counter' [pw=`counter'], by(`2' `3')
}

}

if "`1'"=="`4'"{

if ("`cluster'"!=""&"`cluster'"!="`2'")&("`trends_lin'"!=""&"`trends_lin'"!="`2'"){
collapse (mean) `1' `controls' `trends_nonparam' `trends_lin' `cluster' `recat_treatment' (count) `counter' [pw=`counter'], by(`2' `3')
}

if ("`cluster'"==""|"`cluster'"=="`2'")&("`trends_lin'"!=""&"`trends_lin'"!="`2'"){
collapse (mean) `1' `controls' `trends_nonparam' `trends_lin' `recat_treatment' (count) `counter' [pw=`counter'], by(`2' `3')
}

if ("`cluster'"!=""&"`cluster'"!="`2'")&("`trends_lin'"==""|"`trends_lin'"=="`2'"){
collapse (mean) `1' `controls' `trends_nonparam' `cluster' `recat_treatment' (count) `counter' [pw=`counter'], by(`2' `3')
}

if ("`cluster'"==""|"`cluster'"=="`2'")&("`trends_lin'"==""|"`trends_lin'"=="`2'"){
collapse (mean) `1' `controls' `trends_nonparam' `recat_treatment' (count) `counter' [pw=`counter'], by(`2' `3')
}

}

}



** AJOUT MELITINE **************************************************************
// Dropping all the (g,t)s, such that at t, group g has experienced both a strictly larger and a strictly lower treatment than its period-one treatment.
gen d_sq_XX=`4'
sort `2' `3'
replace d_sq_XX=d_sq_XX[_n-1] if `2'==`2'[_n-1]
	
	if "`robust_dynamic'"!=""&"`drop_larger_lower'"!=""{
	
	gen diff_from_sq_XX=(`4'-d_sq_XX)
	sort `2' `3'
	gen ever_strict_increase_XX=(diff_from_sq_XX>0&abs(diff_from_sq_XX)>`threshold_stable_treatment') 
	replace ever_strict_increase_XX=1 if ever_strict_increase_XX[_n-1]==1&`2'==`2'[_n-1]

	gen ever_strict_decrease_XX=(diff_from_sq_XX<0&abs(diff_from_sq_XX)>`threshold_stable_treatment') 
	replace ever_strict_decrease_XX=1 if ever_strict_decrease_XX[_n-1]==1&`2'==`2'[_n-1]

	drop if ever_strict_increase_XX==1 & ever_strict_decrease_XX==1
	
	drop ever_strict_increase_XX ever_strict_decrease_XX
	
	}
********************************************************************************	
	
// Creating all the variables needed for estimation of instantaneous effect

*Y, G, T, D variables

gen outcome_XX=`1'
egen double group_XX=group(`2')
egen time_XX=group(`3')
gen treatment_XX=`4'

*Creating a discretized treatment even if recat_treatment option not specified

if "`recat_treatment'" !=""{
gen D_cat_XX=`recat_treatment'
}
else{
gen D_cat_XX=treatment_XX
}

*Creating groups of recategorized treatment, to ensure we have an ordered treatment with interval of 1 between consecutive values

egen d_cat_group_XX=group(D_cat_XX)

*Declaring data set as panel

xtset group_XX time_XX

*First diff outcome, treatment, and controls

g diff_y_XX = d.outcome_XX
g diff_d_XX = d.treatment_XX

if "`controls'" !=""{
local count_controls=0
foreach var of varlist `controls'{
local count_controls=`count_controls'+1
gen ZZ_cont`count_controls'=d.`var'
}
}

*Lag D_cat

g lag_d_cat_group_XX = L1.d_cat_group_XX


capture drop stag_d_XX
capture drop ever_change_d_XX
capture drop diff_stag_d_XX
capture drop tot_d_XX
capture drop increase_d_XX
capture drop ev_inc_d_XX
capture drop ev_dec_d_XX

gen stag_d_XX=treatment_XX
// Note: important to ensure that creation of ever_change_d_XX and diff_stag_d_XX correct even with unbalanced panel of groups. 
// Conventions used: 
// a) if a group is missing at a date t, but treatment in t-1 and t+1 are equal, and had never changed treatment before
//t-1, considered as group that has never changed till t+1 at least: ever_change_d_XX=0 in t+1, so can be used as control, 
// and we also set diff_stag_d_XX=0 in t so can be used as control in t
// b) if a group is missing at a date t, treatments in t-1 and t+1 are different, and had never changed treatment before
//t-1, we set diff_stag_d_XX=. and that group is essentially dropped from the estimation after t-1, because we don't know when treatment changed. 


 if "`robust_dynamic'"!=""{
** RETRAIT MELITINE
//gen d_sq_XX=treatment_XX
sort group_XX time_XX
gen ever_change_d_XX=(abs(treatment_XX-treatment_XX[_n-1])>`threshold_stable_treatment') if treatment_XX!=.&treatment_XX[_n-1]!=.&group_XX==group_XX[_n-1]
replace ever_change_d_XX=1 if ever_change_d_XX[_n-1]==1 & group_XX==group_XX[_n-1]
replace stag_d_XX=stag_d_XX[_n-1] if ever_change_d_XX==1 & ever_change_d_XX[_n-1]==1&group_XX==group_XX[_n-1]
** RETRAIT MELITINE
//replace d_sq_XX=d_sq_XX[_n-1] if group_XX==group_XX[_n-1]

gen tot_d_XX=(treatment_XX-d_sq_XX)*`counter'*`discount'^time_XX 

bys group_XX: egen increase_d_XX=total(tot_d_XX)
replace  increase_d_XX=(increase_d_XX>0)
}

xtset group_XX time_XX
g diff_stag_d_XX = d.stag_d_XX


 if "`robust_dynamic'"!=""{
replace diff_stag_d_XX=0 if ever_change_d_XX==0
}


if "`robust_dynamic'"==""{
gen ever_change_d_XX=0 if diff_d_XX!=.
gen increase_d_XX=(diff_d_XX>0) if diff_d_XX!=.
}





// If placebos requested, creating all the variables needed for estimation of placebos

** MODIFICATION MELITINE ** "`firstdiff_placebo'"!="" remplace "`longdiff_placebo'"==""
if "`placebo'"!="0"&"`firstdiff_placebo'"!=""{

forvalue i=1/`=`placebo''{

*Lag First diff outcome, treatment, and controls
capture drop diff_d_lag`i'_XX
capture drop diff_y_lag`i'_XX
g diff_d_lag`i'_XX = L`i'.diff_d_XX
g diff_y_lag`i'_XX = L`i'.diff_y_XX

if "`controls'" !=""{
forvalue j=1/`=`count_controls''{
gen ZZ_cont_lag`i'_`j'=L`i'.ZZ_cont`j'
}

}

}

}

** MODIFICATION MELITINE ** "`firstdiff_placebo'"=="" remplace "`longdiff_placebo'"!=""
if "`placebo'"!="0"&"`firstdiff_placebo'"==""{

forvalue i=1/`=`placebo''{

*Long diff outcome and controls, and if dynamic effects not requested: forward of first diff treatment

capture drop ldiff_y_`i'_XX
capture drop ldiff_y_for`i'_XX
g ldiff_y_`i'_XX = S`=`i''.outcome_XX
g ldiff_y_`i'_lag_XX =L.ldiff_y_`i'_XX
drop ldiff_y_`i'_XX 

if "`controls'" !=""{

local count_controls=0
foreach var of varlist `controls'{
local count_controls=`count_controls'+1
g ZZ_cont_ldiff`i'_`count_controls'=S`=`i''.`var'
}
forvalue j=1/`=`count_controls''{
g  ZZ_cont_ldiff_`i'_lag_`j'=L.ZZ_cont_ldiff`i'_`j'
drop ZZ_cont_ldiff`i'_`j'
}

}

if "`dynamic'"=="0" {

capture drop diff_stag_d_for`i'_XX
g diff_stag_d_for`i'_XX = F`i'.diff_stag_d_XX
capture drop counter_F`i'_XX
g counter_F`i'_XX=F`i'.`counter'

}

}
}


// If dynamic effects requested, creating all the variables needed for estimation of dynamic effects

if "`dynamic'"!="0"{

forvalue i=1/`=`dynamic''{

*Long diff outcome, long diff treatment, forward of first diff treatment, and long diff controls

capture drop diff_stag_d_for`i'_XX
g diff_stag_d_for`i'_XX = F`i'.diff_stag_d_XX
capture drop counter_F`i'_XX
g counter_F`i'_XX=F`i'.`counter' // F is for 'forward' - used in the case where there are 
//several treatments ?

capture drop ldiff_y_`i'_XX
capture drop ldiff_y_for`i'_XX
g ldiff_y_`i'_XX = S`=`i'+1'.outcome_XX
g ldiff_y_for`i'_XX =F`i'.ldiff_y_`i'_XX 

capture drop ldiff_d_`i'_XX
capture drop ldiff_d_for`i'_XX
g ldiff_d_`i'_XX = S`=`i'+1'.treatment_XX
g ldiff_d_for`i'_XX =F`i'.ldiff_d_`i'_XX

if "`controls'" !=""{

local count_controls=0
foreach var of varlist `controls'{
local count_controls=`count_controls'+1
g ZZ_cont_ldiff`i'_`count_controls'=S`=`i'+1'.`var'
}
forvalue j=1/`=`count_controls''{
g  ZZ_cont_ldiff_for`i'_`j'=F`i'.ZZ_cont_ldiff`i'_`j'
}

}

}

}

//Replace controls by their first difference 

if "`controls'" !=""{
local count_controls=1
foreach var of varlist `controls'{
replace `var'=ZZ_cont`count_controls'
local count_controls=`count_controls'+1
}
}

//Creating trends_var if needed

if "`trends_nonparam'" !=""{
egen long trends_var_XX=group(`trends_nonparam' time_XX)
}

if "`if_first_diff'"!=""{
gen if_first_diff_XX=(`if_first_diff')
replace diff_stag_d_XX=. if if_first_diff_XX==0
}


// Run did_multiplegt_results.

did_multiplegt_results `varlist', threshold_stable_treatment(`threshold_stable_treatment') trends_nonparam(`trends_nonparam') trends_lin(`trends_lin') controls(`controls') counter(`counter') placebo(`placebo') dynamic(`dynamic') breps(`breps') cluster(`cluster') switchers(`switchers') `longdiff_placebo' `firstdiff_placebo' `count_switchers_tot' `count_switchers_contr' `robust_dynamic' discount(`discount') seed(`seed') `drop_larger_lower' `always_trends_nonparam' `always_trends_lin'

// Compute standard errors of point estimates 

if `breps'>0 {

drop _all

svmat didmgt_bootstrap
sum didmgt_bootstrap1
scalar se_effect_0_2=r(sd)
forvalue i=1/`dynamic'{
sum didmgt_bootstrap`=`i'+1'
scalar se_effect_`i'_2=r(sd)
}
forvalue i=1/`placebo'{
sum didmgt_bootstrap`=`i'+`dynamic'+1'
scalar se_placebo_`i'_2=r(sd)
}

}

// Clearing ereturn

ereturn clear

// Error message if instantaneous effect could not be estimated

if "`count_switchers_tot'"!=""{
ereturn scalar N_switchers_effect_0_tot=N_switchers_effect_0_tot_2
}
if N_effect_0_2==.{
di as error ""
di as error "The command was not able to estimate the treatment effect at the period when groups' treatment change." 
di as error "If your treatment is continuous or takes a large number of values, you may need to use" 
di as error "the threshold_stable_treatment option to ensure you have groups whose treatment does not change" 
di as error "over time. You may also need to use the recat_treatment option to discretize your treatment variable."
matrix didmgt_effect0=.,.,.,.,.,.
}

// If instantaneous effect could be estimated, collect estimate and number of observations 

else {
ereturn scalar effect_0 = effect_0_2
if `breps'>0 {
ereturn scalar se_effect_0 = se_effect_0_2
matrix didmgt_effect0=effect_0_2,se_effect_0_2,effect_0_2-1.96*se_effect_0_2,effect_0_2+1.96*se_effect_0_2,N_effect_0_2,N_switchers_effect_0_2
}
ereturn scalar N_effect_0 = N_effect_0_2
ereturn scalar N_switchers_effect_0 = N_switchers_effect_0_2
if "`count_switchers_contr'"!=""&("`trends_lin'"!=""|"`trends_nonparam'"!=""){
ereturn scalar N_switchers_effect_0_contr=N_switchers_effect_0_contr_2
}
}

// If dynamic effects requested, collect estimates and number of observations

if "`dynamic'"!="0"{

*Looping over the number of dynamic effects requested

forvalue i=1/`=`dynamic''{

// Error message if dynamic effect i could not be estimated

if "`count_switchers_tot'"!=""{
ereturn scalar N_switchers_effect_`i'_tot=N_switchers_effect_`i'_tot_2
}
if N_effect_`i'_2==.{
di as error ""
di as error "The command was not able to estimate the treatment effect "`i' 
di as error "periods after groups' treatment changes for the first time." 
di as error "If your treatment is continuous or takes a large number of values, you may need to use" 
di as error "the threshold_stable_treatment option to ensure you have groups whose treatment does not change" 
di as error "over time. You may also need to use the recat_treatment option to discretize your treatment variable."
di as error "You may also be trying to estimate more dynamic effects than it is possible to do in your data."
matrix didmgt_effect`i'=.,.,.,.,.,.
}

// If dynamic effect i could be estimated, collect estimate and number of observations 

else {
ereturn scalar effect_`i' = effect_`i'_2
if `breps'>0 {
ereturn scalar se_effect_`i' = se_effect_`i'_2
matrix didmgt_effect`i'=effect_`i'_2,se_effect_`i'_2,effect_`i'_2-1.96*se_effect_`i'_2,effect_`i'_2+1.96*se_effect_`i'_2,N_effect_`i'_2,N_switchers_effect_`i'_2
}
ereturn scalar N_effect_`i' = N_effect_`i'_2
ereturn scalar N_switchers_effect_`i' = N_switchers_effect_`i'_2
if "`count_switchers_contr'"!=""&("`trends_lin'"!=""|"`trends_nonparam'"!=""){
ereturn scalar N_switchers_effect_`i'_contr=N_switchers_effect_`i'_contr_2
}
}

*End of the loop on the number of dynamic effects
}

*End of the condition assessing if the computation of dynamic effects was requested by the user
}

// If placebos requested, collect estimates and number of observations

if "`placebo'"!="0"{

*Looping over the number of placebos requested

forvalue i=1/`=`placebo''{

// Error message if placebo i could not be estimated

if "`count_switchers_tot'"!=""{
ereturn scalar N_switchers_placebo_`i'_tot=N_switchers_placebo_`i'_tot_2
}
if N_placebo_`i'_2==.{
di as error ""
di as error "The command was not able to estimate the placebo "`i' 
di as error "periods before groups' treatment changes." 
di as error "If your treatment is continuous or takes a large number of values, you may need to use" 
di as error "the threshold_stable_treatment option to ensure you have groups whose treatment does not change" 
di as error "over time. You may also need to use the recat_treatment option to discretize your treatment variable."
di as error "You may also be trying to estimate more placebos than it is possible to do in your data."
matrix didmgt_placebo`i'=.,.,.,.,.,.

}

// If placebo i could be estimated, collect estimate and number of observations 

else {
ereturn scalar placebo_`i' = placebo_`i'_2
if `breps'>0 {
ereturn scalar se_placebo_`i' = se_placebo_`i'_2
matrix didmgt_placebo`i'=placebo_`i'_2,se_placebo_`i'_2,placebo_`i'_2-1.96*se_placebo_`i'_2,placebo_`i'_2+1.96*se_placebo_`i'_2,N_placebo_`i'_2,N_switchers_placebo_`i'_2
}
ereturn scalar N_placebo_`i' = N_placebo_`i'_2
ereturn scalar N_switchers_placebo_`i' = N_switchers_placebo_`i'_2
if "`count_switchers_contr'"!=""&("`trends_lin'"!=""|"`trends_nonparam'"!=""){
ereturn scalar N_switchers_placebo_`i'_contr=N_switchers_placebo_`i'_contr_2
}
}

*End of the loop on the number of placebos
}

*End of the condition assessing if the computation of placebos was requested by the user
}

// If dynamic effects or placebos requested and covariance option specified, compute covariances between all estimated effects

scalar too_many_dynamic_or_placebo=0
scalar too_few_bootstrap_reps=0

if `breps'>0&"`covariances'"!=""{

if "`dynamic'"!="0"{

forvalue i=0/`dynamic'{
forvalue j=`=`i'+1'/`dynamic'{
capture correlate didmgt_bootstrap`=`i'+1' didmgt_bootstrap`=`j'+1', covariance
if _rc==2000{
scalar too_many_dynamic_or_placebo=1
}
if _rc==2001{
scalar too_few_bootstrap_reps=1
}
if _rc!=2000&_rc!=2001{
ereturn scalar cov_effects_`i'_`j'=r(cov_12)
scalar cov_effects_`i'_`j'_int=r(cov_12)
scalar cov_effects_`j'`i'_int=r(cov_12)
}
}
}

}


if `placebo'>1{

forvalue i=1/`placebo'{
forvalue j=`=`i'+1'/`placebo'{
capture correlate didmgt_bootstrap`=`i'+`dynamic'+1' didmgt_bootstrap`=`j'+`dynamic'+1', covariance
if _rc==2000{
scalar too_many_dynamic_or_placebo=1
}
if _rc==2001{
scalar too_few_bootstrap_reps=1
}
if _rc!=2000&_rc!=2001{
ereturn scalar cov_placebo_`i'_`j'=r(cov_12)
scalar cov_placebo_`i'_`j'_int=r(cov_12)
}
}
}

}

}


/////// Error messages if too_many_dynamic_or_placebo or too_few_bootstrap_reps

if too_many_dynamic_or_placebo==1 {
di as error ""
di as error "The command was not able to run till the end, presumably because it could not" 
di as error "estimate all placebos and dynamic effects requested. Estimates are stored in e()," 
di as error "so you can type ereturn list to see which placebos and dynamic effects the command"
di as error "could estimate. To solve this problem, you will probably have to diminish the number"
di as error "of placebos or dynamic effects requested. See the help file for more information on the"
di as error "maximum number of dynamic effects and placebos the command can compute."
}

if too_few_bootstrap_reps==1 {
di as error ""
di as error "The command was not able to run till the end, presumably because it could not" 
di as error "compute sufficiently many bootstrap replications for some of the placebos and dynamic" 
di as error "effects requested. To solve this problem, you will probably have to increase the number"
di as error "of bootstrap replications."
}

if too_few_bootstrap_reps==0&too_many_dynamic_or_placebo==0{


/////// Computing average effect, if option requested

if "`average_effect'"!=""{

scalar average_effect_int=0
scalar var_average_effect_int=0
scalar N_average_effect_int=0
scalar N_switch_average_effect_int=0


//// Computing weights

scalar total_weight=0
matrix didmgt_Weight=J(`dynamic'+1,1,0)
forvalue i=0/`=`dynamic''{
matrix didmgt_Weight[`i'+1,1]=denom_DID_ell_`i'
scalar total_weight=total_weight+denom_delta_`i'
}
matrix didmgt_Weight=didmgt_Weight*(1/total_weight)


//// Computing average effect, its variance, and returning results 

forvalue i=0/`=`dynamic''{
scalar average_effect_int=average_effect_int+didmgt_Weight[`i'+1,1]*effect_`i'_2
scalar N_average_effect_int=N_average_effect_int+N_effect_`i'_2
scalar N_switch_average_effect_int=N_switch_average_effect_int+N_switchers_effect_`i'_2
if "`breps'"!="0"&"`covariances'"!=""{
scalar var_average_effect_int=var_average_effect_int+didmgt_Weight[`i'+1,1]^2*se_effect_`i'_2^2
if `i'<`dynamic'{
forvalue j=`=`i'+1'/`=`dynamic''{
scalar var_average_effect_int=var_average_effect_int+didmgt_Weight[`i'+1,1]*didmgt_Weight[`j'+1,1]*2*cov_effects_`i'_`j'_int
}
}
}
}

*Returning results

ereturn scalar effect_average=average_effect_int
if "`breps'"!="0"&"`covariances'"!=""{
ereturn scalar se_effect_average=sqrt(var_average_effect_int)
matrix didmgt_average=average_effect_int,sqrt(var_average_effect_int),average_effect_int-1.96*sqrt(var_average_effect_int),average_effect_int+1.96*sqrt(var_average_effect_int),N_average_effect_int,N_switch_average_effect_int
}
ereturn scalar N_effect_average=N_average_effect_int
ereturn scalar N_switchers_effect_average=N_switch_average_effect_int
}

///// Running joint test that placebos all 0, if jointtestplacebo option specified

if "`breps'"!="0"&"`covariances'"!=""&"`jointtestplacebo'"!=""&`placebo'>1{
	
matrix didmgt_Placebo=J(`placebo',1,0)
matrix didmgt_Var_Placebo=J(`placebo',`placebo',0)
forvalue i=1/`placebo'{
matrix didmgt_Placebo[`i',1]=placebo_`i'_2
scalar cov_placebo_`i'_`i'_int=se_placebo_`i'_2^2
forvalue j=1/`i'{
matrix didmgt_Var_Placebo[`i',`j']=cov_placebo_`j'_`i'_int
}
if `i'<`placebo'{
forvalue j=`=`i'+1'/`placebo'{
matrix didmgt_Var_Placebo[`i',`j']=cov_placebo_`i'_`j'_int
}
}
}

matrix didmgt_Var_Placebo_inv=invsym(didmgt_Var_Placebo)
matrix didmgt_Placebo_t=didmgt_Placebo'
matrix didmgt_chi2placebo=didmgt_Placebo_t*didmgt_Var_Placebo_inv*didmgt_Placebo
ereturn scalar p_jointplacebo=1-chi2(`placebo',didmgt_chi2placebo[1,1])
}

///// Putting estimates and their confidence intervals on a graph, if breps option specified

if "`breps'"!="0"{

local estimates_req=3+`placebo'+`dynamic'

if `breps'<`estimates_req' {
set obs `estimates_req'
}

gen time_to_treatment=.
gen treatment_effect=.
gen se_treatment_effect=.
gen N_treatment_effect=.
gen treatment_effect_upper_95CI=.
gen treatment_effect_lower_95CI=.

if "`placebo'"!="0"{
forvalue i=1/`=`placebo''{
replace time_to_treatment=-`i'-1 if _n==`placebo'-`i'+1
replace treatment_effect=placebo_`i'_2 if _n==`placebo'-`i'+1
replace se_treatment_effect=se_placebo_`i'_2 if _n==`placebo'-`i'+1
replace N_treatment_effect=N_placebo_`i'_2 if _n==`placebo'-`i'+1
replace treatment_effect_upper_95CI=placebo_`i'_2+1.96*se_placebo_`i'_2 if _n==`placebo'-`i'+1
replace treatment_effect_lower_95CI=placebo_`i'_2-1.96*se_placebo_`i'_2 if _n==`placebo'-`i'+1
}
}
replace time_to_treatment=-1 if _n==`placebo'+1
replace treatment_effect=0 if _n==`placebo'+1 
replace treatment_effect_upper_95CI=0 if _n==`placebo'+1 
replace treatment_effect_lower_95CI=0 if _n==`placebo'+1 
 
replace time_to_treatment=0 if _n==`placebo'+2
replace treatment_effect=effect_0_2 if _n==`placebo'+2
replace se_treatment_effect=se_effect_0_2 if _n==`placebo'+2
replace N_treatment_effect=N_effect_0_2 if _n==`placebo'+2
replace treatment_effect_upper_95CI=effect_0_2+1.96*se_effect_0_2 if _n==`placebo'+2 
replace treatment_effect_lower_95CI=effect_0_2-1.96*se_effect_0_2 if _n==`placebo'+2 

if "`dynamic'"!="0"{
forvalue i=1/`=`dynamic''{
replace time_to_treatment=`i' if _n==`placebo'+`i'+2
replace treatment_effect=effect_`i'_2 if _n==`placebo'+`i'+2 
replace se_treatment_effect=se_effect_`i'_2 if _n==`placebo'+`i'+2
replace N_treatment_effect=N_effect_`i'_2 if _n==`placebo'+`i'+2
replace treatment_effect_upper_95CI=effect_`i'_2+1.96*se_effect_`i'_2 if _n==`placebo'+`i'+2 
replace treatment_effect_lower_95CI=effect_`i'_2-1.96*se_effect_`i'_2 if _n==`placebo'+`i'+2
}
}

/////Saving results in a data set, if option requested

if "`save_results'"!=""{

	if "`average_effect'"!=""{ 
	replace treatment_effect=average_effect_int if _n==`placebo'+`dynamic'+3 
	replace se_treatment_effect=sqrt(var_average_effect_int) if _n==`placebo'+`dynamic'+3
	replace N_treatment_effect=N_average_effect_int if _n==`placebo'+`dynamic'+3
	replace treatment_effect_upper_95CI=treatment_effect+1.96*se_treatment_effect if _n==`placebo'+`dynamic'+3 
	replace treatment_effect_lower_95CI=treatment_effect-1.96*se_treatment_effect if _n==`placebo'+`dynamic'+3  
	}
 	
keep time_to_treatment N_treatment_effect treatment_effect se_treatment_effect treatment_effect_upper_95CI treatment_effect_lower_95CI

if "`average_effect'"!=""{
keep if _n<=3+`placebo'+`dynamic'
}
if "`average_effect'"==""{
keep if _n<=2+`placebo'+`dynamic'
}

}

// Producing the graphs

** MODIFICATION MELITINE ** "`firstdiff_placebo'"!="" remplace "`longdiff_placebo'"==""
if "`firstdiff_placebo'"!=""&"`dynamic'"=="0"{

drop if time_to_treatment==-1
replace time_to_treatment=time_to_treatment+1 if time_to_treatment<0

if "`graphoptions'"==""{
twoway (connected treatment_effect time_to_treatment, lpattern(solid)) (rcap treatment_effect_upper_95CI treatment_effect_lower_95CI time_to_treatment), xlabel(`=-`placebo''[1]`=`dynamic'') xtitle("Relative time to period where treatment changes (t=0)", size(large)) title("DID, from period t-1 to t", size(large)) graphregion(color(white)) plotregion(color(white)) legend(off)
}
if "`graphoptions'"!=""{
global options="`graphoptions'"
twoway (connected treatment_effect time_to_treatment, lpattern(solid)) (rcap treatment_effect_upper_95CI treatment_effect_lower_95CI time_to_treatment), $options
}

}

** MODIFICATION MELITINE ** "`firstdiff_placebo'"=="" remplace "`longdiff_placebo'"!=""
if "`firstdiff_placebo'"==""{

if "`graphoptions'"==""{
twoway (connected treatment_effect time_to_treatment, lpattern(solid)) (rcap treatment_effect_upper_95CI treatment_effect_lower_95CI time_to_treatment), xlabel(`=-`placebo'-1'[1]`=`dynamic'') xtitle("Relative time to period where treatment first changes (t=0)", size(large)) title("DID, from last period before treatment changes (t=-1) to t", size(large)) graphregion(color(white)) plotregion(color(white)) legend(off)
}
if "`graphoptions'"!=""{
global options="`graphoptions'"
twoway (connected treatment_effect time_to_treatment, lpattern(solid)) (rcap treatment_effect_upper_95CI treatment_effect_lower_95CI time_to_treatment), $options
}

}

if "`save_results'"!=""{
save "`save_results'", replace
}

// End of the condition assessing if breps requested (so graph has to be produced)
}

// Producing a table

if `breps'>0{
matrix didmgt_results=didmgt_effect0
local rownames "Effect_0"
if "`dynamic'"!="0"{
forvalue i=1/`=`dynamic''{
matrix didmgt_results=didmgt_results \ didmgt_effect`i'
local rownames "`rownames' Effect_`i'"
}
}

** AJOUT MELITINE **************************************************************
** For compatibility with HonestDiD
matrix didmgt_results_no_avg = didmgt_results[1...,1]


if "`covariances'"!=""&"`jointtestplacebo'"!=""&`placebo'>1{
matrix didmgt_results_no_avg = didmgt_results_no_avg \ didmgt_Placebo
matrix didmgt_results_no_avg = didmgt_results_no_avg'
}
********************************************************************************

if "`average_effect'"!=""&"`covariances'"!=""{
matrix didmgt_results=didmgt_results \ didmgt_average
local rownames "`rownames' Average"
}
if "`placebo'"!="0"{
forvalue i=1/`=`placebo''{
matrix didmgt_results=didmgt_results \ didmgt_placebo`i'
local rownames "`rownames' Placebo_`i'"
}
}
matrix colnames didmgt_results  = "Estimate" "SE" "LB CI" "UB CI" "N" "Switchers"
matrix rownames didmgt_results= `rownames'
display _newline
di as input " "
display _newline
di as input "To estimate event-study/dynamic effects, "
di as input "we recommend using the much faster did_multiplegt_dyn command, available from the ssc repository."
noisily matlist didmgt_results, title("DID estimators of the instantaneous treatment effect, of dynamic treatment effects if the dynamic option is used, and of placebo tests of the parallel trends assumption if the placebo option is used. The estimators are robust to heterogeneous effects, and to dynamic effects if the robust_dynamic option is used.")
matrix didmgt_b2=didmgt_results[1...,1..1]
ereturn matrix didmgt_estimates=didmgt_b2
** AJOUT MELITINE **************************************************************
** The matrices returned for results and variances have been renamed "didmgt_estimates" and "didmgt_variances". But, for compatibility with old do files, we create the same matrices, names "estimates" and "variances".
matrix didmgt_b2=didmgt_results[1...,1..1]
ereturn matrix estimates=didmgt_b2
if "`average_effect'"!=""&"`covariances'"!=""{
matrix didmgt_var2=J(`placebo'+`dynamic'+2,1,0)
forvalue i=1/`=`placebo'+`dynamic'+2'{
matrix didmgt_var2[`i',1]=didmgt_results[`i',2]^2
}
}
if "`average_effect'"==""|"`covariances'"==""{
matrix didmgt_var2=J(`placebo'+`dynamic'+1,1,0)
forvalue i=1/`=`placebo'+`dynamic'+1'{
matrix didmgt_var2[`i',1]=didmgt_results[`i',2]^2
}
}
matrix rownames didmgt_var2= `rownames'
// For compatibility with old do files:
matrix compatibility_variances = didmgt_var2
ereturn matrix didmgt_variances=didmgt_var2
ereturn matrix variances= compatibility_variances
ereturn local cmd "did_multiplegt"
}
}

// End of the condition checking that not too_many_dynamic_or_placebo or too_few_bootstrap_reps

** AJOUT MELITINE **************************************************************
** For compatibility with HonestDiD
if `breps'>0 {
mata st_matrix("didmgt_vcov", variance(st_matrix("didmgt_bootstrap")))
}
********************************************************************************


}

restore

// End of the quietly condition
}

// Answers to FAQs


if `breps'==0{
di as text "When there are no more than 2 bootstrap replications, the command does not produce a table or a graph,"
di as text "but the estimators requested are stored as eclass objects. Type ereturn list to see them."
}
if `breps'>0{
** MODIFICATION MELITINE ** "`firstdiff_placebo'"!="" remplace "`longdiff_placebo'"==""
if "`firstdiff_placebo'"!=""&"`dynamic'"!="0"{
di as text ""
di as text "When dynamic effects and first-difference placebos are requested, the command does"
di as text "not produce a graph, because placebos estimators are DIDs across consecutive time periods,"
di as text "while dynamic effects estimators are long-difference DIDs, so they are not really comparable."
}
}
end


///////////////////////////////////////////////////////////////////
///// Program #2: does all the sanity checks before estimation ////
///////////////////////////////////////////////////////////////////

capture program drop did_multiplegt_check
program did_multiplegt_check, eclass
	version 12.0
	syntax varlist(min=4 max=4 numeric) [if] [in]  [, RECAT_treatment(varlist numeric) THRESHOLD_stable_treatment(real 0) trends_nonparam(varlist numeric) trends_lin(varlist numeric) controls(varlist numeric) weight(varlist numeric) placebo(integer 0) dynamic(integer 0) breps(integer 50) cluster(varlist numeric) covariances average_effect LONGdiff_placebo FIRSTdiff_placebo robust_dynamic if_first_diff(string) drop_larger_lower always_trends_nonparam always_trends_lin] 
preserve	
	
// Names of temporary variables
tempvar counter

// Initializing check

scalar did_multiplegt_check=1

// Selecting sample

	if "`if'" !=""{
	keep `if'
	}
	tokenize `varlist'
	drop if `1'==.|`2'==.|`3'==.|`4'==.
	if "`controls'" !=""{
	foreach var of varlist `controls'{
	drop if `var'==.
	}
	}
	if "`cluster'" !=""{
	drop if `cluster'==.
	}
	if "`recat_treatment'" !=""{
	drop if `recat_treatment'==.
	}
	if "`weight'" !=""{
	drop if `weight'==.
	}

// Creating the Y, G, T, D variables
	
	gen outcome_XX=`1'
	gen double group_XX=`2'
	egen time_XX=group(`3')
	gen treatment_XX=`4'


// Counting time periods and checking at least two time periods

sum time_XX, meanonly
if r(max)<2 {
di as error"There are less than two time periods in the data, the command cannot run."
scalar did_multiplegt_check=0
}
local max_time=r(max)

// Creating a discretized treatment even if recat_treatment option not specified

if "`recat_treatment'" !=""{
gen D_cat_XX=`recat_treatment'
}
else{
gen D_cat_XX=treatment_XX
}

// Creating groups of recategorized treatment, to ensure we have an ordered treatment with interval of 1 between consecutive values

egen d_cat_group_XX=group(D_cat_XX)

// Counting treatment values and checking at least two values

sum d_cat_group_XX, meanonly
if r(max)==r(min) {
di as error "Either the treatment variable or the recategorized treatment in the recat_treatment option takes only one value, the command cannot run."
scalar did_multiplegt_check=0
}

// Checking that the number in threshold_stable_treatment is positive

if `threshold_stable_treatment'<0{
di as error "The number in the threshold_stable_treatment option should be greater than or equal to 0."
scalar did_multiplegt_check=0
}


// Checking that number of placebos requested is admissible

if `placebo'>`max_time'-2{
di as error "The number of placebo estimates you have requested it too large: it should be at most equal to the number"
di as error "of time periods in your data minus 2."
scalar did_multiplegt_check=0
}

// Checking that number of dynamic effects requested is admissible

if `dynamic'>`max_time'-2{
di as error "The number of dynamic effects you have requested it too large: it should be at most equal to the number"
di as error "of time periods in your data minus 2."
scalar did_multiplegt_check=0
}

// Checking that number of bootstrap replications requested greater than 2

if `breps'==1{
di as error "The number of bootstrap replications should be equal to 0, or greater than 2."
scalar did_multiplegt_check=0
}

// Checking that if dynamic effects requested, robust_dynamic also requested
if "`robust_dynamic'"==""&`dynamic'!=0 {
di as error "If you request the computation of some dynamic effects," 
di as error "you need to request that your estimators be robust to dynamic effects."
scalar did_multiplegt_check=0
}


// Checking that if average_effect option requested, number of dynamic effects at least one

if "`average_effect'"!=""&`dynamic'==0 {
di as error "If you request the average_effect option," 
di as error "you need to request that at least one dynamic effect be computed."
scalar did_multiplegt_check=0
}

** RETRAIT MELITINE ******************************************************
// Checking that if longdiff_placebo option requested, robust_dynamic also requested - not necessary anymore since longdiff_placebo is now a default option 
//if "`longdiff_placebo'"!=""&"`robust_dynamic'"=="" {
//di as error "If you request the longdiff_placebo option, you also need to request the robust_dynamic option." 
//scalar did_multiplegt_check=0
//}
*************************************************************************

** AJOUT MELITINE *******************************************************

// Checking the user did not specify both firstdiff_placebo and longdiff_placebo

if "`placebo'"!="0"&"`firstdiff_placebo'"!=""&"`longdiff_placebo'"!="" {
di as error "You have specified both the firstdiff_placebo and the longdiff_placebo options. Only one of them should be specified."
scalar did_multiplegt_check=0
}

// Checking that if the firstdiff_placebo option is not requested and dynamic options requested, as longdiff_placebos are implemented, number of placebos lower than number of dynamic

if "`firstdiff_placebo'"==""&"`dynamic'"!="0"&`placebo'>`dynamic'{
di as error "By default, the command computes long-difference placebos. If the dynamic options are requested, the number" 
di as error "of placebos requested cannot be larger than the number of dynamic effects." 
scalar did_multiplegt_check=0
}

// Checking that if always_trends_lin or always_trends_nonparam is specified, the corresponding trends_lin or trends_nonparam are also specified.

if ("`always_trends_lin'"!=""&"`trends_lin'"=="")|("`always_trends_nonparam'"!=""&"`trends_nonparam'"==""){
di as error "You have specified the option always_trends_lin without specifying  the trends_lin option," 
di as error " or you have specified the option always_trends_nonparam without specifying the trends_nonparam option." 
scalar did_multiplegt_check=0
}

********************************************************************************

restore

end

/////////////////////////////////////////////////////////
///// Program #3: Runs and boostraps did_multiplegt_estim
/////////////////////////////////////////////////////////


capture program drop did_multiplegt_results
program did_multiplegt_results, eclass
	version 12.0
	syntax varlist(min=4 numeric) [if] [in]  [, RECAT_treatment(varlist numeric) THRESHOLD_stable_treatment(real 0) trends_nonparam(varlist numeric) trends_lin(varlist numeric) controls(varlist numeric) counter(varlist numeric) placebo(integer 0) dynamic(integer 0) breps(integer 50) cluster(varlist numeric) covariances switchers(string) LONGdiff_placebo FIRSTdiff_placebo count_switchers_tot count_switchers_contr robust_dynamic discount(real 1) seed(integer 0) drop_larger_lower always_trends_nonparam always_trends_lin]
	
// If computation of standard errors requested, bootstrap did_multiplegt_estim

if `breps'>0 {

tempvar group_bsample

// Initializing the too many controls scalar

scalar too_many_controls=0
 
forvalue i=1/`breps'{

preserve
if `seed'!=0{
set seed `=`seed'+`i''
}
bsample, cluster(`cluster')

//Indicate that program will run bootstrap replications

local bootstrap_rep=1

did_multiplegt_estim `varlist', threshold_stable_treatment(`threshold_stable_treatment') trends_nonparam(`trends_nonparam') trends_lin(`trends_lin') controls(`controls') counter(`counter') placebo(`placebo') dynamic(`dynamic') breps(`breps') cluster(`cluster') bootstrap_rep(`bootstrap_rep') switchers(`switchers') `longdiff_placebo' `firstdiff_placebo' `count_switchers_tot' `count_switchers_contr' `robust_dynamic' discount(`discount') `drop_larger_lower' `always_trends_nonparam' `always_trends_lin'


// Put results into a matrix 

matrix didmgt_bootstrap_`i'=effect_0_2
forvalue j=1/`dynamic'{
matrix didmgt_bootstrap_`i'=didmgt_bootstrap_`i',effect_`j'_2
}
forvalue j=1/`placebo'{
matrix didmgt_bootstrap_`i'=didmgt_bootstrap_`i',placebo_`j'_2
}

restore

// End of the loop on number of bootstrap replications
}

// Putting the matrix with all bootstrap reps together

matrix didmgt_bootstrap=didmgt_bootstrap_1
forvalue i=2/`breps'{
matrix didmgt_bootstrap=didmgt_bootstrap\ didmgt_bootstrap_`i'
}

// Error message if too many controls

if too_many_controls==1{
di as error "In some bootstrap replications, the command had to run regressions with strictly more" 
di as error "control variables than the sample size, so the controls could not all be accounted for." 
di as error "If you want to solve this problem, you may reduce the number of control"
di as error "variables. You may also use the recat_treatment option to discretize your treatment."
di as error "Finally, you could reduce the number of placebos and/or dynamic effects requested."
}

// End of if condition assessing if bootstrap reps requested 
}

// Run did_multiplegt_estim to get estimates and number of observations used 

preserve

//Indicate that program will run main estimation

local bootstrap_rep=0

// Initializing the too many controls scalar

scalar too_many_controls=0

did_multiplegt_estim `varlist', threshold_stable_treatment(`threshold_stable_treatment') trends_nonparam(`trends_nonparam') trends_lin(`trends_lin') controls(`controls') counter(`counter') placebo(`placebo') dynamic(`dynamic') breps(`breps') cluster(`cluster') bootstrap_rep(`bootstrap_rep') switchers(`switchers') `longdiff_placebo' `firstdiff_placebo' `count_switchers_tot' `count_switchers_contr' `robust_dynamic' discount(`discount') `drop_larger_lower' `always_trends_nonparam' `always_trends_lin'


// Error message if too many controls

if too_many_controls==1{
di as error "In the main estimation, the command had to run regressions with strictly more" 
di as error "control variables than the sample size, so the controls could not all be accounted for." 
di as error "If you want to solve this problem, you may reduce the number of control"
di as error "variables. You may also use the recat_treatment option to discretize your treatment."
di as error "Finally, you could reduce the number of placebos and/or dynamic effects requested."
}

restore

end

////////////////////////////////////////////////////////////////////////////////
///// Program #4: performs outcome change residualisation, and requests ////////
///// computation of all point estimates asked by user /////////////////////////
////////////////////////////////////////////////////////////////////////////////

capture program drop did_multiplegt_estim
program did_multiplegt_estim, eclass
	version 12.0
	syntax varlist(min=4 numeric) [if] [in] [, THRESHOLD_stable_treatment(real 0) trends_nonparam(varlist numeric) trends_lin(varlist numeric) controls(varlist numeric) counter(varlist numeric) placebo(integer 0) dynamic(integer 0) breps(integer 50) cluster(varlist numeric) bootstrap_rep(integer 0) switchers(string) LONGdiff_placebo FIRSTdiff_placebo count_switchers_tot count_switchers_contr robust_dynamic discount(real 1) drop_larger_lower always_trends_nonparam always_trends_lin]


tempvar tag_obs group_incl

if `bootstrap_rep'==0{
gen tag_switchers_contr_XX=0
}
	
if "`trends_lin'" !=""|"`controls'" !=""{

sum d_cat_group_XX, meanonly
local D_min=r(min)
local D_max=r(max)

forvalue d=`=`D_min''/`=`D_max'' {

global cond_increase "abs(diff_stag_d_XX)>`threshold_stable_treatment'&increase_d_XX==1&lag_d_cat_group_XX==`d'&diff_stag_d_XX!=."
global cond_stable "abs(diff_stag_d_XX)<=`threshold_stable_treatment'&ever_change_d_XX==0&lag_d_cat_group_XX==`d'"
global cond_decrease "abs(diff_stag_d_XX)>`threshold_stable_treatment'&increase_d_XX==0&lag_d_cat_group_XX==`d'&diff_stag_d_XX!=."

sum diff_y_XX if $cond_increase, meanonly
scalar n_increase=r(N)
sum diff_y_XX if $cond_stable, meanonly
scalar n_stable=r(N)
sum diff_y_XX if $cond_decrease, meanonly
scalar n_decrease=r(N)

// Assessing if the residualization needs to be done for that treatment value
if ("`switchers'"==""&n_stable>0&(n_increase>0|n_decrease>0))|("`switchers'"=="in"&n_stable>0&n_increase>0)|("`switchers'"=="out"&n_stable>0&n_decrease>0) {

sum diff_y_XX if $cond_stable, meanonly

// Assessing if too many controls
if r(N)>=1{
scalar too_many_controls_temp=(r(N)<wordcount("`controls'"))
scalar too_many_controls=max(too_many_controls,too_many_controls_temp)
}

///////////////// Regression of diff_y on controls and FEs of trends_lin, and storing coefficients

cap drop FE*

if "`trends_lin'"!=""{

capture $noisily reghdfe diff_y_XX `controls' [aweight=`counter'] if $cond_stable, absorb(FE1=`trends_lin' FE2=time_XX) resid keepsingletons

}

if "`trends_lin'"==""&"`trends_nonparam'"==""&"`controls'"!=""{

capture $noisily reghdfe diff_y_XX `controls' [aweight=`counter'] if $cond_stable, absorb(FE1=time_XX) resid keepsingletons

}

if "`trends_nonparam'"!=""&"`controls'"!=""{

capture $noisily reghdfe diff_y_XX `controls' [aweight=`counter'] if $cond_stable, absorb(FE1=trends_var_XX) resid keepsingletons

}

capture matrix didmgt_B = e(b)

// Patching if not enough observations in this regression
if _rc!=301{

if "`trends_lin'"!=""{

gen `tag_obs'=e(sample)
bys `trends_lin': egen `group_incl'=max(`tag_obs') 

fcollapse (mean) FE_1=FE1, by(`trends_lin') merge

sum FE_1 [aweight=`counter'] if lag_d_cat_group_XX==`d'&`group_incl'==1

** AJOUT MELITINE **************************************************************
if "`always_trends_lin'"==""{
	replace FE_1=r(mean) if lag_d_cat_group_XX==`d'&`group_incl'==0 
	// what does the r(mean) do exactly here ?
}

if "`always_trends_lin'"!=""{
	drop if lag_d_cat_group_XX==`d'&`group_incl'==0
}

********************************************************************************

if `bootstrap_rep'==0{
replace tag_switchers_contr_XX=1 if `group_incl'==1&($cond_increase | $cond_decrease) 
}
}

// Creating variables with controls coefficients
local j = 0
foreach var of local controls {
local j = `j' + 1
gen coeff`j' = didmgt_B[1,`j']
}

///////////////////// Residualizing outcome changes

// Current outcome FD, for instantaneous effect estimation
if "`trends_lin'"!=""{
replace diff_y_XX=diff_y_XX-FE_1 if lag_d_cat_group_XX==`d'
}

local j=0
foreach var of local controls{
local j=`j'+1
replace diff_y_XX=diff_y_XX-coeff`j'*ZZ_cont`j' if lag_d_cat_group_XX==`d'
}

// Lagged outcome FD, for FD placebo estimation

** MODIFICATION MELITINE ** "`firstdiff_placebo'"!="" remplace "`longdiff_placebo'"==""
if "`placebo'"!="0"&"`firstdiff_placebo'"!=""{

forvalue i=1/`=`placebo''{

if "`trends_lin'"!=""{
replace diff_y_lag`i'_XX=diff_y_lag`i'_XX-FE_1 if lag_d_cat_group_XX==`d'
}

local j=0
foreach var of local controls{
local j=`j'+1
replace diff_y_lag`i'_XX=diff_y_lag`i'_XX-coeff`j'*ZZ_cont_lag`i'_`j' if lag_d_cat_group_XX==`d'
}

}

}

// Lagged outcome long diff, for long diff placebo estimation

** MODIFICATION MELITINE ** "`firstdiff_placebo'"=="" remplace "`longdiff_placebo'"!=""
if "`placebo'"!="0"&"`firstdiff_placebo'"==""{

forvalue i=1/`=`placebo''{

if "`trends_lin'"!=""{
replace ldiff_y_`i'_lag_XX=ldiff_y_`i'_lag_XX-FE_1*`i' if lag_d_cat_group_XX==`d'
}

local j=0
foreach var of local controls{
local j=`j'+1
replace ldiff_y_`i'_lag_XX=ldiff_y_`i'_lag_XX-coeff`j'*ZZ_cont_ldiff_`i'_lag_`j' if lag_d_cat_group_XX==`d'
}

}

}

// Lead outcome long diff, for dynamic effect estimation
if "`dynamic'"!="0"{

forvalue i=1/`=`dynamic''{

if "`trends_lin'"!=""{
replace ldiff_y_for`i'_XX=ldiff_y_for`i'_XX-FE_1*(`i'+1) if lag_d_cat_group_XX==`d'
}

local j=0
foreach var of local controls{
local j=`j'+1
replace ldiff_y_for`i'_XX=ldiff_y_for`i'_XX-coeff`j'*ZZ_cont_ldiff_for`i'_`j' if lag_d_cat_group_XX==`d'
}

}

}

cap drop `group_incl' `tag_obs'
cap drop FE*
cap drop coeff*

/// End of patch if not enough observations in the regression with controls
}

/// End of condition assessing if residualization needed for that treatment value
}

// End of the loop over values of D_cat
}

// End of the if condition assessing if trends_lin or controls requested
}


// Counting time periods

sum time_XX, meanonly
local max_time=r(max)

// Estimating the instantaneous effect

*Running did_multiplegt_core
did_multiplegt_core, threshold_stable_treatment(`threshold_stable_treatment') trends_nonparam(`trends_nonparam') trends_lin(`trends_lin') controls(`controls') placebo(`placebo') d_cat_group(d_cat_group_XX) lag_d_cat_group(lag_d_cat_group_XX) diff_d(diff_d_XX) diff_y(diff_y_XX) counter(`counter') time(time_XX) group_int(group_XX) max_time(`max_time') counter_placebo(0) counter_dynamic(0) bootstrap_rep(`bootstrap_rep') switchers(`switchers') `robust_dynamic' discount(`discount') trends_lin(`trends_lin') `drop_larger_lower' `always_trends_nonparam' `always_trends_lin' `firstdiff_placebo' `longdiff_placebo'

*Collecting point estimate and number of observations

scalar effect_0_2=effect_XX
scalar N_effect_0_2=N_effect
scalar N_switchers_effect_0_2=N_switchers
if "`count_switchers_tot'"!=""{
scalar N_switchers_effect_0_tot_2=N_switchers_tot2
}
if "`count_switchers_contr'"!=""&("`trends_lin'"!=""|"`trends_nonparam'"!=""){
scalar N_switchers_effect_0_contr_2=N_switchers_contr2
} 
scalar denom_DID_ell_0=denom_DID_ell_XX
scalar denom_delta_0=denom_XX

// If first difference placebos requested, estimate them and number of observations used in that estimation

** MODIFICATION MELITINE ** "`firstdiff_placebo'"!="" remplace "`longdiff_placebo'"==""
if "`placebo'"!="0"&"`firstdiff_placebo'"!=""{

tempvar cond_placebo  
gen `cond_placebo'=1

*Looping over the number of placebos requested

forvalue i=1/`=`placebo''{

*Replacing FD of outcome by lagged FD of outcome, FD of controls by lagged FD of controls, and excluding from placebo observations whose lagged FD of treatment non 0. 

// Note: the line below is superfluous if the robust_dynamic option is specified, because then 
// only (g,t)s with diff_stag_d_XX>`threshold_stable_treatment', meaning those changing treatment for the first time at t satisfy "cond_increase_t" and "cond_decrease_t" anyways.
// But that line plays a role if the robust_dynamic option is not specified so it is important to keep it. 
replace `cond_placebo'=0 if abs(diff_d_lag`i'_XX)>`threshold_stable_treatment'

preserve

replace diff_y_XX=diff_y_lag`i'_XX

if "`controls'" !=""{
local j=0
foreach var of local controls{
local j=`j'+1
replace `var'=ZZ_cont_lag`i'_`j'
}
}

*If no observation satisfy `cond_placebo'==1, set N_placebo_`i'_2 to 0

sum diff_y_XX if `cond_placebo'==1

if r(N)==0{

scalar N_placebo_`i'_2=.
scalar placebo_`i'_2=.
scalar N_switchers_placebo_`i'_2=.
if "`count_switchers_tot'"!=""{
scalar N_switchers_placebo_`i'_tot_2=.
}
if "`count_switchers_contr'"!=""&("`trends_lin'"!=""|"`trends_nonparam'"!=""){
scalar N_switchers_placebo_`i'_contr_2=.
} 
}

*Otherwise, run did_multiplegt_core

else{

did_multiplegt_core if `cond_placebo'==1, threshold_stable_treatment(`threshold_stable_treatment') trends_nonparam(`trends_nonparam') trends_lin(`trends_lin') controls(`controls') placebo(`placebo') d_cat_group(d_cat_group_XX) lag_d_cat_group(lag_d_cat_group_XX) diff_d(diff_d_XX) diff_y(diff_y_XX) counter(`counter') time(time_XX) group_int(group_XX) max_time(`max_time') counter_placebo(`i') counter_dynamic(0) bootstrap_rep(`bootstrap_rep') switchers(`switchers') `robust_dynamic' discount(`discount') trends_lin(`trends_lin') `drop_larger_lower' `always_trends_nonparam' `always_trends_lin' `firstdiff_placebo' `longdiff_placebo'

*Collecting point estimate and number of observations

scalar placebo_`i'_2=effect_XX
scalar N_placebo_`i'_2=N_effect
scalar N_switchers_placebo_`i'_2=N_switchers
if "`count_switchers_tot'"!=""{
scalar N_switchers_placebo_`i'_tot_2=N_switchers_tot2
}
if "`count_switchers_contr'"!=""&("`trends_lin'"!=""|"`trends_nonparam'"!=""){
scalar N_switchers_placebo_`i'_contr_2=N_switchers_contr2
} 

}

restore

*End of the loop on the number of placebos
}

*End of the condition assessing if the computation of placebos was requested by the user
}


// If long-difference placebos requested, estimate them and number of observations used in that estimation

** MODIFICATION MELITINE ** "`firstdiff_placebo'"=="" remplace "`longdiff_placebo'"!=""
if "`placebo'"!="0"&"`firstdiff_placebo'"==""{

tempvar cond_placebo
gen `cond_placebo'=1

*Looping over the number of placebos requested

forvalue i=1/`=`placebo''{

*Replacing FD of outcome by long diff of outcome, and creating variable to exclude from placebo observations whose lead FD of treatment non 0. 

if `i'>1{
replace `cond_placebo'=0 if abs(diff_stag_d_for`=`i'-1'_XX)>`threshold_stable_treatment'
}

preserve

replace diff_y_XX=ldiff_y_`i'_lag_XX
if `i'>1{
replace `counter'=counter_F`=`i'-1'_XX
}
if "`controls'" !=""{
local j=0
foreach var of local controls{
local j=`j'+1
replace `var'=ZZ_cont_ldiff_`i'_lag_`j'
}
}

*If no observation satisfy `cond_placebo'==1, set N_placebo_`i'_2 to 0

sum diff_y_XX if `cond_placebo'==1

if r(N)==0{
scalar N_placebo_`i'_2=.
scalar placebo_`i'_2=.
scalar N_switchers_placebo_`i'_2=.
if "`count_switchers_tot'"!=""{
scalar N_switchers_placebo_`i'_tot_2=.
}
if "`count_switchers_contr'"!=""&("`trends_lin'"!=""|"`trends_nonparam'"!=""){
scalar N_switchers_placebo_`i'_contr_2=.
} 
}

*Otherwise, run did_multiplegt_core

else{

did_multiplegt_core if `cond_placebo'==1, threshold_stable_treatment(`threshold_stable_treatment') trends_nonparam(`trends_nonparam') trends_lin(`trends_lin') controls(`controls') placebo(`placebo') d_cat_group(d_cat_group_XX) lag_d_cat_group(lag_d_cat_group_XX) diff_d(diff_d_XX) diff_y(diff_y_XX) counter(`counter') time(time_XX) group_int(group_XX) max_time(`max_time') counter_placebo(`i') counter_dynamic(0) bootstrap_rep(`bootstrap_rep') switchers(`switchers') `robust_dynamic' discount(`discount') trends_lin(`trends_lin') `drop_larger_lower' `always_trends_nonparam' `always_trends_lin' `firstdiff_placebo' `longdiff_placebo'

*Collecting point estimate and number of observations

scalar placebo_`i'_2=-effect_XX
scalar N_placebo_`i'_2=N_effect
scalar N_switchers_placebo_`i'_2=N_switchers
if "`count_switchers_tot'"!=""{
scalar N_switchers_placebo_`i'_tot_2=N_switchers_tot2
}
if "`count_switchers_contr'"!=""&("`trends_lin'"!=""|"`trends_nonparam'"!=""){
scalar N_switchers_placebo_`i'_contr_2=N_switchers_contr2
} 
}

restore

*End of the loop on the number of placebos
}

*End of the condition assessing if the computation of long-diff placebos was requested by the user
}


// If dynamic effects requested, estimate them and number of observations used in that estimation

if "`dynamic'"!="0"{

tempvar cond_dynamic
gen `cond_dynamic'=1

*Looping over the number of placebos requested

forvalue i=1/`=`dynamic''{

*Replacing FD of outcome by long diff of outcome, and creating variable to exclude from placebo observations whose lead FD of treatment non 0. 

replace `cond_dynamic'=0 if abs(diff_stag_d_for`i'_XX)>`threshold_stable_treatment'

preserve

replace diff_y_XX=ldiff_y_for`i'_XX
replace `counter'=counter_F`i'_XX

replace diff_d_XX=ldiff_d_for`i'_XX

if "`controls'" !=""{
local j=0
foreach var of local controls{
local j=`j'+1
replace `var'=ZZ_cont_ldiff_for`i'_`j'
}
}

*If no observation satisfy `cond_dynamic'==1, set N_effect_`i'_2 to 0

sum diff_y_XX if `cond_dynamic'==1

if r(N)==0{

scalar N_effect_`i'_2=.
scalar N_switchers_effect_`i'_2=.
scalar effect_`i'_2=.
if "`count_switchers_tot'"!=""{
scalar N_switchers_effect_`i'_tot_2=.
}
if "`count_switchers_contr'"!=""&("`trends_lin'"!=""|"`trends_nonparam'"!=""){
scalar N_switchers_effect_`i'_contr_2=.
} 
}

*Otherwise, run did_multiplegt_core

else{


did_multiplegt_core if `cond_dynamic'==1, threshold_stable_treatment(`threshold_stable_treatment') trends_nonparam(`trends_nonparam') trends_lin(`trends_lin') controls(`controls') placebo(`placebo') d_cat_group(d_cat_group_XX) lag_d_cat_group(lag_d_cat_group_XX) diff_d(diff_d_XX) diff_y(diff_y_XX) counter(`counter') time(time_XX) group_int(group_XX) max_time(`max_time') counter_placebo(0) counter_dynamic(`i') bootstrap_rep(`bootstrap_rep') switchers(`switchers') `robust_dynamic' discount(`discount') trends_lin(`trends_lin') `drop_larger_lower' `always_trends_nonparam' `always_trends_lin' `firstdiff_placebo' `longdiff_placebo'

*Collecting point estimate and number of observations

scalar effect_`i'_2=effect_XX
scalar N_effect_`i'_2=N_effect
scalar N_switchers_effect_`i'_2=N_switchers
if "`count_switchers_tot'"!=""{
scalar N_switchers_effect_`i'_tot_2=N_switchers_tot2
}
if "`count_switchers_contr'"!=""&("`trends_lin'"!=""|"`trends_nonparam'"!=""){
scalar N_switchers_effect_`i'_contr_2=N_switchers_contr2
} 
scalar denom_DID_ell_`i'=denom_DID_ell_XX
scalar denom_delta_`i'=denom_XX
}

restore

drop diff_stag_d_for`i'_XX 

*End of the loop on the number of dynamic effects
}

*End of the condition assessing if the computation of dynamic effects was requested by the user
}

end

////////////////////////////////////////////////////////////////////////////////
///// Program #5: performs computation of all individual point estimates ///////
////////////////////////////////////////////////////////////////////////////////

capture program drop did_multiplegt_core
program did_multiplegt_core
	version 12.0
	syntax [if] [in] [, THRESHOLD_stable_treatment(real 0) trends_nonparam(varlist numeric) trends_lin(varlist numeric) controls(varlist numeric) placebo(integer 0) d_cat_group(varlist numeric) lag_d_cat_group(varlist numeric) diff_d(varlist numeric) diff_y(varlist numeric) counter(varlist numeric) time(varlist numeric) group_int(varlist numeric) max_time(integer 0) counter_placebo(integer 0) counter_dynamic(integer 0) bootstrap_rep(integer 0) switchers(string) robust_dynamic discount(real 1) trends_lin(varlist numeric) drop_larger_lower always_trends_nonparam always_trends_lin FIRSTdiff_placebo LONGdiff_placebo]

tempvar diff_y_res1 diff_y_res2 group_incl treatment_dummy tag_obs tag_switchers counter_tot counter_switchers tag_switchers_tot counter_switchers_tot tag_switchers_contr counter_switchers_contr

preserve

// Selecting the sample

	if "`if'" !=""{
	keep `if'
	}
	
// Drop if diff_y_XX missing, to avoid that those observations are used in estimation

drop if diff_y_XX==.
	
// Creating residualized first diff outcome if trends_nonparam specified in estimation

if "`trends_nonparam'" !=""{

sum d_cat_group_XX, meanonly
local D_min=r(min)
local D_max=r(max)

forvalue d=`=`D_min''/`=`D_max'' {

global cond_increase "abs(diff_stag_d_XX)>`threshold_stable_treatment'&increase_d_XX==1&lag_d_cat_group_XX==`d'&diff_stag_d_XX!=."

global cond_stable "abs(diff_stag_d_XX)<=`threshold_stable_treatment'&ever_change_d_XX==0&lag_d_cat_group_XX==`d'"
global cond_decrease "abs(diff_stag_d_XX)>`threshold_stable_treatment'&increase_d_XX==0&lag_d_cat_group_XX==`d'&diff_stag_d_XX!=."

sum diff_y_XX if $cond_stable, meanonly

/////////////// Regression of diff_y on time FEs interacted with trends_nonparam variable, and computation of residuals

cap drop FE*
capture $noisily reghdfe diff_y_XX  [aweight=`counter'] if $cond_stable, absorb(FE1=trends_var_XX) resid keepsingletons

if _rc!=2001{ /// Modif Felix
// Patching if not enough observations in this regression
capture matrix didmgt_B = e(b)
if _rc!=301{ // r(301) in an error for "last estimate not found"

gen `tag_obs'=e(sample)
bys trends_var_XX: egen `group_incl'=max(`tag_obs')

fcollapse (mean) FE=FE1 , by(trends_var_XX) merge

gen `diff_y_res1'  = diff_y_XX - FE
gen constant = didmgt_B[1,1]
replace `diff_y_res1' = `diff_y_res1' - constant

cap drop FE constant

** AJOUT MELITINE **************************************************************

if "`always_trends_nonparam'"==""{
$noisily reg diff_y_XX i.time_XX [aweight=`counter'] if $cond_stable, $no_header_no_table
predict `diff_y_res2', r
replace diff_y_XX=`diff_y_res2' if lag_d_cat_group_XX==`d'&`group_incl'==0
}

replace diff_y_XX=`diff_y_res1' if lag_d_cat_group_XX==`d'&`group_incl'==1


if "`always_trends_nonparam'"!=""{
drop if lag_d_cat_group_XX==`d'&`group_incl'==0
}

********************************************************************************

if `bootstrap_rep'==0{
replace tag_switchers_contr_XX=1 if `group_incl'==1&($cond_increase | $cond_decrease) 
}

** RETRAIT MELITINE - plus de `diff_y_res2' dans le drop, exécuté après
drop `diff_y_res1' `group_incl' `tag_obs'


** AJOUT MELITINE **************************************************************
if "`always_trends_nonparam'"==""{
drop `diff_y_res2'
}
********************************************************************************

}

} // Modif Felix

// End of the loop over values of D_cat
}

// End of the if condition assessing if trends_nonparam included in estimation
}
 
// Treatment effect

// Initializing estimate, weight, and variable to count observations used in estimation
scalar effect_XX=0
scalar N_effect =0
scalar N_switchers=0
scalar N_switchers_tot2=0
scalar N_switchers_contr2=0
scalar denom_XX=0
scalar denom_DID_ell_XX=0
gen `tag_obs'=0
gen `tag_switchers'=0
gen `tag_switchers_tot'=0

$noisily di "Computing DIDM"

// Looping over time periods
forvalue t=`=`counter_placebo'+2'/`=`max_time'-`counter_dynamic''{

// Determining the min and max value of group of treatment at t-1

sum lag_d_cat_group_XX if time_XX==`t', meanonly

local D_min=r(min)
local D_max=r(max)



// Ensuring that there are observations with non missing lagged treatment

if `D_min'!=.&`D_max'!=.{

// Looping over possible values of lag_D at time t
forvalue d=`=`D_min''/`=`D_max'' {

// Defining conditions for groups where treatment increased/remained stable/decreased between t-1 and t

// Note: If robust_dynamic option specified, 
// cond_increase_t=those:  
// 1) whose treatment changes for first time at t (t=t-\ell in paper): abs(diff_stag_d_XX)>`threshold_stable_treatment'
// 2) whose treatment cost is higher than if they had kept status quo treatment: increase_d_XX==1
// Same thing for cond_decrease_t
// cond_stable_t= those: 
// 1) whose treatment does not change at t, 
// 2) whose treatment has not changed before t (was implied by 1 in staggered designs, not the case anymore, hence the addition of ever_change_d_XX==0)
// those two conditions are sufficient to select the right control groups when we estimate the instantaneous treatment effect, or first difference placebos,
// but if we are running this to estimate a dynamic effect at time t+\ell we need a third condition: 
// 3) the ``if `cond_dynamic'==1'' condition when we call the command in program #4 above ensures we do not have obs whose treatment changed for the first time somehwere between t+1 and t+\ell. 
// If robust_dynamic option not specified, ever_change_d_XX==0, diff_stag_d_XX=diff_d_XX, and increase_d_XX=(diff_d_XX>0) so the new conditions below are equal to the old ones commented above.
 
global cond_increase_t "abs(diff_stag_d_XX)>`threshold_stable_treatment'&increase_d_XX==1&lag_d_cat_group_XX==`d'&time_XX==`t'&diff_stag_d_XX!=."
global cond_stable_t "abs(diff_stag_d_XX)<=`threshold_stable_treatment'&ever_change_d_XX==0&lag_d_cat_group_XX==`d'&time_XX==`t'"
global cond_decrease_t "abs(diff_stag_d_XX)>`threshold_stable_treatment'&increase_d_XX==0&lag_d_cat_group_XX==`d'&time_XX==`t'&diff_stag_d_XX!=."

// Counting number of units in each supergroup
sum d_cat_group_XX if $cond_increase_t, meanonly
scalar n_increase=r(N)
sum d_cat_group_XX if $cond_stable_t, meanonly
scalar n_stable=r(N)
sum d_cat_group_XX if $cond_decrease_t, meanonly
scalar n_decrease=r(N)

// If there are units whose treatment increased and units whose treatment remained stable, estimate corresponding DID, 
// increment point estimate and weight, and tag observations used in estimation

if "`switchers'" !="out"{
if `bootstrap_rep'==0{
replace `tag_switchers_tot'=1 if $cond_increase_t
}
if n_increase*n_stable>0 {
gen `treatment_dummy' =($cond_increase_t)

if `bootstrap_rep'==0{
replace `tag_obs'=1 if (($cond_increase_t)|($cond_stable_t))
replace `tag_switchers'=1 if $cond_increase_t
}

$noisily reg diff_y_XX `treatment_dummy' [aweight=`counter'] if ($cond_increase_t)|($cond_stable_t), $no_header_no_table
sum `counter' if $cond_increase_t, meanonly
scalar effect_XX=effect_XX+_b[`treatment_dummy']*r(N)*r(mean)*(`discount'^`t')
$noisily reg diff_d_XX `treatment_dummy' [aweight=`counter'] if ($cond_increase_t)|($cond_stable_t), $no_header_no_table
sum `counter' if $cond_increase_t, meanonly
scalar denom_XX=denom_XX+_b[`treatment_dummy']*r(N)*r(mean)*(`discount'^`t')
scalar denom_DID_ell_XX=denom_DID_ell_XX+r(N)*r(mean)*(`discount'^`t')
drop `treatment_dummy' 
}
}

// If there are units whose treatment decreased and units whose treatment remained stable, estimate corresponding DID, 
// increment point estimate and weight, and tag observations used in estimation

if "`switchers'"!="in"{
if `bootstrap_rep'==0{
replace `tag_switchers_tot'=1 if $cond_decrease_t
}
if n_decrease*n_stable>0 {
gen `treatment_dummy' =($cond_decrease_t)

if `bootstrap_rep'==0{
replace `tag_obs'=1 if (($cond_decrease_t)|($cond_stable_t))
replace `tag_switchers'=1 if $cond_decrease_t
}

$noisily reg diff_y_XX `treatment_dummy' [aweight=`counter'] if ($cond_decrease_t)|($cond_stable_t), $no_header_no_table
sum `counter' if $cond_decrease_t, meanonly
scalar effect_XX=effect_XX-_b[`treatment_dummy']*r(N)*r(mean)*(`discount'^`t')
$noisily reg diff_d_XX `treatment_dummy' [aweight=`counter'] if ($cond_decrease_t)|($cond_stable_t), $no_header_no_table
sum `counter' if $cond_decrease_t, meanonly
scalar denom_XX=denom_XX-_b[`treatment_dummy']*r(N)*r(mean)*(`discount'^`t')
scalar denom_DID_ell_XX=denom_DID_ell_XX+r(N)*r(mean)*(`discount'^`t')
drop `treatment_dummy' 
}
}

// End of loop on recat treatment values at t-1 
}

// End of condition ensuring that there are observations with non missing lagged treatment
}

// End of loop on time
}

if "`robust_dynamic'"==""{
scalar effect_XX=effect_XX/denom_XX
}

if "`robust_dynamic'"!=""{
scalar effect_XX=effect_XX/denom_DID_ell_XX
}

if `bootstrap_rep'==0{
egen `counter_tot'=total(`counter') if `tag_obs'==1
sum `counter_tot', meanonly
scalar N_effect=r(mean)
egen `counter_switchers'=total(`counter') if `tag_switchers'==1
sum `counter_switchers', meanonly
scalar N_switchers=r(mean)
egen `counter_switchers_tot'=total(`counter') if `tag_switchers_tot'==1
sum `counter_switchers_tot', meanonly
scalar N_switchers_tot2=r(mean)
egen `counter_switchers_contr'=total(`counter') if tag_switchers_contr_XX==1&`tag_switchers'==1
sum `counter_switchers_contr', meanonly
scalar N_switchers_contr2=r(mean)
}

restore

end
