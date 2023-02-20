*﻿* did_multiplegt with asymptotic variances
** This version: February 15th 2023
** This version is to take into account one covariate.
***Doulo
********************************************************************************
*                                 PROGRAM 1                                    *
********************************************************************************

capture program drop did_multiplegt_var_v8

program did_multiplegt_var_v8, eclass
	version 12.0
	syntax varlist(min=4 max=4 numeric) [if] [in] [, dynamic(integer 0) switchers(string) controls(varlist numeric) drop_larger_lower]

	qui{
capture drop outcome_XX
capture drop group_XX
capture drop time_XX
capture drop treatment_XX
capture drop d_sq_XX
capture drop diff_from_sq_XX
capture drop ever_change_d_XX
capture drop never_change_d_XX
capture drop temp_F_g_XX
capture drop F_g_XX
capture drop t_min_XX
capture drop T_max_XX
capture drop N_gt_XX
capture drop T_g_XX
capture drop min_F_g_XX
capture drop controls_time_XX
capture drop _fillin
capture drop treat_not_missing_XX
capture drop time_treat_not_miss_XX
capture drop time_d_nonmiss_XX
capture drop min_time_d_nonmiss_XX
capture drop max_treat_not_miss_XX
capture drop d_missing_startinghere_XX
capture drop d_missing_untilhere_XX
capture drop min_treat_not_miss_XX
capture drop first_switch_unknown_date_t_XX
capture drop first_switch_unknown_date_XX
capture drop d_F_g_temp_XX
capture drop d_F_g_XX
capture drop R_g_XX
capture drop L_g_XX
capture drop U_Gg_plus_XX
capture drop U_Gg_minus_XX
capture drop U_Gg_global_XX
capture drop w_plus_XX
capture drop first_obs_by_gp_XX
capture drop avg_post_switch_treat_XX
capture drop diff_from_sq_XX
capture drop ever_strict_increase_XX
capture drop ever_strict_decrease_XX

tokenize `varlist'
drop if `2'==.|`3'==.


////// Note on imbalanced panels, and comparison with previous command /////
//// 1. When a (g,t) is missing, 3 possibilities: 
//// 1.a) D(g,t) missing Y(g,t) not missing
//// 1.b) D(g,t) not missing Y(g,t) missing
//// 1.c) D(g,t) missing Y(g,t) missing.
//// 2. At the group level, two possibilities: 
//// 2.a) all groups g observed continuously from T_min to T_max, with either 1<T_min or T_max<T (or both): the group enters the panel late or leaves early or both, but when observed observed continuously.
//// 2.b) for at least one group g, there is a t_1, t_2, t_3 t_1<t_2<t_3 such that g unobserved at t_1 and t_3 and observed at t_2.
//// There are differences in conventions when dealing with missing D(g,t) between old and new version of the command: 
//// Old command intentionally dropped all (g,t)s with missing D(g,t) or Y(g,t). 
//// Old command also unintentionally dropped some (g,t) such that D(g,t') missing for some t'<t. See e.g. line 1580.
//// New command tries to keep some (g,t)s with missing D(g,t) and non-missing Y(g,t). 
//// Also tries to almost always keep (g,t) such that D(g,t') missing for some t'>t. See conventions used below. 
//// In view of these conventions, old and new command should not give different results when panel balanced (D(g,t) and Y(g,t) never missing) or when we only have (g,t)-missingness of type 1.b).
//// Otherwise, can yield different results.

******  Creating the necessary variables. ************************************

////// Status quo treatment //
gen time_d_nonmiss_XX=`3' if `4'!=.
bys `2': egen min_time_d_nonmiss_XX=min(time_d_nonmiss_XX)
gen d_sq_XX_temp=`4' if `3'==min_time_d_nonmiss_XX
bys `2': egen d_sq_XX=mean(d_sq_XX_temp)
drop d_sq_XX_temp min_time_d_nonmiss_XX time_d_nonmiss_XX


////// If the option drop_larger_lower was specified, drop groups which experienced both an increase and a decrease in comparison with their status quo treatment. //

	gen diff_from_sq_XX=(`4'-d_sq_XX)

	
	if "`drop_larger_lower'"!=""{		
	sort `2' `3'
	gen ever_strict_increase_XX=(diff_from_sq_XX>0&`4'!=.) 
	replace ever_strict_increase_XX=1 if ever_strict_increase_XX[_n-1]==1&`2'==`2'[_n-1]

	gen ever_strict_decrease_XX=(diff_from_sq_XX<0&`4'!=.) 
	replace ever_strict_decrease_XX=1 if ever_strict_decrease_XX[_n-1]==1&`2'==`2'[_n-1]

	drop if ever_strict_increase_XX==1 & ever_strict_decrease_XX==1
	
	drop ever_strict_increase_XX ever_strict_decrease_XX
	
	}


sort `2' `3'
	
gen outcome_XX=`1'
egen group_XX=group(`2')
egen time_XX=group(`3')
gen treatment_XX=`4'

////// Counting number of groups //
sum group_XX
scalar G_XX=r(max)

////// Balancing the panel //
fillin group_XX time_XX
bys group_XX: egen d_sq_XX_new=mean(d_sq_XX)
drop d_sq_XX
rename d_sq_XX_new d_sq_XX

////// Ever changed treatment //
//gen diff_from_sq_XX=(treatment_XX-d_sq_XX)
gen ever_change_d_XX=(abs(diff_from_sq_XX)>0&treatment_XX!=.)
replace ever_change_d_XX=1 if ever_change_d_XX[_n-1]==1&group_XX==group_XX[_n-1]

////// For each value of the status-quo treatment, we drop time periods such that we do not have any control with that status quo treatment after that period //
//// Watch out: this means the panel is no longer balanced, though it is balanced within values of the status-quo treatment //
gen never_change_d_XX=1-ever_change_d_XX
bys time_XX d_sq_XX: egen controls_time_XX=max(never_change_d_XX) // indicates whether there is at least one control for each time period and each value of status quo treatment
drop if controls_time_XX==0

////// Date of the first treatment change //
sort group_XX time_XX
gen temp_F_g_XX=time_XX if ever_change_d_XX==1&ever_change_d_XX[_n-1]==0 
replace temp_F_g_XX=0 if temp_F_g_XX==.
bys group_XX: egen F_g_XX=max(temp_F_g_XX)
drop temp_F_g_XX


////// Dealing with missing treatments. ///

///// In case 1) and 2) below, the missing D(g,t) cannot be recovered, even in a staggered design, so we replace the Y(g,t) by missings to ensure those (g,t)s cannot be used in the estimation.
///// In case 3) and 4) below, the missing D(g,t) can be recovered in a staggered design, so we replace the missing D(g,t) by their values and do not change the Y(g,t)s: those (g,t)s are used in the estimation.

////// 1) If D(g,t) is missing, D(g,t') is missing for all t'>t, and treatment of group g has not changed yet at t, we cannot recover that group's missing treatment, even in a staggered design, so outcome of that group starting from the first missing treatment replaced by missing. //
////// Relatedly, if (g,t) such that D(g,t') missing for all t'<=t, then we consider that group has not joined the panel yet, and we replace its outcome by missings at those dates. This is the only exception to the rule of replacing D(g,t) by the value it would have in a staggered design: with  . . 0 0 in a staggered we could replace the . . by 0 0. But in a non staggered, could be wrong, and could lead us to misidentify date of first switch which would affect reduced-form estimates, unlike other conventions.
gen treat_not_missing_XX = (treatment_XX!=.) if treatment_XX!=.
gen time_treat_not_miss_XX = time_XX*treat_not_missing_XX
bys group_XX: egen max_treat_not_miss_XX = max(time_treat_not_miss_XX)
gen d_missing_startinghere_XX=(time_XX>max_treat_not_miss_XX)
bys group_XX: egen min_treat_not_miss_XX = min(time_treat_not_miss_XX)
gen d_missing_untilhere_XX=(time_XX<min_treat_not_miss_XX) //(time_XX<min_treat_not_miss_XX|min_treat_not_miss_XX==0)
replace outcome_XX=. if (d_missing_startinghere_XX==1&(time_XX<=F_g_XX|F_g_XX==0))|d_missing_untilhere_XX==1

////// 2) If D(g,F_g-1) is missing, given the way we constructed F_g above perhaps F_g is incorrect: group changed treatment for the first time at F_g-1. In any case, we cannot recover D(g,F_g-1), even in a staggered design.
///// Therefore, outcome of that group starting at the first treatment change replaced by missing. //

gen first_switch_unknown_date_t_XX=1  if time_XX==F_g_XX&treatment_XX[_n-1]==.
replace first_switch_unknown_date_t_XX=0 if first_switch_unknown_date_t_XX==.
bys group_XX: egen first_switch_unknown_date_XX=max(first_switch_unknown_date_t_XX)
replace outcome_XX=. if first_switch_unknown_date_XX==1&time_XX>=F_g_XX 

/////////// NB: when there are missing treatments around the date of first treatment change, and consequently the initial date of treatment change is unknown, F_g_XX is set equal to the first non-missing changed treatment. (For example, if the treatment path over 10 time periods is (0,0,.,.,.,.,1,1,1,1), F_g_XX is set equal to 7.) However, this is no problem as the outcomes are still missing for all time periods after the first missing treatments.

///// 3) Otherwise, if a (g,t) is such that D(g,t) is missing, t<=F_g_XX, but there is a t'>t such that D(g,t')=d_sq_XX, we consider that D(g,t)=d_sq_XX (by construction true in a staggered design). This means this (g,t) is used as a control by the command: it is possible that g's treatment has never changed yet at t. 

replace treatment_XX=d_sq_XX if treatment_XX==.&(time_XX<F_g_XX|(F_g_XX==0&d_missing_startinghere_XX==0))

///// 4) Finally, if a (g,t) is such that D(g,t) is missing and t>F_g, we consider that D(g,t)=D(g,F_g). Convention innocuous for the reduced-form parameters DID_l, not innocuous for the cost benefit ratios delta: implicitly assumes that treatment is the same as in F_g_XX at those time periods (by construction, true in a staggered design).

gen d_F_g_temp_XX = treatment_XX if time_XX==F_g_XX
bys group_XX: egen d_F_g_XX = mean(d_F_g_temp_XX)
replace treatment_XX=d_F_g_XX if treatment_XX==.&time_XX>F_g_XX&F_g_XX>0


////// Defining N_gt, the weight of each cell (g,t) //
gen N_gt_XX=1
replace N_gt_XX=0 if outcome_XX==.|treatment_XX==.


////// Computing t_min_XX, T_max_XX, and replacing F_g_XX by last period plus one for those that never change treatment //

sum time_XX
scalar t_min_XX=r(min)
scalar T_max_XX=r(max)
replace F_g_XX=T_max_XX+1 if F_g_XX==0 // defining F_g as T+1 for never-treated units

////// Determining T_g, last period where there is still a group with the same treatment as g's in period 1 and whose treatment has not changed since the start of the panel. //
bys d_sq_XX : egen T_g_XX = max(F_g_XX) 
replace T_g_XX = T_g_XX-1

////// Defining R_g, an indicator variable for groups whose average post switch treatment value is larger than their initial value of treatment. They will be considered switchers in. If R_g==0, that means the group is a switcher out. For never-switchers, R_g will be undefined. //
bys group_XX : egen avg_post_switch_treat_XX_temp = total(treatment_XX) if time_XX>=F_g_XX&time_XX<=T_g_XX
replace avg_post_switch_treat_XX_temp =avg_post_switch_treat_XX_temp/(T_g_XX-F_g_XX+1)
bys group_XX: egen avg_post_switch_treat_XX=mean(avg_post_switch_treat_XX_temp)
drop avg_post_switch_treat_XX_temp

drop if avg_post_switch_treat_XX==d_sq_XX&F_g_XX!=T_g_XX+1
// When a group is a switching group, but its average post-treatment treatment value is exactly equal to its status quo treatment, we cannnot classify it as a swicher in or a switcher out, but it is not a control either. As such, we drop it from the estimation.

gen R_g_XX=(avg_post_switch_treat_XX>d_sq_XX) if F_g_XX!=T_g_XX+1
// Here, R_g_XX==. for never-switchers groups only. Indeed, thanks
// to line 108 (drop if controls_time_XX==0) (g,t) such that t>T_g dropped.


///// Creating the variable L_g_XX = T_g_XX - F_g_XX, so that we can compute L_u or L_a afterwards. //
gen L_g_XX = T_g_XX - F_g_XX

///// Trick with the first observation of each group_XX //
bysort group_XX : gen first_obs_by_gp_XX = (_n==1)


////// Declaring data as panel //
xtset group_XX time_XX

///// Writing the variables as first differences //
capture drop diff_y_XX
capture drop diff_d_XX
gen diff_y_XX = d.outcome_XX
g diff_d_XX = d.treatment_XX

if "`controls'" !=""{
local count_controls=0
// Here there is only one covariate, so we are anticipating a bit the case with several covariates.
foreach var of varlist `controls'{
local count_controls=`count_controls'+1
capture drop diff_X`count_controls'_XX
gen diff_X`count_controls'_XX=d.`var'


// Computing average value over groups (or time ???) for each covariate, for each status quo
bys group_XX d_sq_XX : egen avg_diff_X`count_controls'_XX = mean(diff_X`count_controls'_XX)

// Computing the difference between the first differences of covariates and the average of their first-difference 
bys group_XX d_sq_XX : egen resid_X`count_controls'_time_FE_XX = diff_X`count_controls'_XX - avg_diff_X`count_controls'_XX
}
}

///// Next steps to code:
* Make a loop over all status quos
* Gather all the resid_X`count_controls'_time_FE_XX into a matrix
* Apply the matrix accum function
* Isolate XX and Xy
* matrix theta_d_sq_XX = invsym(XX)*Xy




////// Premilinaries to the estimation //

///// Computing T_u/T_a, and L_u/L_a, as it is necessary to compare them to estimate the number of dynamic effects to compute //

// For switchers in
if "`switchers'"==""|"`switchers'"=="in"{ 
sum L_g_XX if R_g_XX==1
scalar L_u_XX=r(max)
}

// For switchers out
if "`switchers'"==""|"`switchers'"=="out"{
sum L_g_XX if R_g_XX==0
scalar L_a_XX=r(max)
}


// Computing the final number of dynamic effects to be estimated. If the number asked by the user was too large, display an error message.
if "`switchers'"==""{
scalar l_XX=max(L_a_XX, L_u_XX)
scalar l_XX=min(l_XX, `dynamic')
}

if "`switchers'"=="in"{
	scalar l_XX=min(`dynamic', L_u_XX)
}

if "`switchers'"=="out"{
	scalar l_XX=min(`dynamic', L_a_XX)
}


if l_XX<`dynamic'{
	di as error "The number of dynamic effects requested is too large."
	di as error "The number of dynamic effects which can be estimated is at most " l_XX "."
	di as error "The command will therefore try to estimate " l_XX " dynamic effect(s)."
}

///// Generating default values for the variables which will be aggregated. If there is a subsequent estimation, variables will be set equal to their estimated values instead. If there is no subsequent estimation, they will remain equal to zero but will not be missing. //
forvalue i=0/`=l_XX'{	
	
	capture drop U_Gg`i'_plus_XX 
	capture drop U_Gg`i'_minus_XX
	capture drop count`i'_plus_XX
	capture drop count`i'_minus_XX
	capture drop count`i'_global_XX
	capture drop U_Gg`i'_global_XX
	capture drop count`i'_global_XX
	capture drop N_effect_`i'_XX
	capture drop DID_`i'_XX
	
	gen U_Gg`i'_plus_XX = 0
	gen U_Gg`i'_minus_XX = 0
	gen count`i'_plus_XX = 0
	gen count`i'_minus_XX = 0
	
	scalar N1_`i'_XX=0
	scalar N0_`i'_XX=0

}

gen U_Gg_plus_XX = 0
gen U_Gg_minus_XX = 0
scalar U_Gg_den_plus_XX = 0
scalar U_Gg_den_minus_XX = 0
scalar sum_N1_l_XX = 0
scalar sum_N0_l_XX = 0



////// Perform here the estimation, i.e. call the program did_multiplegt_var_core_v8. //

// For switchers in
if ("`switchers'"==""|"`switchers'"=="in"){
	did_multiplegt_var_core_v8 outcome_XX group_XX time_XX treatment_XX, dynamic(`dynamic') switchers_core(in)
	
// Store the results
forvalue i=0/`=l_u_a_XX'{	
/////////// NB: in the case of unbalanced panels, it can happen that the U_Gg`i'_XX are not computed by program 2 (for example when y is missing). Consequently, for the command not to display an error message and continue running, we need to verify the variable is created, which is conditional on  N1_`i'_XX!=0.
if N1_`i'_XX!=0{
		replace U_Gg`i'_plus_XX = U_Gg`i'_XX
		replace count`i'_plus_XX= count`i'_XX
}

	}
	
if sum_N1_l_XX!=0{
	replace U_Gg_plus_XX = U_Gg_XX
	scalar U_Gg_den_plus_XX=U_Gg_den_XX
	}
	}

// For switchers out
if ("`switchers'"==""|"`switchers'"=="out"){
	did_multiplegt_var_core_v8 outcome_XX group_XX time_XX treatment_XX, dynamic(`dynamic') switchers_core(out)
	
// Store the results
forvalue i=0/`=l_u_a_XX'{
if N0_`i'_XX!=0{
		replace U_Gg`i'_minus_XX = - U_Gg`i'_XX
		replace count`i'_minus_XX= count`i'_XX
}
	
	}
	
if sum_N0_l_XX!=0{
	replace U_Gg_minus_XX = - U_Gg_XX
	scalar U_Gg_den_minus_XX=U_Gg_den_XX
	}
	
	}


///// Aggregating the obtained results. //

///// Computing the DID_l s. //

// Creating a matrix for the results
matrix mat_res_XX = J(l_XX+2,6,.)

// Computing first the "global" U_Ggl s
forvalue i=0/`=l_XX'{ 
gen U_Gg`i'_global_XX = (N1_`i'_XX/(N1_`i'_XX+N0_`i'_XX))*U_Gg`i'_plus_XX +(N0_`i'_XX/(N1_`i'_XX+N0_`i'_XX))*U_Gg`i'_minus_XX
gen count`i'_global_XX=count`i'_plus_XX+count`i'_minus_XX


// Summing them to obtain the DID_l s (equation 15 of the pdf asymptotic_variances)
egen DID_`i'_XX = total(U_Gg`i'_global_XX) 
replace  DID_`i'_XX = DID_`i'_XX/G_XX
scalar DID_`i'_XX = DID_`i'_XX


if ("`switchers'"==""&N1_`i'_XX==0&N0_`i'_XX==0)|("`switchers'"=="out"&N0_`i'_XX==0)|("`switchers'"=="in"&N1_`i'_XX==0){
	scalar DID_`i'_XX=.
}

// Storing the results and the number of switchers into the matrix mat_res_XX
* Storing the results 
matrix mat_res_XX[`i'+1,1]=DID_`i'_XX 
local rownames "`rownames' Effect_`i'"
* Number of switchers
scalar N_switchers_effect_`i'_XX=N1_`i'_XX+N0_`i'_XX
matrix mat_res_XX[`i'+1,6]=N_switchers_effect_`i'_XX
* Number of observations used in the estimation
egen N_effect_`i'_XX = total(count`i'_global_XX) 
scalar N_effect_`i'_XX = N_effect_`i'_XX
matrix mat_res_XX[`i'+1,5]=N_effect_`i'_XX
}

///// Computing \hat{\delta}_+. //

// Computing the weight w_+.

if "`switchers'"==""{
	scalar w_plus_XX = U_Gg_den_plus_XX*sum_N1_l_XX/(U_Gg_den_plus_XX*sum_N1_l_XX+U_Gg_den_minus_XX*sum_N0_l_XX)
}

if "`switchers'"=="out"{
	scalar w_plus_XX=0
}

if "`switchers'"=="in"{
	scalar w_plus_XX=1
}

// Aggregating to obtain the "global" U_Gg s
gen U_Gg_global_XX = w_plus_XX*U_Gg_plus_XX +(1-w_plus_XX)*U_Gg_minus_XX

egen delta_XX = total(U_Gg_global_XX)
replace delta_XX = delta_XX/G_XX
scalar delta_XX = delta_XX

// Completing the results matrix
* Storing the results
matrix mat_res_XX[l_XX+2,1]=delta_XX
local rownames "`rownames' Average"
* Number of switchers
scalar N_switchers_effect_XX=0
forvalue i=0/`=l_XX'{
	scalar N_switchers_effect_XX = N_switchers_effect_XX + N_switchers_effect_`i'_XX
}
matrix mat_res_XX[l_XX+2,6]=N_switchers_effect_XX
* Number of observations used in the estimation
scalar N_effect_XX=0
forvalue i=0/`=l_XX'{
	scalar N_effect_XX = N_effect_XX + N_effect_`i'_XX
}

matrix mat_res_XX[l_XX+2,5]=N_effect_XX



*****  Estimating the asymptotic variances ***********************************

///// Estimating \hat{\sigma}^2_l and the confidence intervals //
forvalue i=0/`=l_XX'{
if ("`switchers'"==""&(N1_`i'_XX!=0|N0_`i'_XX!=0))|("`switchers'"=="out"&N0_`i'_XX!=0)|("`switchers'"=="in"&N1_`i'_XX!=0){ 

// Estimating \hat{\sigma}^2_l
capture drop as_var_`i'_temp_XX
capture drop as_var_`i'_XX

gen as_var_`i'_temp_XX = (U_Gg`i'_global_XX - DID_`i'_XX)^2
egen as_var_`i'_XX=total(as_var_`i'_temp_XX) 

scalar se_`i'_XX = sqrt(as_var_`i'_XX/G_XX^2)
matrix mat_res_XX[`i'+1,2]=se_`i'_XX
	
// Lower bound of the 95% confidence interval
scalar LB_CI_`i'_XX = DID_`i'_XX - 1.96*se_`i'_XX
matrix mat_res_XX[`i'+1,3]= LB_CI_`i'_XX

// Upper bound of the 95% confidence interval
scalar UB_CI_`i'_XX = DID_`i'_XX + 1.96*se_`i'_XX
matrix mat_res_XX[`i'+1,4]=UB_CI_`i'_XX


}
}


///// Estimating \hat{\sigma}^2 and the confidence interval for the average effect //
if ("`switchers'"==""&(sum_N1_l_XX!=0|sum_N0_l_XX!=0))|("`switchers'"=="out"&sum_N0_l_XX!=0)|("`switchers'"=="in"&sum_N1_l_XX!=0){

// Estimating \hat{\sigma}^2
capture drop as_var_temp_XX
capture drop as_var_XX

gen as_var_temp_XX = (U_Gg_global_XX - delta_XX)^2 
egen as_var_XX=total(as_var_temp_XX) 

scalar se_XX = sqrt(as_var_XX/G_XX^2)
matrix mat_res_XX[l_XX+2,2]=se_XX

// Lower bound of the 95% confidence interval
scalar LB_CI_XX = delta_XX - 1.96*se_XX
matrix mat_res_XX[l_XX+2,3]= LB_CI_XX

// Lower bound of the 95% confidence interval
scalar UB_CI_XX = delta_XX + 1.96*se_XX
matrix mat_res_XX[l_XX+2,4]= UB_CI_XX

}

	} // end of the quietly condition

	
	
	
	
*****  Returning the results of the estimation. ******************************

ereturn clear
matrix rownames mat_res_XX= `rownames'
matrix colnames mat_res_XX= "Estimate" "SE" "LB CI" "UB CI" "N" "Switchers"
noisily matlist mat_res_XX
ereturn matrix estimates = mat_res_XX

 
end



********************************************************************************
*                                 PROGRAM 2                                    *
********************************************************************************

capture program drop did_multiplegt_var_core_v8


program did_multiplegt_var_core_v8, eclass
	version 12.0
	syntax varlist(min=4 max=4 numeric) [if] [in] [, dynamic(integer 0) switchers_core(string)]

	qui{
	
///// Initializing depending on whether we consider switchers in or switchers out: indicating the number of dynamic effects to estimate and the index for whether we compute N^1_l or N^0_l. //
	
if "`switchers_core'"=="in"{
scalar l_u_a_XX=min(`dynamic', L_u_XX)
scalar increase_XX=1
}

if "`switchers_core'"=="out"{
scalar l_u_a_XX=min(`dynamic', L_a_XX)
scalar increase_XX=0
}


*****  Estimating the DID_{+,l}s or the DID_{-,l}s *****************************

////// Loop to estimate the dynamic effects //
forvalue i=0/`=l_u_a_XX'{
	
capture drop distance_to_switch_`i'_XX
capture drop never_change_d_`i'_XX
capture drop N_never_change_d_`i'_XX
capture drop N`=increase_XX'_t_`i'_XX
capture drop N`=increase_XX'_t_`i'_g_XX
capture drop N_gt_control_`i'_XX
capture drop diff_y_`i'_XX
capture drop diff_y_`i'_XX_temp
capture drop dummy_for_U_Gg`i'_temp_XX
capture drop U_Gg`i'_temp_XX
capture drop U_Gg`i'_XX
capture drop count`i'_XX_temp
capture drop count`i'_XX

////// Creating long difference of outcome and differences //
sort group_XX time_XX
if `i'==0{
	gen diff_y_`i'_XX = diff_y_XX
	gen diff_y_`i'_XX_temp = diff_y_XX
}

if `i'>0{
	bys group_XX : gen diff_y_`i'_XX_temp = diff_y_`=`i'-1'_XX_temp + diff_y_XX[_n+`i']
	bys group_XX : gen diff_y_`i'_XX = diff_y_`i'_XX_temp[_n-`i']
}

* Do the same for all the covariates.
* Perform the reisudalization: remove from diff_y_`i'_XX the part that can be explained by withdrawing theta_d_sq_XX*(matrix of covariates)

////// Identifying the control (g,t)s in the estimation of dynamic effect i //
bys group_XX: gen never_change_d_`i'_XX=(F_g_XX>time_XX) if diff_y_`i'_XX!=.

////// Counting the number of controls (g,t)s at each time period //
*NB: when N_gt_XX will not only be 0 or 1, the line below should become something like
*bys time_XX: egen N_u_t_`i'_XX=total(N_gt_XX) if never_change_d_`i'_XX==1.

// N^g_t
bys time_XX d_sq_XX : egen N_gt_control_`i'_XX=total(never_change_d_`i'_XX) 


///// binary variable indicating whether group g is l periods away from switch //
gen distance_to_switch_`i'_XX=(time_XX==F_g_XX+`i'&`i'<=L_g_XX&R_g_XX==increase_XX&N_gt_control_`i'_XX>0&N_gt_control_`i'_XX!=.) if diff_y_`i'_XX!=.

///// Computing N^1_{t,l} or N^0_{t,l}. //
bys time_XX: egen N`=increase_XX'_t_`i'_XX=total(distance_to_switch_`i'_XX)

///// Computing N^1_l or N^0_l. //
forvalue t=`=t_min_XX'/`=T_max_XX'{
	sum N`=increase_XX'_t_`i'_XX if time_XX==`t'
	scalar N`=increase_XX'_`i'_XX = N`=increase_XX'_`i'_XX + r(mean)
}

///// Computing N^0_{t,l,g} or N^1_{t,l,g}. //
bys time_XX d_sq_XX : egen N`=increase_XX'_t_`i'_g_XX=total(distance_to_switch_`i'_XX)

///// If the dynamic effect could be estimated (as there are switchers), we compute it. //
if N`=increase_XX'_`i'_XX!=0{ 

///// Computing the U^+_{G,g,l} //
// Creating a dummy variable indicating whether l<=T_g_XX-2
gen dummy_for_U_Gg`i'_temp_XX = (`i'<=T_g_XX-2)
// Computing (g,t) cell U^+_{G,g,l}
gen U_Gg`i'_temp_XX = dummy_for_U_Gg`i'_temp_XX*(G_XX / N`=increase_XX'_`i'_XX) * N_gt_XX * [distance_to_switch_`i'_XX - (N`=increase_XX'_t_`i'_g_XX/N_gt_control_`i'_XX) * never_change_d_`i'_XX] * diff_y_`i'_XX 


gen count`i'_XX_temp=(U_Gg`i'_temp_XX!=.&U_Gg`i'_temp_XX!=0|(U_Gg`i'_temp_XX==0&diff_y_`i'_XX==0&(distance_to_switch_`i'_XX!=0|(N`=increase_XX'_t_`i'_g_XX!=0&never_change_d_`i'_XX!=0))))

bysort group_XX : egen U_Gg`i'_XX=total(U_Gg`i'_temp_XX)
bysort group_XX : egen count`i'_XX=total(count`i'_XX_temp)  
 
replace U_Gg`i'_XX = U_Gg`i'_XX*first_obs_by_gp_XX
replace count`i'_XX = count`i'_XX*first_obs_by_gp_XX

}

}


*** For the estimation of \hat{\delta} ***

///// Computing the sum of the N1_`i'_XX for the weights w. //
scalar sum_N`=increase_XX'_l_XX = 0
forvalue i=0/`=l_u_a_XX'{
	scalar sum_N`=increase_XX'_l_XX = sum_N`=increase_XX'_l_XX + N`=increase_XX'_`i'_XX
}	

capture drop delta_XX
capture drop U_Gg_XX
capture drop U_Gg_num_XX
capture drop U_Gg_den_XX
gen U_Gg_num_XX=0
gen U_Gg_den_XX=0


forvalue i=0/`=l_u_a_XX'{
	
if N`=increase_XX'_`i'_XX!=0{
capture drop delta_D_`i'_temp_XX
capture drop delta_D_`i'_XX
	
	
////// Computing the weights w. //
scalar w_`i'_XX = N`=increase_XX'_`i'_XX / sum_N`=increase_XX'_l_XX
	
	
///// Computing the delta^D_{+,l}s which are necessary for the denominator of \hat{\delta}_+. //
gen delta_D_`i'_temp_XX = N_gt_XX/N`=increase_XX'_`i'_XX*[(treatment_XX-d_sq_XX)* R_g_XX + (1-R_g_XX)*(d_sq_XX-treatment_XX)] if R_g_XX==increase_XX&F_g_XX<=T_g_XX-`i'&time_XX==F_g_XX+`i'&diff_y_`i'_XX!=.&N_gt_control_`i'_XX>0&N_gt_control_`i'_XX!=.

replace delta_D_`i'_temp_XX=0 if delta_D_`i'_temp_XX==.
egen delta_D_`i'_XX = total(delta_D_`i'_temp_XX)
drop delta_D_`i'_temp_XX	
	
	
///// Computing the numerator of U^+_{G,g}: summing up the U_{G,g,l}s, after weighting them. //
bys group_XX : replace U_Gg_num_XX = U_Gg_num_XX + w_`i'_XX * U_Gg`i'_XX
	
	
///// Computing the denominator of U^+_{G,g}: summing up the delta^D_{+,l}s, after weighting them.  //
bys group_XX : replace U_Gg_den_XX = U_Gg_den_XX + w_`i'_XX * delta_D_`i'_XX
}


}
		
///// Computing the U^+_{G,g}s. //
bys group_XX : gen U_Gg_XX = U_Gg_num_XX/U_Gg_den_XX


	} // end of the quietly condition

end
