** February, Wednesday 22nd
** This do file to test whether the new command deals with controls the same way as the old command does.

********************************************************************************
* WITH TOY DATA SETS                                                           *
********************************************************************************

// DGP with AT=NT=T

drop _all
set obs 16
gen g=floor((_n-1)/4)+1
gen t=_n-4*floor((_n-1)/4)
gen d=(t>1)
replace d=0 if g==1|g==4
replace d=1 if g==3
gen y0 = g+t  
gen y = y0 + 2*d
replace y=y-1 if d[_n-1]==1&g==g[_n-1]
replace y=y-1 if d[_n-2]==1&g==g[_n-2]
drop y0

rename y resid
g x= rnormal(0,0.5)
g y= x + resid

// Avec did_multiplegt :
// On devrait avoir: DID_{+,0}=2, DID_{+,1}=1, DID_{+,2}=0.
// et delta_+=1

// Avec la modification :
// étant donné que le traitement est binaire, les mêmes résultats sont attendus.

did_multiplegt y g t d, dynamic(2) controls(x) robust_dynamic 

//set trace on
did_multiplegt_var_v9 y g t d, dynamic(2) controls(x)
//set trace off

********************************************************************************
// DGP with non binary treatment

drop _all
set obs 32
set seed 3
gen g=floor((_n-1)/4)+1
gen t=_n-4*floor((_n-1)/4)
gen d=(uniform()>=2/3)+(uniform()>=1/3)
br g t d
replace d=1 if g==2
replace d=0 if g==4
replace d=2 if g==8
br g t d
set obs 40
replace t=0 if _n>32
replace g=_n-32 if _n>32
set obs 48
replace t=-1 if _n>40
replace g=_n-40 if _n>40
set obs 56
replace t=-2 if _n>48
replace g=_n-48 if _n>48
sort g t
replace d=d[_n+1] if t==0
replace d=d[_n+1] if t==-1
replace d=d[_n+1] if t==-2
br g t d

gen X1=uniform()
gen X2=uniform()
//gen y0=t+g^2*t+X1+X2
gen y0=X1+X2

gen y=.
sort g t
replace y=y0+2*d
replace y=y+1 if d[_n-1]==1&g==g[_n-1]
replace y=y+2 if d[_n-1]==2&g==g[_n-1]

did_multiplegt y g t d, dynamic(2) robust_dynamic  controls(X1 X2)

//set trace on 
did_multiplegt_var_v9 y g t d, dynamic(2)  controls(X1 X2)
//set trace off

********************************************************************************
// Toy data set with unbalanced covariates : does did_multiplegt reject the dynamic effects coming after a covariate is missing ?

clear
drop _all
set obs 40
set seed 1
gen g=floor((_n-1)/10)+1 
gen t=_n-10*floor((_n-1)/10) 

// switcher in
gen d=0
replace d=1 if g==2&t>=3

// switcher out
replace d=1 if g==3|(g==4&t<=4)

br g t d


gen X1=uniform()
gen X2=uniform()

gen y0=X1+2*X2


// Missing values for covariates
replace d=. if g==2&t==4

gen y=.
sort g t
replace y=y0+2*d
replace y=y+1 if d[_n-1]==1&g==g[_n-1]
replace y=y+2 if d[_n-1]==2&g==g[_n-1]



did_multiplegt y g t d, dynamic(2) robust_dynamic controls(X1 X2)
did_multiplegt_var_v9 y g t d, dynamic(2) controls(X1 X2)

// the old version of the command 


********************************************************************************
* WITH REAL DATA                                                               *
********************************************************************************

use "/Users/148715/C DE CHAISEMARTIN Dropbox/Mélitine Malézieux/Melitine_RAship/did_multiplegt/test_dofiles_and_datasets/voting_cnty_clean.dta", clear

set matsize 800

did_multiplegt prestout cnty90 year numdailies, dynamic(2) controls(ishare_urb preseligible D_ishare_town) robust_dynamic breps(10)

set trace on
did_multiplegt_var_v9 prestout cnty90 year numdailies, dynamic(2) controls(ishare_urb preseligible D_ishare_town)
set trace off

// not the same results, but makes sense as there are some missing treatments and outcomes...

********************************************************************************
 
use "/Users/148715/Chaisemartin Dropbox/Mélitine Malézieux/Mac/Documents/Work/tests/database_CTricaud.dta", clear

gen G=code_insee
gen T=year
gen D=integration
gen Y=access_transport

did_multiplegt_var_v9 Y G T D, dynamic(2) controls(above_five building_permits)
did_multiplegt Y G T D, dynamic(2) controls(above_five building_permits) robust_dynamic breps(3)

// same results !
// Just to check:
did_multiplegt_var_v9 Y G T D, dynamic(2)
did_multiplegt Y G T D, dynamic(2) robust_dynamic breps(3)


********************************************************************************

use "/Users/148715/Documents/WAGEPAN.DTA", clear

did_multiplegt_var_v9 lwage nr year union, dynamic(2) controls(educ south poorhlth black hours) //switchers(out)
did_multiplegt lwage nr year union, robust_dynamic dynamic(2) controls(educ south poorhlth black hours) breps(100) cluster(nr) //switchers(out)

// results are the same
// just to check:
did_multiplegt_var_v9 lwage nr year union, dynamic(2) 
did_multiplegt lwage nr year union, robust_dynamic dynamic(2) breps(100) cluster(nr)

********************************************************************************
* With missing values for some covariates                                      *
********************************************************************************

use "/Users/148715/Documents/WAGEPAN.DTA", clear

// Make some covariates values missing
set seed 456
gen missing_educ=(uniform()<=0.02)
gen missing_south=(uniform()<=0.02)
gen missing_hours=(uniform()<=0.02)
replace educ=. if missing_educ==1
replace south=. if missing_south==1
replace hours=. if missing_hours==1

did_multiplegt_var_v8 lwage nr year union, dynamic(2) controls(educ south poorhlth black hours)
did_multiplegt lwage nr year union, robust_dynamic dynamic(2) controls(educ south poorhlth black hours) breps(3)

// not the same results !

********************************************************************************

// DGP
drop _all
set obs 32
set seed 3
gen g=floor((_n-1)/4)+1
gen t=_n-4*floor((_n-1)/4)
gen d=(uniform()>=2/3)+(uniform()>=1/3)
br g t d
replace d=1 if g==2
replace d=0 if g==4
replace d=2 if g==8
br g t d
set obs 40
replace t=0 if _n>32
replace g=_n-32 if _n>32
set obs 48
replace t=-1 if _n>40
replace g=_n-40 if _n>40
set obs 56
replace t=-2 if _n>48
replace g=_n-48 if _n>48
sort g t
replace d=d[_n+1] if t==0
replace d=d[_n+1] if t==-1
replace d=d[_n+1] if t==-2
br g t d

// Covariates depend on one another
gen X1=uniform()+t 
gen X2=uniform()+2*t + 0.01*X1
gen X3=uniform()+3*t + 0.02*X2
replace X1=X1 + 0.03*X3

replace X1=. if (t==0&g==6)|(t==2&g==8)

gen y=g*t+X1+2*X2+3*X3
*gen y=g+t+X1+2*X2+3*X3
*gen y=X1+2*X2+3*X3

/*
gen y=.
sort g t
replace y=y0+2*d
replace y=y+1 if d[_n-1]==1&g==g[_n-1]
replace y=y+2 if d[_n-1]==2&g==g[_n-1]
*/

// Computing first-differences of the variables
xtset g t
gen FD_y = d.y
gen FD_X1 = d.X1
gen FD_X2 = d.X2
gen FD_X3 = d.X3

// Identifying control groups
gen d_sq_XX=d
replace d_sq_XX=d_sq_XX[_n-1] if g==g[_n-1]

gen diff_from_sq_XX=(d-d_sq_XX)
gen ever_change_d_XX=(abs(diff_from_sq_XX)>0&d!=.) 
replace ever_change_d_XX=1 if ever_change_d_XX[_n-1]==1&g==g[_n-1]


////// Option 1: Performing the regression of Y(g,t)-Y(g,t-1) on X(g,t)-X(g,t-1) and time fixed effects. Omly among controls and units having the same status quo !
reghdfe FD_y FD_X1 FD_X2 FD_X3 if ever_change_d_XX==0&d_sq_XX==2, absorb(t)
matrix option_1 = e(b)

////// Option 2: Computing by hand the coefficients, keeping in mind that the residuals of the regression of X(g,t)-X(g,t-1) on time fixed effects are equal to X(g,t)-X(g,t-1) - [Xd(.,t)-Xd(.,t-1)], where Xd(.,t) represents the average value of X(g,t) over groups whose status quo treatment equals d.

// Computing the average value of X(g,t) over groups which never changed treatment yet and whose status quo treatment equals d 
*preserve
*keep if ever_change_d_XX==0&d_sq_XX==2&FD_y!=.&FD_X1!=.&FD_X2!=.&FD_X3!=.

// No need to bys according to the status, because we selected above units with a certain status quo already
bys t d_sq_XX : egen avg_FD_X1 = mean(FD_X1) if ever_change_d_XX==0&FD_y!=.&FD_X1!=.
bys t d_sq_XX : egen avg_FD_X2 = mean(FD_X2) if ever_change_d_XX==0&FD_y!=.&FD_X2!=.
bys t d_sq_XX : egen avg_FD_X3 = mean(FD_X3) if ever_change_d_XX==0&FD_y!=.&FD_X3!=.


// Computing the difference between the first-difference of variables and the first-difference of their average value across groups and status quos
gen resid_X1 = FD_X1 - avg_FD_X1
gen resid_X2 = FD_X2 - avg_FD_X2
gen resid_X3 = FD_X3 - avg_FD_X3


// Use the function matrix accum to compute the wanted product on the control cells
keep if d_sq_XX==2
matrix accum overall = FD_y resid_X1 resid_X2 resid_X3

matrix XX = overall[2..4,2..4]
//matrix XX = XX/35
matrix Xy = overall[2..4,1]
//matrix Xy = Xy/35

// Computing the coefficients
matrix option_2 = invsym(XX)*Xy

*restore

matlist option_1
matlist option_2

// results are the same.

e


********************************************************************************
* With missing outcomes and treatments                                         *
********************************************************************************
