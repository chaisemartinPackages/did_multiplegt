*** Meeting with Clément - 17.03 ***

********************************************************************************
* Toy data sets                                                                *
********************************************************************************

drop _all
set obs 24
set seed 34
gen g=floor((_n-1)/4)+1
gen t=_n-4*floor((_n-1)/4)

// a switcher in and a control for status quo 0
gen d=0
replace d=1 if g==2&t>=3

// a switcher in and a control for status quo 1
replace d=1 if g==3|g==4
replace d=2 if g==4&t>=2

// a switcher out and a control for status quo 2
replace d=2 if g==5|g==6
replace d=1 if g==6&t>=3

br g t d

gen X1=uniform()
gen X2=uniform() //+0.3*X1

*gen y0=3*(t==2)+9*(t==3)+13*(t==4)+X1+2*X2
gen y0=X1+2*X2

gen y = y0 + 2*d
replace y=y-1 if d[_n-1]==1&g==g[_n-1]
replace y=y-1 if d[_n-2]==1&g==g[_n-2]

gen TE = 2*d
replace TE=TE-1 if d[_n-1]==1&g==g[_n-1]
replace TE=TE-1 if d[_n-2]==1&g==g[_n-2]


did_multiplegt y g t d, dynamic(1) robust_dynamic  controls(X1 X2) cluster(g)

//did_multiplegt_var y g t d, dynamic(1)  controls(X1 X2)

did_multiplegt_var_v10 y g t d, dynamic(1)  controls(X1 X2)

// True results are:
// Effect_0 = 2
// Effect_1 =2.3

// the old command behaves unexpectedly, it is wrong.

********************************************************************************

// Running an example with more groups to see if the displayed error messages evolve.
drop _all
set obs 72
set seed 34
gen g=floor((_n-1)/4)+1
gen t=_n-4*floor((_n-1)/4)

// a switcher in and a control for status quo 0
gen d=0 if g<=6
replace d=1 if g>=3&g<=6&t>=3

// a switcher in and a control for status quo 1
replace d=1 if g>6&g<=12
replace d=2 if g>=9&g<=12&t>=2

// a switcher out and a control for status quo 2
replace d=2 if g>12&g<=18
replace d=1 if g>=15&g<=18&t>=3

br g t d

gen X1=uniform()
gen X2=uniform() //+0.3*X1

//gen y0=t+g^2*t+X1+2*X2
gen y0=X1+2*X2

gen y = y0 + 2*d
replace y=y-1 if d[_n-1]==1&g==g[_n-1]
replace y=y-1 if d[_n-2]==1&g==g[_n-2]

gen TE = 2*d
replace TE=TE-1 if d[_n-1]==1&g==g[_n-1]
replace TE=TE-1 if d[_n-2]==1&g==g[_n-2]


did_multiplegt y g t d, dynamic(1) robust_dynamic  controls(X1 X2)

//did_multiplegt_var y g t d, dynamic(1)  controls(X1 X2)

did_multiplegt_var_v10 y g t d, dynamic(1)  controls(X1 X2)

// Here, displayed results are the same, which confirms Clément's intuition.


********************************************************************************

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
gen X3=uniform()
replace X3 = uniform() +0.7*X2 if d==1
//gen y0=t+g^2*t+X1+X2
gen y0=X1+X2+X3

gen y=.
sort g t
replace y=y0+2*d
replace y=y+1 if d[_n-1]==1&g==g[_n-1]
replace y=y+2 if d[_n-1]==2&g==g[_n-1]

did_multiplegt y g t d, dynamic(2) robust_dynamic  controls(X1 X2 X3) cluster(g)

//set trace on 
did_multiplegt_var_v10 y g t d, dynamic(2)  controls(X1 X2 X3)
//set trace off

// Here, results are the same.

********************************************************************************
* Real data applications.                                                      *
********************************************************************************

* Union data

use "/Users/148715/Documents/WAGEPAN.DTA", clear

did_multiplegt_var_v10 lwage nr year union, dynamic(2) controls(educ south poorhlth black hours)

did_multiplegt lwage nr year union, robust_dynamic dynamic(2) controls(educ south poorhlth black hours) breps(3) cluster(nr)

// results are the same

// just to check:
did_multiplegt_var_v10 lwage nr year union, dynamic(2) 
did_multiplegt lwage nr year union, robust_dynamic dynamic(2) breps(100) cluster(nr)

// Here, results of the two commands are the same.

********************************************************************************
* Gentzkow data

use "/Users/148715/C DE CHAISEMARTIN Dropbox/Mélitine Malézieux/Melitine_RAship/did_multiplegt/test_dofiles_and_datasets/voting_cnty_clean.dta", clear

//gen D_ishare_town2 = D_ishare_town*2
did_multiplegt_var_v10 prestout cnty90 year numdailies , dynamic(2) controls(/*D_ishare_town2*/ preseligible D_ishare_town)

did_multiplegt prestout cnty90 year numdailies, dynamic(2) robust_dynamic cluster(cnty90) breps(0) controls( preseligible D_ishare_town)

// e(effect_0) =  .0032113395837595
// e(effect_1) =  .0006728797605313
//  e(effect_2) =  .0005675104991679

// Here, the old and the new command do not display the same results.

********************************************************************************
* Clémence data

use "/Users/148715/Chaisemartin Dropbox/Mélitine Malézieux/Mac/Documents/Work/tests/database_CTricaud.dta", clear

gen G=code_insee
gen T=year
gen D=integration
gen Y=access_transport

timer clear

timer on 1  
did_multiplegt_var_v10 Y G T D, dynamic(2) controls(above_five building_permits)
timer off 1
// 14 sec

timer on 2
did_multiplegt Y G T D, dynamic(2) robust_dynamic breps(3) controls(above_five building_permits) cluster(G)
timer off 2
// 12 sec

timer list

// Both commands display the exact same results.
