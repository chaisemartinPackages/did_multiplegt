# did_multiplegt

Library of Estimators in Difference-in-Difference (DID) designs with multiple groups and periods.

## Setup

### Stata
```stata
ssc install did_multiplegt, replace
```

## Syntax

```stata
did_multiplegt (mode) Y G T D [if] [in] [, options]
```

## Description

**did_multiplegt** wraps in a single command all the estimators from de Chaisemartin and D'Haultfoeuille. Depending on the {cmd:mode} argument, this command can be used to call the following estimators.

+ [did_multiplegt_dyn](https://github.com/chaisemartinPackages/did_multiplegt_dyn). In **dyn** mode, the command computes the DID event-study estimators introduced in de Chaisemartin and D'Haultfoeuille (2024a). This mode can be used both with a binary and staggered (absorbing) treatment and a non-binary treatment (discrete or continuous) that can increase or decrease multiple times. The estimator is also robust to heterogeneous effects of the current and lagged treatments. Lastly, it can be used with data where the panel structure is unblanced or more disaggregated than the group level.
+ [did_had](https://github.com/chaisemartinPackages/did_multiplegt). In **had** mode, the command computes the DID estimator introduced in de Chaisemartin and D'Haultfoeuille (2024b). This mode estimates the effect of a treatment on an outcome in a heterogeneous adoption design (HAD) with no stayers but some quasi stayers. 
+ [did_multiplegt_old](https://github.com/chaisemartinPackages/did_multiplegt/tree/main/did_multiplegt_old). In **old** mode, the command computes the DID estimators introduced in de Chaisemartin and D'Haultfoeuille (2020). This mode corresponds to the old version of the did_multiplegt command. Specifically, it can be used to estimate DID_M, i.e. the average across t and d of the treatment effects of groups that have treatment d at t-1 and change their treatment at t, using groups that have treatment d at t-1 and t as controls. This mode could also be used to compute event-study estimates, but we strongly suggest to use the **dyn** mode, since it is way faster and includes comprehensive estimation and post-estimation support.

The conversion of **did_multiplegt** to a library so far involves only the Stata version. The R version of **did_multiplegt** is still available on CRAN:
```r
install.packages("DIDmultiplegt", force = TRUE)
```

**did_multiplegt** updates automatically all the packages above (on average) every 100 runs of the command. Self-updates can be stopped by specifying the command with the **no_updates** option.

## Arguments

+ **mode** is the command selector and can be only be {cmd:dyn}, {cmd:had} or {cmd:old}.
+ **Y** is the outcome variable.
+ **G** is the group variable.
+ **T** is the time period variable.
+ **D** is the treatment variable.
+ **options** is a pass-through and can include all the options of the command called with **mode**. It can include the **no_updates** option, which will apply only for **did_multiplegt** and will not be passed onto the **mode** options.

## Example: Estimating the effect of union membership on wages

Loading the worker-year level data from Vella and Verbeek (1998):
```stata
bcuse wagepan, clear
```

Computing DID_M from de Chaisemartin and D'Haultfoeuille (2020):
```stata
did_multiplegt (old) lwage nr year union, breps(100) cluster(nr)
```

Computing 5 dynamic effects and 2 placebos using DID_l from de Chaisemartin and D'Haultfoeuille (2024a):
```stata
did_multiplegt (dyn) lwage nr year union, effects(5) placebo(2) graph_off
```

## Authors

+ Clément de Chaisemartin, Economics Department, Sciences Po, France.
+ Diego Ciccia, Sciences Po, France.
+ Xavier D'Haultfoeuille, CREST-ENSAE, France.
+ Felix Knau, Sciences Po, France.
+ Mélitine Malézieux, Stockholm School of Economics, Sweden.
+ Doulo Sow, Sciences Po, France.

## References

de Chaisemartin, C and D'Haultfoeuille, X (2020). American Economic Review, vol. 110, no. 9. [Two-Way Fixed Effects Estimators with Heterogeneous Treatment Effects](https://www.aeaweb.org/articles?id=10.1257/aer.20181169)

de Chaisemartin, C and D'Haultfoeuille, X (2024a). Review of Economics and Statistics, 1-45. [Difference-in-Differences Estimators of Intertemporal Treatment Effects](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3731856)

de Chaisemartin, C and D'Haultfoeuille, X (2024b). [Two-way Fixed Effects and Differences-in-Differences Estimators in Heterogeneous Adoption Designs](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4284811)

Vella, F. and Verbeek, M. 1998. Journal of Applied Econometrics 13(2), 163–183. [Whose wages do unions raise? a dynamic model of unionism and wage rate determination for young men](https://onlinelibrary.wiley.com/doi/abs/10.1002/(SICI)1099-1255(199803/04)13:2%3C163::AID-JAE460%3E3.0.CO;2-Y)
