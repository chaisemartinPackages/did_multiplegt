# did_multiplegt
Estimation in Difference-in-Difference (DID) designs with multiple groups and periods.

**Short description**

did_multiplegt estimates the effect of a treatment on an outcome, using group- (e.g. county- or state-) level panel data with multiple groups and periods. The panel of groups may be unbalanced: not all groups have to be observed at every period (see FAQ section for more info on that).  The data may also be at a more disaggregated level than the group level (e.g. individual-level wage data to measure the effect of a regional-level minimum-wage on individuals' wages). The treatment doesn't need to be binary.

**Installation**

Check out the command in stata for more details.

```applescript
ssc install did_multiplegt, replace
```

**References**

de Chaisemartin, C andD'Haultfoeuille,X (2020a).American Economic Review, vol. 110, no. 9. [Two-Way Fixed Effects Estimators with HeterogeneousTreatment Effects.
](https://www.aeaweb.org/articles?id=10.1257/aer.20181169)

de Chaisemartin, C andD'Haultfoeuille,X (2020b).[Difference-in-Differences Estimators of Intertemporal Treatment Effects.
](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3731856)

de Chaisemartin, C andD'Haultfoeuille,X (2020c). [Two-way fixed effects regressions with several treatments.
](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3751060)

Vella, F. and Verbeek,M. 1998. Journalof AppliedEconometrics 13(2), 163–183. [Whose wages do unions raise? a dynamic model of unionism and wagerate determinationfor young men.
](https://onlinelibrary.wiley.com/doi/abs/10.1002/(SICI)1099-1255(199803/04)13:2%3C163::AID-JAE460%3E3.0.CO;2-Y) 
