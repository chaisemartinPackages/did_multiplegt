# did_multiplegt
Estimation in Difference-in-Difference (DID) designs with multiple groups and periods.

[Short description](#Short-description) | [Installation](#Installation)

[FAQ section](#FAQ-section) | [References](#References )

## Short description

did_multiplegt estimates the effect of a treatment on an outcome, using group- (e.g. county- or state-) level panel data with multiple groups and periods. 
The panel of groups may be unbalanced: not all groups have to be observed at every period (see FAQ section for more info on that). 
The data may also be at a more disaggregated level than the group level (e.g. individual-level wage data to measure the effect of a regional-level minimum-wage on individuals' wages).
The treatment doesn't need to be binary.

## Installation

### Stata (SSC)
```applescript
ssc install did_multiplegt, replace
```

### R (GitHub)
```applescript
library(devtools)
install_github("chaisemartinPackages/did_multiplegt/R")
```

## FAQ section

> ❓**Do I have to include group- and time- fixed effects in my regression when using the did_multiplegt package?**


No, you do not have to.  Even if you do not specify any of the options controls(varlist), trends_nonparam(varlist) or trends_lin(varlist), group- and time-fixed effects will be accounted for.

> ❓**When does the command produce a table, and what does the table contain?**

If strictly more than one bootstrap replication has been run, the command returns a table with all the estimated treatment effects and placebos, their standard errors, their 95% confidence intervals, the number of observations used in the estimation, and the number of switchers the effects and placebos apply to.  The p-value from the joint test that all placebos are equal to 0 is not shown in the table, but it is stored in e().

> ❓**When does the command produce a graph, and how is the graph constructed?**

If strictly more than one bootstrap replication has been run, the command returns a graph with all the estimated treatment effects and placebos, and their 95%
confidence intervals constructed using a normal approximation.  An exception is when dynamic effects and first-difference placebos are requested.  Then, the command
does not produce a graph, because placebos are first-difference estimators, while dynamic effects are long-difference estimators, so they are not really comparable.
When dynamic effects and long-difference placebos are requested, everything is relative to the period prior to first-switches, referred to as period -1.
Accordingly, the first placebo is shown at period -2 on the graph.

> ❓**My group-level panel is unbalanced: some groups (e.g. counties) are not observed in every year. Can I still use the command?**

You can.  A frequent case of unbalancedness is when some groups are not observed over the full duration of the panel, but all groups are observed at evenly-spaced
intervals over the period where they are observed.  For instance, your data may be a yearly county-level panel from 1990 to 2000, where some counties appear after
1990 while some exit before 2000, but from the year they appear till the year they exit, all counties are observed every year.  Then, there is nothing special you
have to do, you can run the command as is.

A more complicated case is when some groups are observed at unevenly spaced intervals.  For instance, a county is observed in 1990, 1991, 1993, 1994, etc. If you
wish to compute the DID_M estimator, there is nothing special you have to do:  that estimator only relies on first-differences, so it will just not be able to use
the 1993 observation for that county.  If you wish to compute the DID_l estimators, the years where that county is observed will be used in the estimation, except if
that county's 1991 and 1993 treatments differ, and that county had never changed treatment prior to 1991.  Then, the year when that county's treatment changed for
the first time is not known, so the approach implemented by default is to drop it from the estimation.  If many groups are observed at unevenly spaced intervals,
this approach may strongly reduce the sample size.  In that case, you may consider filling the group-level panel's holes, using Stata's fillin command.  Then, you
need to replace a group's missing treatments by the value of that same group's treatment, in the first year after those missing years where the treatment is not
missing.  In the example above, this amounts to replacing the county's 1992 treatment by its 1993 value.  Then, that county will be included in the estimation.
However, the estimation will assume that the county's treatment changed for the first time from 1991 to 1992, and not from 1992 to 1993.  Accordingly, the county's
estimated effect in 1993 will be considered as a dynamic effect 1 period after the first treatment change, while it may be an instantaneous effect.  Without that
assumption, that group cannot be used in the computation of the DID_l estimators, which all require that a group's outcome be observed in the last year before its
treatment changes.

Finally, it may be the case that the data is fully missing at one or several time periods.  For instance, you have data for 1990, 1991, and 1993, but 1992 is missing
for every group.  If you wish to compute the DID_l estimators, it is important to fill the gap in the data, as otherwise the estimation will assume that 1991 and
1993 are as far apart as 1990 and 1991.  There are two ways of doing so.  First, you can append to your data a data set identical to your 1991 data, but with the
year equal to 1992, and the outcome missing for every observation.  This is a conservative solution, where no first treatment change occurring between 1991 and 1993
will be used in the estimation, which may be reasonable because the year in which the change occurred is effectively unknown.  Second, you can append to your data a
data set identical to your 1993 data, with the year equal to 1992, and the outcome missing for every observation.  Then, the first treatment changes occurring
between 1991 and 1993 will be used in the estimation, assuming they all took place between 1991 and 1992.


> ❓**Related to imbalanced panels, my outcomes (and potentially the control variables) are measured less frequently than the treatment.  For instance, the outcome is
measured every two years, but I know the treatment of every group in every year.  How should I proceed?**

To fix ideas, let us first assume that the outcome is measured every two years, but you know the treatment of every group in every year.

If you allow for dynamic effects, you should split the sample into two subsamples, and run the command twice, one time on each of the subsamples.  In the first
estimation, you should include all group*time cells (g,t) such that at t, g's treatment has never changed since the start of the panel, and all (g,t)s such that g's
treatment has changed at least once at t, and the changed occurred at a period where the outcome is observed.  Since the outcome is measured every two years, in that
subsample the first treatment effect (denoted Effect_0) is the instantaneous treatment effect, the second effect (Effect_1) is the effect of having started receiving
the treatment 2 years ago, the third effect (Effect_2) is the effect of having started receiving the treatment 4 years ago, etc.  In the second estimation, you
should include all group*time cells (g,t) such that at t, g's treatment has never changed since the start of the panel, and all (g,t)s such that g's treatment has
changed at least once at t, and the changed occurred at a period where the outcome is not observed.  In that subsample the first treatment effect (denoted Effect_0)
is the effect of having started receiving the treatment 1 year ago, the second effect (Effect_1) is the effect of having started receiving the treatment 3 years ago,
etc.  You may then combine the two sets of estimated effects into one event-study graph, with the only caveat that the "odd" and "even" effects are estimated on
different subsamples.  Importantly, the two estimations have to be run on a dataset at the same bi-yearly level as the outcome variable: the yearly level treatment
information should only be used to select the relevant subsamples.

If you do not allow for dynamic effects, you should also split your sample into two different subsamples and run the command separately on each of them.  In the
first estimation, you should include all group*time cells (g,t) such that D(g,t-1)=D(g,t-2). In that subsample, switchers are such that D(g,t)!=D(g,t-1), and the
command estimates their treatment effect at period t, the period when they switched.  In the first estimation, you should include all group*time cells (g,t) such
that D(g,t)=D(g,t-1). In that subsample, switchers are such that D(g,t-1)!=D(g,t-2), and the command estimates their treatment effect at period t, one period after
they switched. You may average those two effects, weighting them proportionally to their number of switchers.

If the treatment is observed three times more often than the treatment, you just need to follow the same logic, split the sample into three subsamples and run the
command three times, etc.

> ❓**Can I make sure that all the dynamic effects I estimate apply to the same groups?**

With a binary treatment and a balanced panel of groups such that at least one group remains untreated and at least one group remains treated throughout the panel,
this can be achieved as follows.  Assume one estimates two dynamic effects.  Then, let Ylead2 denote the second lead of the outcome variable.  If one writes
if_first_diff(Ylead2!=.) in the command's options, groups for which only the instantaneous or first dynamic effect can be estimated are discarded, and the
instantaneous effect and the first two dynamic effects apply to the same groups.  If the number of observations per group does not change over time, the number of
observations these three effects apply to will also be the same.

> ❓**The number of observations per group (e.g.: counties' populations) is changing over time, and differentially so across groups (e.g.: some counties' populations grow
more than others).  Accordingly, groups whose number of observations grow more receive more weight in the estimates of long-than short-run effects.  Can I avoid such
compositional changes?**

Yes you can.  If your data is not aggregated at the group*period level, start by aggregating the outcome treatment and control variables at that level, and create a
variable equal to the number of observations in each group*period.  Then create a variable time_invariant_weight equal, for each group*period, to the average number
of observations in the group across all periods of the panel.  Finally, just weight the estimation by time_invariant_weight.

> ❓**How many control variables can I include in the estimation?**

The DID_M and DID_l estimators with control variables are similar to those without controls, except that the first-difference of the outcome is replaced by residuals
from regressions of the first-difference of the outcome on the first-differences of the controls and time fixed effects.  Those regressions are estimated in the
sample of control (g,t)s:  (g,t)s such that group g's treatment does not change from t-1 to t for the DID_M estimator, and (g,t)s such that group g's treatment has
not changed till period t for the DID_l estimators.  Those regressions are also estimated separately for each possible value of the treatment, or for each set of
treatment values binned together if the recat_treatment(varlist) option is used.  If the treatment takes values 0, 1, 2, 3, and 4, one regression is estimated for
control (g,t)s with a treatment equal to 0, one regression is estimated for control (g,t)s with a treatment equal to 1, etc.  The number of control variables needs
to be significantly smaller than the number of control (g,t)s in each of those regressions.  Otherwise, those regressions will overfit and produce noisy estimates.
If the number of observations is lower than the number of variables in one of those regressions, the command will run but not all the control variables can be
accounted for, and an error message will let you know about that.
 
> ❓**Can I estimate the effect of a treatment, controlling for other treatments that may also change over the panel?**

The command can compute the DIDM estimator, controlling for other treatments that may change over the panel, as proposed by de Chaisemartin and D'Haultfoeuille
(2020c).  In that case, let othertreat be a variable containing the other treatment, and let fd_othertreat be the first difference of that other treatment.  To
compute the estimator proposed by de Chaisemartin and D'Haultfoeuille (2020c), one should write if_first_diff(fd_othertreat==0) trends_nonparam(othertreat)
always_trends_nonparam in the command's options.  With two rather than one other treatments, one should write if_first_diff(fd_othertreat1==0&fd_othertreat2==0)
trends_nonparam(othertreats) always_trends_nonparam in the command's options, where othertreats is generated as follows:  egen othertreats=group(othertreat1
othertreat2).  Etc. for the cases with three, four ... other treatments.  See de Chaisemartin and D'Haultfoeuille (2020c) for further details.

> ❓**How can I estimate the backward DID in de Chaisemartin and D'Haultfoeuille (2020c)?**

To estimate the backward DID, one just needs to revert the time variable (for example "gen opposite_time = - time"), and use the command with this reverted time
variable.  Additionnally, if one wants to compute the backward DID controlling for other treatments, then the variable specified in the if_first_diff(variable)
option needs to be the lead of the first-difference of the other treatments.

> ❓**How can I use the command to estimate heterogeneous treatment effects?**

The command can easily be used to investigate treatment effect heterogeneity with respect to group-level time-invariant variables.  Let X be one such variable, and
let us assume X is binary.  Then you can just run the command twice, adding first "if X==0" and then "if X==1" at the end, to separately estimate the effects in
groups with X=0/X=1.  Assuming you are clustering your ses at the group level, to test if the effects significantly differ in the two groups you can just use the sum
of the estimated variances in the X==0 and X==1 subgroups to estimate the variance of the difference between two effects.  If your data is at a more disaggregated
level than groups and X is a time-invariant characteristic also at a more disaggregated level (you have X=0 and X=1 observations within the same group), then you can
still use the same procedure to estimate heterogeneous effects, but you will need to bootstrap the command yourself to estimate the variance of the differences
between the effects, if you want your ses to be clustered at the group level.

> ❓**The command takes a lot of time to run, is there a way I can speed it up?**

First, you can decrease the number of bootstrap replications performed.  By default, 50 bootstrap replications are performed, but if you just want to look at the
point estimates, you can decrease the number of replications.

You can also speed it up when the robust_dynamic option is specified.  In staggered adoption designs with a binary treatment, you just need to define a variable
Cohort equal to the time period when group g first gets treated (and to 0 if group g never gets treated).  Then, you need to run did_multiplegt with Cohort instead
of G as the group variable.  If the number of groups is large in your application, using this strategy can divide the computing time by more than 10.  In more
complicated designs with a non-binary and/or non-staggered treatment, the Cohort variable needs to be defined as follows: egen Cohort=group(D1 F increase), where:
D1 is a time-invariant variable equal to group g's treatment at the first period it appears in the data; F is a time-invariant variable equal to the first time
period where group g's treatment changes (and to 0 if group g's treatment never changes); increase is a variable equal to 1 if on average across all time periods,
group g's treatments are strictly higher than D1, and to 0 otherwise.  If the trends_non_param option is specified, Cohort should be defined as follows: egen
Cohort=group(D1 F increase trends_non_param).

If you define Cohort as the group variable, you will not be able to cluster your standard errors at, say, the group level.  Then, you will need to bootstrap the
command yourself to obtain standard errors.  To do so, you just need to: i) preserve the data; ii) draw a bootstrap sample using the bsample command (you can cluster
the bootstrap, say, at the group level); iii) run did_multiplegt with Cohort as the group variable; iv) store the estimates somewhere; v) restore your data; vi)
replicate i) to v), say, 100 times; vii) use the standard error of your estimates across the 100 replications to estimate their actual standard errors.

Specifying Cohort instead of G as the group variable will return numerically equivalent estimates, if your data is at the (g,t) level, the panel of groups is
balanced, the estimation is unweighted or weighted with time-invariant weights (e.g. groups' populations at the start of the panel), and there are no controls or
linear trends.  Otherwise, the two estimations may give different results. For instance, with time-varying weights, cohorts cannot be written as simple averages of
groups with time-invariant weights, and one can show that imposing parallel trends at the group level is not equivalent to imposing it at the cohort level.  Without
controls or linear trends, differences are likely to be small, however.  With controls or linear trends, differences may be larger.
 
> ❓**Is did_multiplegt compatible with the honestdid command?**

 Yes, it is.  You can find an example of how to use honestdid after did_multiplegt [here](https://github.com/mcaceresb/stata-honestdid#staggered-timing-1).

**If your question is not listed above, please send an e-mail at: chaisemartin.packages@gmail.com.**

## References 
📖 

de Chaisemartin, C andD'Haultfoeuille,X (2020a).American Economic Review, vol. 110, no. 9. [Two-Way Fixed Effects Estimators with HeterogeneousTreatment Effects.
](https://www.aeaweb.org/articles?id=10.1257/aer.20181169)

de Chaisemartin, C andD'Haultfoeuille,X (2020b). [Difference-in-Differences Estimators of Intertemporal Treatment Effects.
](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3731856)

de Chaisemartin, C andD'Haultfoeuille,X (2020c). [Two-way fixed effects regressions with several treatments.
](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3751060)

Vella, F. and Verbeek,M. 1998. Journalof Applied Econometrics 13(2), 163–183. [Whose wages do unions raise? a dynamic model of unionism and wagerate determinationfor young men.
](https://onlinelibrary.wiley.com/doi/abs/10.1002/(SICI)1099-1255(199803/04)13:2%3C163::AID-JAE460%3E3.0.CO;2-Y) 
