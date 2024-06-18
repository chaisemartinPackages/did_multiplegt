{smcl}
{* *! version 1  2018-07-27}{...}
{viewerjumpto "Syntax" "did_multiplegt##syntax"}{...}
{viewerjumpto "Arguments" "did_multiplegt##options"}{...}
{viewerjumpto "Description" "did_multiplegt##description"}{...}
{viewerjumpto "Examples" "did_multiplegt##examples"}{...}

{title:Title}

{p 4 8}
{cmd:did_multiplegt} {hline 2} Library of Estimators in 
Difference-in-Difference (DID) designs with multiple groups and periods.
{p_end}

{marker syntax}{...}
{title:Syntax}

{p 4 8}
{cmd:did_multiplegt (mode) Y G T D} {ifin}
[{cmd:, options}]
{p_end}

{marker description}{...}
{title:Description}

{p 4 8}
{cmd:did_multiplegt} wraps in a single command all
the estimators from de Chaisemartin and D'Haultfoeuille.
Depending on the {cmd:mode} argument, this command can be used to call the following estimators.
{p_end}

{p 12 12}
{stata help did_multiplegt_dyn:did_multiplegt_dyn}. In {cmd:dyn} mode, the command computes the DID event-study estimators introduced in de Chaisemartin and D'Haultfoeuille (2024a). This mode can be used both with a binary and staggered (absorbing) treatment and a non-binary treatment (discrete or continuous) that can increase or decrease multiple times. The estimator is also robust to heterogeneous effects of the current and lagged treatments. Lastly, it can be used with data where the panel structure is unblanced or more disaggregated than the group level.
{p_end}

{p 12 12}
{stata help did_had:did_had}. In {cmd:had} mode, the command computes the DID estimator introduced in de Chaisemartin and D'Haultfoeuille (2024b). This mode estimates the effect of a treatment on an outcome in a heterogeneous adoption design (HAD) with no stayers but some quasi stayers. 
{p_end}

{p 12 12}
{stata help did_multiplegt_old:did_multiplegt_old}. In {cmd:old} mode, the command computes the DID estimators introduced in de Chaisemartin and D'Haultfoeuille (2020). This mode corresponds to the old version of the did_multiplegt command. Specifically, it can be used to estimate DID_M, i.e. the average across t and d of the treatment effects of groups that have treatment d at t-1 and change their treatment at t, using groups that have treatment d at t-1 and t as controls. This mode could also be used to compute event-study estimates, but we strongly suggest to use the {cmd:dyn} mode, since it is way faster and includes comprehensive estimation and post-estimation support.
{p_end}

{p 4 8}
{cmd:did_multiplegt} updates automatically all the packages above (on average) every 100 runs of the command. Self-updates can be stopped by specifying the command with the {cmd:no_updates} option.
{p_end}

{marker arguments}{...}
{title:Arguments}

{p 4 8}
{cmd:mode} is the command selector and can be only be {cmd:dyn}, {cmd:had} or {cmd:old}.
{p_end}

{p 4 8}
{cmd:Y} is the outcome variable.
{p_end}

{p 4 8}
{cmd:G} is the group variable.
{p_end}

{p 4 8}
{cmd:T} is the time period variable.
{p_end}

{p 4 4}
{cmd:D} is the treatment variable.
{p_end}

{p 4 4}
{cmd:options} is a pass-through and can include all the options of the command called with {cmd:mode}. It can include the {cmd:no_updates} option, which will apply only for {cmd:did_multiplegt} and will not be passed onto the {cmd:mode} options.
{p_end}

{marker Example}{...}
{title:Example: Estimating the effect of union membership on wages}

Loading the worker-year level data from Vella and Verbeek (1998):
{p 4 8}
{stata bcuse wagepan}
{p_end}

Computing DID_M from de Chaisemartin and D'Haultfoeuille (2020):
{p 4 8}
{stata did_multiplegt (old) lwage nr year union, breps(100) cluster(nr)}
{p_end}

Computing 5 dynamic effects and 2 placebos using DID_l from de Chaisemartin and D'Haultfoeuille (2024a):
{p 4 8}
{stata did_multiplegt (dyn) lwage nr year union, effects(5) placebo(2) graph_off}
{p_end}

{title:References}

{p 4 8}
de Chaisemartin, C and D'Haultfoeuille, X (2020). American Economic Review, vol. 110, no. 9.
{browse "https://www.aeaweb.org/articles?id=10.1257/aer.20181169":Two-Way Fixed Effects Estimators with Heterogeneous Treatment Effects}.
{p_end}
{p 4 8}
de Chaisemartin, C and D'Haultfoeuille, X (2024a). Review of Economics and Statistics, 1-45.
{browse "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3731856":Difference-in-Differences Estimators of Intertemporal Treatment Effects}.
{p_end}
{p 4 8}
de Chaisemartin, C and D'Haultfoeuille, X (2024b).
{browse "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4284811":Two-way Fixed Effects and Differences-in-Differences Estimators in Heterogeneous Adoption Designs}.
{p_end}
{p 4 8}
Vella, F. and Verbeek, M. 1998. Journal of Applied Econometrics 13(2), 163–183.
{browse "https://onlinelibrary.wiley.com/doi/abs/10.1002/(SICI)1099-1255(199803/04)13:2%3C163::AID-JAE460%3E3.0.CO;2-Y":Whose wages do unions raise? a dynamic model of unionism and wage rate determination for young men}.
{p_end}

{title:Authors}

{p 4 4}
Clément de Chaisemartin, Economics Department, Sciences Po, France.
{p_end}
{p 4 4}
Diego Ciccia, Sciences Po, France.
{p_end}
{p 4 4}
Xavier D'Haultfoeuille, CREST-ENSAE, France.
{p_end}
{p 4 4}
Felix Knau, Sciences Po, France.
{p_end}
{p 4 4}
Mélitine Malézieux, Stockholm School of Economics, Sweden.
{p_end}
{p 4 4}
Doulo Sow, Sciences Po, France.
{p_end}

{title:Contact}

{p 4 4}
Mail:
{browse "mailto:chaisemartin.packages@gmail.com":chaisemartin.packages@gmail.com}
{p_end}

{p 4 4}
GitHub:
{browse "https://github.com/chaisemartinPackages":chaisemartinPackages} or individual packages ({browse "https://github.com/chaisemartinPackages/did_multiplegt_dyn":did_multiplegt_dyn} | {browse "https://github.com/chaisemartinPackages/did_had":did_had} | {browse "https://github.com/chaisemartinPackages/did_multiplegt":did_multiplegt}).
{p_end}