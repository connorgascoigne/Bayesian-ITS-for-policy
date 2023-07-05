# Bayesian-ITS-for-policy

## Summary

This document provides the code to run all the analysis for the manuscript [Bayesian Interrupted Time Series for evaluating policy change on mental well-being: an application to England's welfare reform](https://arxiv.org/abs/2306.15525). Within the [Code](https://github.com/connorgascoigne/Bayesian-ITS-for-policy/tree/main/Code) folder there are three seperate folders for the [data exploration](https://github.com/connorgascoigne/Bayesian-ITS-for-policy/tree/main/Code/Data%20Exploration), [data organising](https://github.com/connorgascoigne/Bayesian-ITS-for-policy/tree/main/Code/Data%20Organising), and [model fitting](https://github.com/connorgascoigne/Bayesian-ITS-for-policy/tree/main/Code/Model%20Fitting).

### Data

We do not supply the data, but list what data sources we use, and how they were accessed.

-   We use survey data from the [United Kingdoms Household Longitudinal survey (UKHLS)](https://www.understandingsociety.ac.uk/). The main survey is free to download after registration. UKHLS datasets that require geographical identifiers are only downloadable after special licences have been granted.
-   We use data on Universal Credit freely downloadable after registration from the [Department for Work and Pensions (DWP)](https://stat-xplore.dwp.gov.uk/webapi/jsf/login.xhtml).
-   We use Office for National Statistics (ONS) data on the [Index of Multiple Deprivation (IMD)](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019) that is free to download.
-   We use ONS data on area level [population counts from different ethnic groups (ethinc mix)](https://www.ethnicity-facts-figures.service.gov.uk/uk-population-by-ethnicity/national-and-regional-populations/population-of-england-and-wales/latest) that is free to download.
-   We use ONS [spatial shapefiles](https://geoportal.statistics.gov.uk/) that is free to download for plotting purposes.
-   We use ONS [lookup files](https://geoportal.statistics.gov.uk/) that is free to download to link the different spatial regions to one another.

### [Data Exploration](://github.com/connorgascoigne/Bayesian-ITS-for-policy/tree/main/Code/Data%20Exploration)

1.  Run the `dataExploration.R` to produce spatial plots of the IMD, ethnic mix and when different the intervention occurred for different Local Authorities. It also produces temporal plots of the prevalence of psychological distress by employment status and ethnic group for the three years before and after the intervention.
2.  Run the `basicITSplot.R` to produce an example of a plot from a typical ITS and one from a ITS with exposed and control groups.

### [Data Organisation](https://github.com/connorgascoigne/Bayesian-ITS-for-policy/tree/main/Code/Data%20Organising)

1.  Run the `dataDropoutFlowChat.R` to see the number of drop outs from the selection criteria and missingness.
2.  Run the `dataProcessesing.R` to combine the survey data from the UKHLS with the IMD and ethnic mix data.
    -   This **does not** combining with the Local Authority data for when the intervention begins.

### [Model Fitting](https://github.com/connorgascoigne/Bayesian-ITS-for-policy/tree/main/Code/Model%20Fitting)

1.  Run the `modelSelection.R` to determine what random effects to include in the model and how to include them (i.e., structured or unstructured).
2.  Run the `modelFit.R` to finalise the data processes, fit the model, produce samples from the posterior and save any relevant parts.
    -   Additional data processing: combining with Local Authority data for when the intervention begins, define the weights.
    -   Saved objects: final data set, model fit, and draws from the full linear predictor, observations, spatial and temporal random effects.

```{=html}
<!-- -->
```
3.  Run the `modelResults.R` to produce the plots, tables, and additional results seen in both the Manuscript and Supplementary Material.
    -   Must be run **after** `modelFit.R`

```{=html}
<!-- -->
```
4.  Run the `sensitivityAnalysis.R` to perform the sensitivity analysis on the definition of when the intervention started.

### Additional files

-   `Code/functions.R` a file of helpful functions.
