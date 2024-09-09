
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pharmtables <img src="man/figures/logo.png" align="right" width="200" style="margin-left:50px;"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/ahasoplakus/pharmtables/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ahasoplakus/pharmtables/actions/workflows/R-CMD-check.yaml)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![codecov](https://codecov.io/gh/ahasoplakus/pharmtables/branch/devel/graph/badge.svg?token=G5URJVIVQM)](https://app.codecov.io/gh/ahasoplakus/pharmtables)
[![Style](https://github.com/ahasoplakus/pharmtables/actions/workflows/styler.yaml/badge.svg)](https://github.com/ahasoplakus/pharmtables/actions/workflows/styler.yaml)
[![lint](https://github.com/ahasoplakus/pharmtables/actions/workflows/lint.yaml/badge.svg)](https://github.com/ahasoplakus/pharmtables/actions/workflows/lint.yaml)
[![riskmetric](https://img.shields.io/badge/riskmetric-0.43-olive)](https://pharmar.github.io/riskmetric/)
<!--![GitHub commit
activity](https://img.shields.io/github/commit-activity/m/ahasoplakus/pharmtables) -->
![GitHub last
commit](https://img.shields.io/github/last-commit/ahasoplakus/pharmtables)
![GitHub pull
requests](https://img.shields.io/github/issues-pr/ahasoplakus/pharmtables)
![GitHub repo
size](https://img.shields.io/github/repo-size/ahasoplakus/pharmtables)
![GitHub language
count](https://img.shields.io/github/languages/count/ahasoplakus/pharmtables)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current
Version](https://img.shields.io/github/r-package/v/ahasoplakus/pharmtables/main?color=purple&label=Development%20Version)](https://github.com/ahasoplakus/pharmtables/tree/main)
<!-- badges: end -->

## Introduction

**{pharmtables}** is a Shiny Application wrapped as an R package to
visualize some common safety tables used in Clinical Analysis and
Reporting. The application is developed using the
<a href="https://thinkr-open.github.io/golem/" target="_blank">Golem</a>
framework.

## Installation

The development version of the package can be installed using

``` r
remotes::install_github("ahasoplakus/pharmtables", ref = "devel", build_vignettes = TRUE)
```

After installation, the app can be launched locally using

``` r
library(pharmtables)
run_app()
```

List of tables that can be generated using {pharmtables} are

-   Demographic and Clinical Characteristics
-   Patient Disposition
-   Overview of Adverse Events
-   Summary of Adverse Events by System Organ Class and Preferred Term
-   Summary of Adverse Events by System Organ Class and Preferred Term
    and Severity
-   Summary of Concomitant Medications by Medication Class and
    Standardized Medication Name
-   Summary of Medical History by Body System or Organ Class and
    Dictionary-Derived Term
-   Summary of Vital Signs, Laboratory and ECG Tests by Parameter,
    Analysis Value and Visit
-   Shift at post dose for Vital Signs, Laboratory and ECG Tests

<left> Find the app deployed in
<a href="https://sukalpo94.shinyapps.io/pharmtables/" target="_blank">shinyapps.io</a>
and
<a href="https://connect.posit.cloud/ahasoplakus/content/01919355-79eb-347f-660e-0798480b0230" taget="_blank">Posit
Cloud Connect</a> </left>
