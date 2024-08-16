
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

`pharmtables` is a shiny application to visualize standard safety tables
used in <b>Clinical Trials</b>

## Installation

Latest `dev` version

``` r
remotes::install_github("ahasoplakus/pharmtables", ref = "devel", build_vignettes = TRUE)
```

<left> Find the app deployed
<a href="https://sukalpo94.shinyapps.io/clinSafety/" target="_blank">here</a>
</left>

Run `library(pharmtables)` to access all the exported functions from
`pharmtables` that help in reproducing analysis performed in the app. Or,
you can run the application locally using:

``` r
# Launch the application
run_app()
```
