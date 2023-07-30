
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clinTables <img src="figures/logo.png" align="right" width="200" style="margin-left:50px;"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/ahasoplakus/clinTables/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ahasoplakus/clinTables/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/ahasoplakus/clinTables/branch/devel/graph/badge.svg?token=G5URJVIVQM)](https://app.codecov.io/gh/ahasoplakus/clinTables)
[![Style](https://github.com/ahasoplakus/clinTables/actions/workflows/styler.yaml/badge.svg)](https://github.com/ahasoplakus/clinTables/actions/workflows/styler.yaml)
[![lint](https://github.com/ahasoplakus/clinTables/actions/workflows/lint.yaml/badge.svg)](https://github.com/ahasoplakus/clinTables/actions/workflows/lint.yaml)
![GitHub commit
activity](https://img.shields.io/github/commit-activity/m/ahasoplakus/clinTables)
![GitHub last
commit](https://img.shields.io/github/last-commit/ahasoplakus/clinTables)
![GitHub pull
requests](https://img.shields.io/github/issues-pr/ahasoplakus/clinTables)
![GitHub repo
size](https://img.shields.io/github/repo-size/ahasoplakus/clinTables)
![GitHub language
count](https://img.shields.io/github/languages/count/ahasoplakus/clinTables)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current
Version](https://img.shields.io/github/r-package/v/ahasoplakus/clinTables/main?color=purple&label=Development%20Version)](https://github.com/ahasoplakus/clinTables/tree/main)
<!-- badges: end -->

# Introduction

**clinTables** is a **R Shiny Application** to visualize standard tables used in <b>Clinical Trials</b>.\
It is developed using the <a href="https://thinkr-open.github.io/golem/" target="_blank">Golem</a>
framework which creates Shiny Applications as R packages, so **clinTables** can be used (installed)
as an R package as well.

## Installation

Install the latest `dev` version `remotes::install_github("ahasoplakus/clinTables")`

After installation, install these packages with `install.packages(c("logger", "haven", "reactable", "markdown", "shinycssloaders", "flextable", "officer"))`
<!---
This application is intended for a pilot submission to the FDA composing of a Shiny application, as part of the
<a href="https://rconsortium.github.io/submissions-wg/" target="_blank">R Submissions Working Group</a>
Pilot&nbsp;2.
The data sets and results displayed in the application originate from the
<a href="https://rconsortium.github.io/submissions-wg/pilot-overall.html#pilot-1---common-analyses" target="_blank">Pilot&nbsp;1 project</a>.

Visit the **Usage Guide** for information on using the application.
Below is a brief description of the reasons behind the conversion to a Rhino application and the application components.

### Source: R Consortium

This R/Shiny application was initially developed using the
<a href="https://thinkr-open.github.io/golem/" target="_blank">Golem</a>
framework by the R Consortium submission working group.

Among the participants are Ning Leng _(Roche)_, Heng Wang _(Roche)_, Mike Stakehouse _(Atorus)_, Eli Miller _(Atorus)_, Yilong Zhang _(Merck)_, Gregery Chen _(Merck)_ and Eric Nantz _(Eli Lilly)_. Paul Schuette and Hye Soo Cho from FDA are also participating in this working group.

The full list is publicly available at the
<a href="https://rconsortium.github.io/submissions-wg/pilot2.html" target="_blank">Pilot&nbsp;2 page</a>.

The source code for the original Pilot application is publicly available at the
<a href="https://github.com/RConsortium/submissions-pilot2" target="_blank">RConsortium/submissions-pilot2</a>
Github repository.

### <a href="https://appsilon.github.io/rhino/" target="_blank"><img src="https://appsilon.github.io/rhino/reference/figures/rhino.png" alt="Rhino logo" style="height: 1.2em;"></a> Rhino Application

A team at
<a href="https://appsilon.com" target="_blank">Appsilon</a>
adapted the pilot Shiny application to use the Rhino framework bringing enterprise features to this application.

The source code is available at the
<a href="https://github.com/Appsilon/rhino-fda-pilot-2-submission/" target="_blank">Appsion/rhino-fda-pilot-2-submission</a>
Github repository. It hosts the source code and an overview of the documentation and procedure used to adapt the original pilot application.

By using the Rhino framework we have added to this pilot:

1. **Clear code structure**: scalable app architecture, modularization based on Box and full support for {Teal}.
2. **Code quality**: unit test for custom logic,
<a href="https://en.wikipedia.org/wiki/Lint_%28software%29" target="_blank">linting</a>.
3. **Interface testing**: end-to-end tests using Cypress2 framework.
4. **Automation**: continuous integration and automated test using Github Actions, package and version management with {Renv}.
5. **Customization**: adds custom styles on top of the {Teal} framework.

### Demographic Table

In this interface, summary statistics associated with baseline clinical characteristics and other demographic factors is shown.

### KM-Plot for TTDE

A Kaplan-Meier (KM) plot of the Time to First Dermatologic Event (TTDE) with strata defined by treatment group is displayed along with an informative risk set table across time.

### Primary Table

A summary table of the primary efficacy analysis is shown for each of the time points of assessment (baseline and week 24) comparing each treatment group. The primary efficacy variable (change from baseline in ADAS Cog (11)) was analyzed using an Analysis of Covariance (ANCOVA) model with treatment and baseline value as covariates, comparing Placebo to Xanomeline High Dose.

### Efficacy Table

A summary table of an additional efficacy analysis is shown for baseline and week 20. The efficacy variable (Glucose) was analzying using ANCOVA model with treatment and baseline value as covariates, comparing Placebo to Xanomeline High Dose.

### Visit Completion

A summary table of the number of patients remaining in the treatment period for each scheduled visit from baseline to week 24.

### 
-->
