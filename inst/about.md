
<style>.pre, pre{background-color: #F3F4ED;}</style>
<style>.code.language-r, code.language-r{color: darkcyan !important;}</style>
<style>.h1, h1{color: #2D4356; font-weight:700;}</style>
<style>.h2, h2{color: #2D4356; font-weight:600;}</style>

<div style="display:flex; justify-content: center;">
<img src="figures/logo.png" align="center" width="250" height="275"/>
</div>

## Introduction

**{pharmtables}** is a Shiny Application wrapped as an R package to visualize some common safety tables used in Clinical Analysis and Reporting. The application is developed using the <a href="https://thinkr-open.github.io/golem/" target="_blank">Golem</a> framework.

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

- Demographic and Clinical Characteristics
- Patient Disposition
- Overview of Adverse Events
- Summary of Adverse Events by System Organ Class and Preferred Term
- Summary of Adverse Events by System Organ Class and Preferred Term and Severity
- Summary of Concomitant Medications by Medication Class and Standardized Medication Name
- Summary of Medical History by Body System or Organ Class and Dictionary-Derived Term
- Summary of Vital Signs, Laboratory and ECG Tests by Parameter, Analysis Value and Visit
- Shift at post dose for Vital Signs, Laboratory and ECG Tests

