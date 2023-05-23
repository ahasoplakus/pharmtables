Tests and Coverage
================
23 May, 2023 15:27:09

-   [Coverage](#coverage)
-   [Unit Tests](#unit-tests)

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                                | Coverage (%) |
|:------------------------------------------------------|:------------:|
| clinTables                                            |    98.69     |
| [R/app\_server.R](../R/app_server.R)                  |    94.74     |
| [R/mod\_data\_read.R](../R/mod_data_read.R)           |    96.35     |
| [R/utils\_helpers.R](../R/utils_helpers.R)            |    97.47     |
| [R/adae\_by\_sev\_tox.R](../R/adae_by_sev_tox.R)      |    97.87     |
| [R/mod\_adxx\_bodsys.R](../R/mod_adxx_bodsys.R)       |    98.59     |
| [R/app\_config.R](../R/app_config.R)                  |    100.00    |
| [R/app\_ui.R](../R/app_ui.R)                          |    100.00    |
| [R/fct\_adsl\_display.R](../R/fct_adsl_display.R)     |    100.00    |
| [R/mod\_adae\_global.R](../R/mod_adae_global.R)       |    100.00    |
| [R/mod\_adae\_sev\_tox.R](../R/mod_adae_sev_tox.R)    |    100.00    |
| [R/mod\_adae\_summary.R](../R/mod_adae_summary.R)     |    100.00    |
| [R/mod\_adsl\_display.R](../R/mod_adsl_display.R)     |    100.00    |
| [R/mod\_dt\_table.R](../R/mod_dt_table.R)             |    100.00    |
| [R/mod\_global\_filters.R](../R/mod_global_filters.R) |    100.00    |
| [R/mod\_process\_adsl.R](../R/mod_process_adsl.R)     |    100.00    |
| [R/run\_app.R](../R/run_app.R)                        |    100.00    |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file                                                              |   n | time | error | failed | skipped | warning | icon |
|:------------------------------------------------------------------|----:|-----:|------:|-------:|--------:|--------:|:-----|
| [test-adae\_by\_sev\_tox.R](testthat/test-adae_by_sev_tox.R)      |  14 | 1.55 |     0 |      0 |       0 |       0 |      |
| [test-fct\_adsl\_display.R](testthat/test-fct_adsl_display.R)     |   2 | 0.28 |     0 |      0 |       0 |       0 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R)     |  10 | 0.10 |     0 |      0 |       1 |       0 | \+   |
| [test-mod\_adae\_global.R](testthat/test-mod_adae_global.R)       |   2 | 0.04 |     0 |      0 |       0 |       0 |      |
| [test-mod\_adae\_sev\_tox.R](testthat/test-mod_adae_sev_tox.R)    |   8 | 3.86 |     0 |      0 |       0 |       0 |      |
| [test-mod\_adae\_summary.R](testthat/test-mod_adae_summary.R)     |   9 | 0.48 |     0 |      0 |       0 |       0 |      |
| [test-mod\_adsl\_display.R](testthat/test-mod_adsl_display.R)     |  10 | 1.44 |     0 |      0 |       0 |       0 |      |
| [test-mod\_adxx\_bodsys.R](testthat/test-mod_adxx_bodsys.R)       |   8 | 0.96 |     0 |      0 |       0 |       0 |      |
| [test-mod\_data\_read.R](testthat/test-mod_data_read.R)           |  18 | 1.47 |     0 |      0 |       0 |       0 |      |
| [test-mod\_dt\_table.R](testthat/test-mod_dt_table.R)             |   2 | 0.01 |     0 |      0 |       0 |       0 |      |
| [test-mod\_global\_filters.R](testthat/test-mod_global_filters.R) |  15 | 0.30 |     0 |      0 |       0 |       0 |      |
| [test-mod\_process\_adsl.R](testthat/test-mod_process_adsl.R)     |  16 | 0.12 |     0 |      0 |       0 |       0 |      |
| [test-utils\_helpers.R](testthat/test-utils_helpers.R)            |   1 | 0.02 |     0 |      0 |       0 |       0 |      |

<details open>
<summary>
Show Detailed Test Results
</summary>

| file                                                                     | context              | test                                              | status  |   n | time | icon |
|:-------------------------------------------------------------------------|:---------------------|:--------------------------------------------------|:--------|----:|-----:|:-----|
| [test-adae\_by\_sev\_tox.R](testthat/test-adae_by_sev_tox.R#L20)         | adae\_by\_sev\_tox   | adae\_by\_sev\_tox works with default view        | PASS    |   5 | 0.94 |      |
| [test-adae\_by\_sev\_tox.R](testthat/test-adae_by_sev_tox.R#L47)         | adae\_by\_sev\_tox   | adae\_by\_sev\_tox works with alternate view      | PASS    |   9 | 0.61 |      |
| [test-fct\_adsl\_display.R](testthat/test-fct_adsl_display.R#L28)        | fct\_adsl\_display   | build\_adsl works with split\_rows\_by as NULL    | PASS    |   1 | 0.07 |      |
| [test-fct\_adsl\_display.R](testthat/test-fct_adsl_display.R#L60)        | fct\_adsl\_display   | build\_adsl works when split\_rows\_by is present | PASS    |   1 | 0.21 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L3)         | golem-recommended    | app ui                                            | PASS    |   2 | 0.08 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L13)        | golem-recommended    | app server                                        | PASS    |   4 | 0.01 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L24_L26)    | golem-recommended    | app\_sys works                                    | PASS    |   1 | 0.00 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L36_L42)    | golem-recommended    | golem-config works                                | PASS    |   2 | 0.00 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L68)        | golem-recommended    | app launches                                      | SKIPPED |   1 | 0.01 | \+   |
| [test-mod\_adae\_global.R](testthat/test-mod_adae_global.R#L20)          | mod\_adae\_global    | module ui works                                   | PASS    |   2 | 0.04 |      |
| [test-mod\_adae\_sev\_tox.R](testthat/test-mod_adae_sev_tox.R#L2_L58)    | mod\_adae\_sev\_tox  | mod\_adae\_sev\_tox\_server works                 | PASS    |   6 | 3.84 |      |
| [test-mod\_adae\_sev\_tox.R](testthat/test-mod_adae_sev_tox.R#L63)       | mod\_adae\_sev\_tox  | module ui works                                   | PASS    |   2 | 0.02 |      |
| [test-mod\_adae\_summary.R](testthat/test-mod_adae_summary.R#L2_L83)     | mod\_adae\_summary   | mod\_adae\_summary\_server works                  | PASS    |   7 | 0.48 |      |
| [test-mod\_adae\_summary.R](testthat/test-mod_adae_summary.R#L89)        | mod\_adae\_summary   | module ui works                                   | PASS    |   2 | 0.00 |      |
| [test-mod\_adsl\_display.R](testthat/test-mod_adsl_display.R#L2_L44)     | mod\_adsl\_display   | mod\_adsl\_display\_server works                  | PASS    |   8 | 1.42 |      |
| [test-mod\_adsl\_display.R](testthat/test-mod_adsl_display.R#L49)        | mod\_adsl\_display   | module ui works                                   | PASS    |   2 | 0.02 |      |
| [test-mod\_adxx\_bodsys.R](testthat/test-mod_adxx_bodsys.R#L2_L49)       | mod\_adxx\_bodsys    | mod\_adxx\_bodsys\_server works                   | PASS    |   6 | 0.94 |      |
| [test-mod\_adxx\_bodsys.R](testthat/test-mod_adxx_bodsys.R#L54)          | mod\_adxx\_bodsys    | module ui works                                   | PASS    |   2 | 0.02 |      |
| [test-mod\_data\_read.R](testthat/test-mod_data_read.R#L2_L39)           | mod\_data\_read      | mod\_data\_read\_server works                     | PASS    |  16 | 1.45 |      |
| [test-mod\_data\_read.R](testthat/test-mod_data_read.R#L44)              | mod\_data\_read      | module ui works                                   | PASS    |   2 | 0.02 |      |
| [test-mod\_dt\_table.R](testthat/test-mod_dt_table.R#L20)                | mod\_dt\_table       | module ui works                                   | PASS    |   2 | 0.01 |      |
| [test-mod\_global\_filters.R](testthat/test-mod_global_filters.R#L2_L62) | mod\_global\_filters | mod\_global\_filters\_server works                | PASS    |  13 | 0.28 |      |
| [test-mod\_global\_filters.R](testthat/test-mod_global_filters.R#L67)    | mod\_global\_filters | module ui works                                   | PASS    |   2 | 0.02 |      |
| [test-mod\_process\_adsl.R](testthat/test-mod_process_adsl.R#L4_L69)     | mod\_process\_adsl   | mod\_process\_adsl\_server works                  | PASS    |  14 | 0.11 |      |
| [test-mod\_process\_adsl.R](testthat/test-mod_process_adsl.R#L74)        | mod\_process\_adsl   | module ui works                                   | PASS    |   2 | 0.01 |      |
| [test-utils\_helpers.R](testthat/test-utils_helpers.R#L2)                | utils\_helpers       | multiplication works                              | PASS    |   1 | 0.02 |      |

| Failed | Warning | Skipped |
|:-------|:--------|:--------|
| !      | \-      | \+      |

</details>
<details>
<summary>
Session Info
</summary>

| Field    | Value                            |
|:---------|:---------------------------------|
| Version  | R version 4.1.0 (2021-05-18)     |
| Platform | x86\_64-w64-mingw32/x64 (64-bit) |
| Running  | Windows 10 x64 (build 19044)     |
| Language | English\_United States           |
| Timezone | Asia/Calcutta                    |

| Package  | Version |
|:---------|:--------|
| testthat | 3.1.8   |
| covr     | 3.6.2   |
| covrpage | 0.2     |

</details>
<!--- Final Status : skipped/warning --->
