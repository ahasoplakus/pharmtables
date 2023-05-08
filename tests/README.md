Tests and Coverage
================
08 May, 2023 17:34:00

-   [Coverage](#coverage)
-   [Unit Tests](#unit-tests)

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                                | Coverage (%) |
|:------------------------------------------------------|:------------:|
| clinTables                                            |    88.06     |
| [R/run\_app.R](../R/run_app.R)                        |     0.00     |
| [R/fct\_adsl\_display.R](../R/fct_adsl_display.R)     |    54.05     |
| [R/mod\_adae\_display.R](../R/mod_adae_display.R)     |    62.50     |
| [R/golem\_utils\_server.R](../R/golem_utils_server.R) |    77.78     |
| [R/golem\_utils\_ui.R](../R/golem_utils_ui.R)         |    87.94     |
| [R/utils\_helpers.R](../R/utils_helpers.R)            |    97.33     |
| [R/app\_config.R](../R/app_config.R)                  |    100.00    |
| [R/app\_server.R](../R/app_server.R)                  |    100.00    |
| [R/app\_ui.R](../R/app_ui.R)                          |    100.00    |
| [R/mod\_adae\_summary.R](../R/mod_adae_summary.R)     |    100.00    |
| [R/mod\_adsl\_display.R](../R/mod_adsl_display.R)     |    100.00    |
| [R/mod\_data\_read.R](../R/mod_data_read.R)           |    100.00    |
| [R/mod\_dt\_table.R](../R/mod_dt_table.R)             |    100.00    |
| [R/mod\_global\_filters.R](../R/mod_global_filters.R) |    100.00    |
| [R/mod\_process\_adsl.R](../R/mod_process_adsl.R)     |    100.00    |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file                                                              |   n | time | error | failed | skipped | warning | icon |
|:------------------------------------------------------------------|----:|-----:|------:|-------:|--------:|--------:|:-----|
| [test-golem-recommended.R](testthat/test-golem-recommended.R)     |  10 | 0.09 |     0 |      0 |       1 |       0 | \+   |
| [test-golem\_utils\_server.R](testthat/test-golem_utils_server.R) |  13 | 0.08 |     0 |      0 |       0 |       0 |      |
| [test-golem\_utils\_ui.R](testthat/test-golem_utils_ui.R)         |  51 | 0.27 |     0 |      0 |       0 |       0 |      |
| [test-mod\_adae\_display.R](testthat/test-mod_adae_display.R)     |   2 | 0.01 |     0 |      0 |       0 |       0 |      |
| [test-mod\_adae\_summary.R](testthat/test-mod_adae_summary.R)     |   8 | 0.31 |     0 |      0 |       0 |       0 |      |
| [test-mod\_adsl\_display.R](testthat/test-mod_adsl_display.R)     |  10 | 1.16 |     0 |      0 |       0 |       0 |      |
| [test-mod\_data\_read.R](testthat/test-mod_data_read.R)           |  10 | 0.08 |     0 |      0 |       0 |       0 |      |
| [test-mod\_dt\_table.R](testthat/test-mod_dt_table.R)             |   2 | 0.00 |     0 |      0 |       0 |       0 |      |
| [test-mod\_global\_filters.R](testthat/test-mod_global_filters.R) |  12 | 0.10 |     0 |      0 |       0 |       0 |      |
| [test-mod\_process\_adsl.R](testthat/test-mod_process_adsl.R)     |  16 | 0.13 |     0 |      0 |       0 |       0 |      |
| [test-utils\_helpers.R](testthat/test-utils_helpers.R)            |   1 | 0.00 |     0 |      0 |       0 |       0 |      |

<details open>
<summary>
Show Detailed Test Results
</summary>

| file                                                                      | context              | test                               | status  |   n | time | icon |
|:--------------------------------------------------------------------------|:---------------------|:-----------------------------------|:--------|----:|-----:|:-----|
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L3)          | golem-recommended    | app ui                             | PASS    |   2 | 0.07 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L13)         | golem-recommended    | app server                         | PASS    |   4 | 0.01 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L24_L26)     | golem-recommended    | app\_sys works                     | PASS    |   1 | 0.00 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L36_L42)     | golem-recommended    | golem-config works                 | PASS    |   2 | 0.00 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L67)         | golem-recommended    | app launches                       | SKIPPED |   1 | 0.01 | \+   |
| [test-golem\_utils\_server.R](testthat/test-golem_utils_server.R#L2)      | golem\_utils\_server | not\_in works                      | PASS    |   2 | 0.00 |      |
| [test-golem\_utils\_server.R](testthat/test-golem_utils_server.R#L7)      | golem\_utils\_server | not\_null works                    | PASS    |   2 | 0.01 |      |
| [test-golem\_utils\_server.R](testthat/test-golem_utils_server.R#L12)     | golem\_utils\_server | not\_na works                      | PASS    |   2 | 0.02 |      |
| [test-golem\_utils\_server.R](testthat/test-golem_utils_server.R#L17_L22) | golem\_utils\_server | drop\_nulls works                  | PASS    |   1 | 0.00 |      |
| [test-golem\_utils\_server.R](testthat/test-golem_utils_server.R#L26_L29) | golem\_utils\_server | %\|\|% works                       | PASS    |   2 | 0.02 |      |
| [test-golem\_utils\_server.R](testthat/test-golem_utils_server.R#L37_L40) | golem\_utils\_server | %\|NA\|% works                     | PASS    |   2 | 0.01 |      |
| [test-golem\_utils\_server.R](testthat/test-golem_utils_server.R#L48_L50) | golem\_utils\_server | rv and rvtl work                   | PASS    |   2 | 0.02 |      |
| [test-golem\_utils\_ui.R](testthat/test-golem_utils_ui.R#L2)              | golem\_utils\_ui     | Test with\_red\_star works         | PASS    |   2 | 0.01 |      |
| [test-golem\_utils\_ui.R](testthat/test-golem_utils_ui.R#L10)             | golem\_utils\_ui     | Test list\_to\_li works            | PASS    |   3 | 0.02 |      |
| [test-golem\_utils\_ui.R](testthat/test-golem_utils_ui.R#L22_L28)         | golem\_utils\_ui     | Test list\_to\_p works             | PASS    |   3 | 0.03 |      |
| [test-golem\_utils\_ui.R](testthat/test-golem_utils_ui.R#L53)             | golem\_utils\_ui     | Test named\_to\_li works           | PASS    |   3 | 0.01 |      |
| [test-golem\_utils\_ui.R](testthat/test-golem_utils_ui.R#L66)             | golem\_utils\_ui     | Test tagRemoveAttributes works     | PASS    |   4 | 0.02 |      |
| [test-golem\_utils\_ui.R](testthat/test-golem_utils_ui.R#L82)             | golem\_utils\_ui     | Test undisplay works               | PASS    |   8 | 0.03 |      |
| [test-golem\_utils\_ui.R](testthat/test-golem_utils_ui.R#L110)            | golem\_utils\_ui     | Test display works                 | PASS    |   4 | 0.03 |      |
| [test-golem\_utils\_ui.R](testthat/test-golem_utils_ui.R#L124)            | golem\_utils\_ui     | Test jq\_hide works                | PASS    |   2 | 0.02 |      |
| [test-golem\_utils\_ui.R](testthat/test-golem_utils_ui.R#L132)            | golem\_utils\_ui     | Test rep\_br works                 | PASS    |   2 | 0.01 |      |
| [test-golem\_utils\_ui.R](testthat/test-golem_utils_ui.R#L140)            | golem\_utils\_ui     | Test enurl works                   | PASS    |   2 | 0.02 |      |
| [test-golem\_utils\_ui.R](testthat/test-golem_utils_ui.R#L148)            | golem\_utils\_ui     | Test columns wrappers works        | PASS    |  16 | 0.07 |      |
| [test-golem\_utils\_ui.R](testthat/test-golem_utils_ui.R#L172)            | golem\_utils\_ui     | Test make\_action\_button works    | PASS    |   2 | 0.00 |      |
| [test-mod\_adae\_display.R](testthat/test-mod_adae_display.R#L20)         | mod\_adae\_display   | module ui works                    | PASS    |   2 | 0.01 |      |
| [test-mod\_adae\_summary.R](testthat/test-mod_adae_summary.R#L2_L64)      | mod\_adae\_summary   | mod\_adae\_summary\_server works   | PASS    |   6 | 0.30 |      |
| [test-mod\_adae\_summary.R](testthat/test-mod_adae_summary.R#L70)         | mod\_adae\_summary   | module ui works                    | PASS    |   2 | 0.01 |      |
| [test-mod\_adsl\_display.R](testthat/test-mod_adsl_display.R#L2_L38)      | mod\_adsl\_display   | mod\_adsl\_display\_server works   | PASS    |   8 | 1.13 |      |
| [test-mod\_adsl\_display.R](testthat/test-mod_adsl_display.R#L43)         | mod\_adsl\_display   | module ui works                    | PASS    |   2 | 0.03 |      |
| [test-mod\_data\_read.R](testthat/test-mod_data_read.R#L2_L17)            | mod\_data\_read      | mod\_data\_read\_server works      | PASS    |   8 | 0.06 |      |
| [test-mod\_data\_read.R](testthat/test-mod_data_read.R#L22)               | mod\_data\_read      | module ui works                    | PASS    |   2 | 0.02 |      |
| [test-mod\_dt\_table.R](testthat/test-mod_dt_table.R#L20)                 | mod\_dt\_table       | module ui works                    | PASS    |   2 | 0.00 |      |
| [test-mod\_global\_filters.R](testthat/test-mod_global_filters.R#L2_L57)  | mod\_global\_filters | mod\_global\_filters\_server works | PASS    |  10 | 0.08 |      |
| [test-mod\_global\_filters.R](testthat/test-mod_global_filters.R#L62)     | mod\_global\_filters | module ui works                    | PASS    |   2 | 0.02 |      |
| [test-mod\_process\_adsl.R](testthat/test-mod_process_adsl.R#L4_L69)      | mod\_process\_adsl   | mod\_process\_adsl\_server works   | PASS    |  14 | 0.11 |      |
| [test-mod\_process\_adsl.R](testthat/test-mod_process_adsl.R#L74)         | mod\_process\_adsl   | module ui works                    | PASS    |   2 | 0.02 |      |
| [test-utils\_helpers.R](testthat/test-utils_helpers.R#L2)                 | utils\_helpers       | multiplication works               | PASS    |   1 | 0.00 |      |

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
| testthat | 3.1.7   |
| covr     | 3.6.2   |
| covrpage | 0.2     |

</details>
<!--- Final Status : skipped/warning --->
