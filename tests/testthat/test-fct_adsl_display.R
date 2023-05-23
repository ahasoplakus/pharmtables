test_that("build_adsl works with split_rows_by as NULL", {
  exp_lyt <- basic_table(
    title = "x.x: Study Subject Data",
    subtitles = c(
      "x.x.x: Demographic Characteristics",
      "Table x.x.x.x: Demographic Characteristics - Full Analysis Set"
    ),
    prov_footer = "Source: ADSL DDMMYYYY hh:mm; Listing x.xx; SDTM package: DDMMYYYY",
    show_colcounts = TRUE
  ) |>
    split_cols_by("ARM") |>
    add_overall_col("All Patients") |>
    summarize_vars(c("AGE", "RACE"),
                   .stats = c("n", "mean_sd", "se", "median", "range",
                              "quantiles", "count_fraction"),
                   .labels = c(n = "n",
                               mean_sd = "Mean, SD",
                               se = "Standard Error",
                               median = "Median",
                               range = "Min-Max",
                               quantiles = c("IQR")))

  out_lyt <- build_adsl(
    split_cols_by = "ARM",
    summ_vars = c("AGE", "RACE")
  )

  expect_identical(exp_lyt, out_lyt)
})

test_that("build_adsl works when split_rows_by is present", {
  exp_lyt <- basic_table(
    title = "x.x: Study Subject Data",
    subtitles = c(
      "x.x.x: Demographic Characteristics",
      "Table x.x.x.x: Demographic Characteristics - Full Analysis Set"
    ),
    prov_footer = "Source: ADSL DDMMYYYY hh:mm; Listing x.xx; SDTM package: DDMMYYYY",
    show_colcounts = TRUE
  ) |>
    split_cols_by("ARM") |>
    split_rows_by("SEX") |>
    add_overall_col("All Patients") |>
    summarize_vars(c("AGE", "RACE"),
                   .stats = c("n", "mean_sd", "se", "median", "range",
                              "quantiles", "count_fraction"),
                   .labels = c(n = "n",
                               mean_sd = "Mean, SD",
                               se = "Standard Error",
                               median = "Median",
                               range = "Min-Max",
                               quantiles = c("IQR")))

  out_lyt <- build_adsl(
    split_cols_by = "ARM",
    split_rows_by = "SEX",
    summ_vars = c("AGE", "RACE")
  )

  expect_identical(exp_lyt, out_lyt)
})
