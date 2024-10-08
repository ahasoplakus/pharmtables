data(adsl)
data(adae)
data(advs)

test_that("build_adsl_chars_table works", {
  lyt <- build_adsl_chars_table(
    split_cols_by = "ARM",
    summ_vars = c("AGE", "RACE")
  )

  exp_lyt <- basic_table(
    title = "",
    show_colcounts = TRUE
  ) |>
    split_cols_by("ARM", split_fun = drop_split_levels) |>
    add_overall_col("All Patients") |>
    analyze_vars(
      c("AGE", "RACE"),
      .stats = c(
        "n",
        "mean_sd",
        "se",
        "median",
        "range",
        "quantiles",
        "count_fraction"
      ),
      .labels = c(
        n = "n",
        mean_sd = "Mean, SD",
        se = "Standard Error",
        median = "Median",
        range = "Min-Max",
        quantiles = c("IQR")
      )
    ) |>
    append_topleft(c("", "Characteristic"))

  expect_identical(lyt, exp_lyt)
})

test_that("build_generic_occurrence_table works", {
  adae <- filter(adae, SAFFL == "Y")

  lyt <- build_generic_occurrence_table(
    occ_df = adae,
    filter_cond = NULL,
    trt_var = "ARM",
    dataset = "adae",
    class_var = "AESOC",
    term_var = "AEDECOD"
  )

  exp_lyt <- basic_table() |>
    split_cols_by(var = "ARM", split_fun = drop_split_levels) |>
    add_colcounts() |>
    add_overall_col(label = "All Patients") |>
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one event",
        nonunique = "Total number of events"
      )
    ) |>
    split_rows_by(
      "AESOC",
      label_pos = "topleft",
      split_label = obj_label(adae[["AESOC"]]),
      indent_mod = 1L,
      split_fun = drop_split_levels
    ) |>
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = c(unique = NULL)
    ) |>
    count_occurrences(vars = "AEDECOD") |>
    append_topleft(paste(" ", obj_label(adae[["AEDECOD"]])))

  expect_identical(lyt$lyt, exp_lyt)

  lyt_ <- build_generic_occurrence_table(
    occ_df = adae,
    filter_cond = filters_to_cond(list(SEX = c("F"))),
    trt_var = "ARM",
    dataset = "adae",
    class_var = "AESOC",
    term_var = "AEDECOD"
  )

  adae_ <- filter(adae, SEX == "F")

  exp_lyt_ <- basic_table() |>
    split_cols_by(var = "ARM", split_fun = drop_split_levels) |>
    add_colcounts() |>
    add_overall_col(label = "All Patients") |>
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one event",
        nonunique = "Total number of events"
      )
    ) |>
    split_rows_by(
      "AESOC",
      label_pos = "topleft",
      split_label = obj_label(adae_[["AESOC"]]),
      indent_mod = 1L,
      split_fun = drop_split_levels
    ) |>
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = c(unique = NULL)
    ) |>
    count_occurrences(vars = "AEDECOD") |>
    append_topleft(paste(" ", obj_label(adae_[["AEDECOD"]])))

  expect_identical(lyt_$lyt, exp_lyt_)
  expect_equal(nrow(lyt_$df_out), nrow(adae_))
})

test_that("build_generic_bds_table works", {
  lyt <- build_generic_bds_table(
    bds_df = advs,
    filter_cond = NULL,
    param = "Diastolic Blood Pressure",
    trt_var = "ARM",
    visit = "AVISIT",
    disp_vars = c("AVAL", "CHG")
  )

  df <- advs |>
    filter(PARAM %in% "Diastolic Blood Pressure")

  var_labs <- map_chr(c("AVAL", "CHG"), \(x) obj_label(df[[x]]))

  exp_lyt <- basic_table(
    show_colcounts = TRUE
  ) |>
    split_cols_by("ARM", split_fun = drop_split_levels) |>
    split_rows_by(
      "AVISIT",
      split_fun = drop_split_levels,
      label_pos = "topleft",
      indent_mod = 1L,
      split_label = obj_label(df[["AVISIT"]])
    ) |>
    split_cols_by_multivar(
      vars = c("AVAL", "CHG"),
      varlabels = var_labs
    ) |>
    summarize_colvars(.labels = c(range = "Min - Max")) |>
    append_topleft(paste(" ", "Summary Statistic"))

  expect_identical(lyt$lyt, exp_lyt)
})

test_that("build_generic_bds_table works with filter oondition", {
  lyt <- build_generic_bds_table(
    bds_df = advs,
    filter_cond = filters_to_cond(list(SEX = c("F"))),
    param = "Diastolic Blood Pressure",
    trt_var = "ARM",
    visit = "AVISIT",
    disp_vars = c("AVAL", "CHG")
  )

  df <- advs |>
    filter(
      PARAM %in% "Diastolic Blood Pressure",
      SEX == "F"
    )

  var_labs <- map_chr(c("AVAL", "CHG"), \(x) obj_label(df[[x]]))

  exp_lyt <- basic_table(
    show_colcounts = TRUE
  ) |>
    split_cols_by("ARM", split_fun = drop_split_levels) |>
    split_rows_by(
      "AVISIT",
      split_fun = drop_split_levels,
      label_pos = "topleft",
      indent_mod = 1L,
      split_label = obj_label(df[["AVISIT"]])
    ) |>
    split_cols_by_multivar(
      vars = c("AVAL", "CHG"),
      varlabels = var_labs
    ) |>
    summarize_colvars(.labels = c(range = "Min - Max")) |>
    append_topleft(paste(" ", "Summary Statistic"))

  expect_identical(lyt$lyt, exp_lyt)
})

test_that("build_generic_bds_table works with timepoint", {
  lyt <- build_generic_bds_table(
    bds_df = advs,
    filter_cond = filters_to_cond(list(SEX = c("F"))),
    param = "Diastolic Blood Pressure",
    trt_var = "ARM",
    visit = "AVISIT",
    timepoint = "ATPTN",
    disp_vars = c("AVAL", "CHG")
  )

  df <- advs |>
    filter(
      PARAM %in% "Diastolic Blood Pressure",
      SEX == "F"
    )

  var_labs <- map_chr(c("AVAL", "CHG"), \(x) obj_label(df[[x]]))

  exp_lyt <- basic_table(
    show_colcounts = TRUE
  ) |>
    split_cols_by("ARM", split_fun = drop_split_levels) |>
    split_rows_by(
      "AVISIT",
      split_fun = drop_split_levels,
      label_pos = "topleft",
      indent_mod = 1L,
      split_label = obj_label(df[["AVISIT"]])
    ) |>
    split_rows_by(
      "ATPTN",
      split_fun = drop_split_levels,
      label_pos = "topleft",
      split_label = obj_label(df[["ATPTN"]])
    ) |>
    split_cols_by_multivar(
      vars = c("AVAL", "CHG"),
      varlabels = var_labs
    ) |>
    summarize_colvars(.labels = c(range = "Min - Max")) |>
    append_topleft(paste(" ", "Summary Statistic"))

  expect_identical(lyt$lyt, exp_lyt)
})

test_that("build_disp_table works with expected inputs", {
  lyt <- build_disp_table(
    adsl = adsl,
    trt_var = "ARM",
    eos_var = "EOSSTT",
    eot_var = "EOTSTT",
    dcs_reas = "DCSREAS",
    dct_reas = "DCTREAS"
  )

  tbl1 <- build_table(lyt = lyt$lyt[[1]], df = lyt$df)
  tbl2 <- build_table(lyt = lyt$lyt[[2]], df = lyt$df)
  rtables::col_info(tbl1) <- rtables::col_info(tbl2)

  tbl <- rbind(tbl1, tbl2)

  expect_snapshot(rbind(tbl1, tbl2))

  leaf_val <-
    tbl@children[["root"]]@children[["ma_ITT_SAFF"]]@children[["ITT"]]@children[["count_fraction"]]@leaf_value # nolint

  expect_equal(length(leaf_val), 4)
  expect_equal(leaf_val[["A: Drug X"]][[1]][[1]], 134)
  expect_equal(leaf_val[["B: Placebo"]][[1]][[1]], 134)

  set.seed(1234)
  adsl_ <- adsl |>
    mutate(
      RANDFL = sample(c("Y", "N"), 400, replace = TRUE),
      PPROTFL = sample(c("Y", "N"), 400, replace = TRUE),
      DCTREAS = DCSREAS
    ) |>
    select(-ITTFL)

  lyt_ <- build_disp_table(
    adsl = adsl_,
    trt_var = "ARM",
    eos_var = "EOSSTT",
    eot_var = "EOTSTT",
    dcs_reas = "DCSREAS",
    dct_reas = "DCTREAS"
  )

  tbl_1 <- build_table(lyt = lyt_$lyt[[1]], df = lyt_$df)
  tbl_2 <- build_table(lyt = lyt_$lyt[[2]], df = lyt_$df)
  rtables::col_info(tbl_1) <- rtables::col_info(tbl_2)

  tbl_ <- rbind(tbl_1, tbl_2)

  leaf_val_ <-
    tbl_@children[["root"]]@children[["RANDFL"]]@children[["Y"]]@children[["SAFF"]]@children[["count_fraction"]]@leaf_value # nolint
  expect_equal(length(leaf_val), 4)
  expect_equal(leaf_val[["A: Drug X"]][[1]][[1]], 134)
  expect_equal(leaf_val[["B: Placebo"]][[1]][[1]], 134)

  expect_snapshot(rbind(tbl_1, tbl_2))
})
