data(adsl)
data(adae)

options(warn = -1)

test_that("add_adae_flags works as expected", {
  out_df <- add_adae_flags(adae)

  expect_true(ncol(out_df) > ncol(adae))
  expect_equal(
    setdiff(names(out_df), names(adae)),
    c(
      "SER", "SAEFATAL", "SAELIFE", "SAEHOSP",
      "SAEDISAB", "SAECONG", "SAEMIE", "WD", "WDSM",
      "AEINT", "AERED", "AED", "AEMIE", "CTC35", "CTC45"
    )
  )
})

test_that("build_adae_summary works", {
  adae_ <- add_adae_flags(adae)

  lyt <- build_adae_summary(
    adae = adae_,
    filter_cond = NULL,
    event_vars = setdiff(names(adae_), names(adae)),
    trt_var = "ARM"
  )

  exp_lyt <- basic_table(show_colcounts = TRUE) |>
    split_cols_by(var = "ARM", split_fun = drop_split_levels) |>
    add_overall_col(label = "All Patients") |>
    count_patients_with_flags(
      "USUBJID",
      flag_variables = var_labels(adae_[, "SER"]),
      .indent_mods = 1L,
      denom = "N_col",
      table_names = "sae"
    ) |>
    count_patients_with_flags(
      "USUBJID",
      flag_variables = var_labels(adae_[, c(
        "SAEFATAL", "SAELIFE", "SAEHOSP",
        "SAEDISAB", "SAECONG", "SAEMIE"
      )]),
      .indent_mods = 2L,
      denom = "N_col",
      table_names = "sae_fl"
    ) |>
    count_patients_with_flags(
      var = "USUBJID",
      flag_variables = var_labels(adae_[, c("WD", "WDSM")]),
      .indent_mods = 1L,
      denom = "N_col",
      table_names = "ae"
    ) |>
    count_patients_with_flags(
      var = "USUBJID",
      flag_variables = var_labels(adae_[, c("AEINT", "AERED", "AED", "AEMIE")]),
      .indent_mods = 2L,
      denom = "N_col",
      table_names = "ds"
    ) |>
    analyze_num_patients(
      vars = "USUBJID",
      .stats = "unique",
      .labels = c(unique = "Any AE"),
      .indent_mods = 1L,
      show_labels = "hidden"
    ) |>
    count_occurrences_by_grade(
      var = "AESEV",
      show_labels = "hidden",
      .indent_mods = 2L
    ) |>
    count_patients_with_flags(
      var = "USUBJID",
      flag_variables = var_labels(adae_[, c("CTC35", "CTC45")]),
      .indent_mods = 2L,
      denom = "N_col",
      table_names = "ctc"
    ) |>
    append_topleft(c("", "Adverse Events"))

  expect_identical(lyt$lyt, exp_lyt)
  expect_snapshot(build_table(lyt = lyt$lyt, df = lyt$df_out, alt_counts_df = adsl))
})

test_that("build_adae_summary works with filter condition", {
  adae <- select(adae, -c(all_of(c("AEACN", "AESEV"))))
  adae_ <- add_adae_flags(adae)

  lyt <- build_adae_summary(
    adae = adae_,
    filter_cond = filters_to_cond(list(SEX = c("F"))),
    event_vars = setdiff(names(adae_), names(adae)),
    trt_var = "ARM"
  )

  adae_1 <- adae_ |>
    filter(SEX == "F")

  exp_lyt <- basic_table(show_colcounts = TRUE) |>
    split_cols_by(var = "ARM", split_fun = drop_split_levels) |>
    add_overall_col(label = "All Patients") |>
    count_patients_with_flags(
      "USUBJID",
      flag_variables = var_labels(adae_[, "SER"]),
      .indent_mods = 1L,
      denom = "N_col",
      table_names = "sae"
    ) |>
    count_patients_with_flags(
      "USUBJID",
      flag_variables = var_labels(adae_[, c(
        "SAEFATAL", "SAELIFE", "SAEHOSP",
        "SAEDISAB", "SAECONG", "SAEMIE"
      )]),
      denom = "N_col",
      .indent_mods = 2L,
      table_names = "sae_fl"
    ) |>
    count_patients_with_flags(
      var = "USUBJID",
      flag_variables = var_labels(adae_[, c("WD", "WDSM")]),
      .indent_mods = 1L,
      denom = "N_col",
      table_names = "ae"
    ) |>
    count_patients_with_flags(
      var = "USUBJID",
      flag_variables = var_labels(adae_[, c("AEINT", "AERED", "AED", "AEMIE")]),
      .indent_mods = 2L,
      denom = "N_col",
      table_names = "ds"
    ) |>
    analyze_num_patients(
      vars = "USUBJID",
      .stats = "unique",
      .labels = c(unique = "Any AE"),
      .indent_mods = 1L,
      show_labels = "hidden"
    ) |>
    count_patients_with_flags(
      var = "USUBJID",
      flag_variables = var_labels(adae_[, c("CTC35", "CTC45")]),
      .indent_mods = 2L,
      denom = "N_col",
      table_names = "ctc"
    ) |>
    append_topleft(c("", "Adverse Events"))

  expect_identical(lyt$lyt, exp_lyt)
  expect_equal(nrow(lyt$df_out), nrow(adae_1))
  expect_snapshot(build_table(lyt = lyt$lyt, df = lyt$df_out, alt_counts_df = adsl))
})

test_that("build_adae_by_sev_tox works", {
  lyt <- build_adae_by_sev_tox(
    adae = adae,
    colsby = "ARM",
    grade_val = "AESEV",
    class_val = "AESOC",
    term_val = "AEDECOD"
  )

  out_df <- build_table(lyt = lyt$lyt, df = lyt$df_out, alt_counts_df = adsl)

  obj_clA <-
    out_df@children[["AESOC"]]@children[["cl A"]]@children[["AEDECOD"]]@children[["dcd A.1.1.1.1"]]@content@children # nolint
  val_obj_clA <- obj_clA[["MILD"]]@leaf_value

  obj_clB <-
    out_df@children[["AESOC"]]@children[["cl B"]]@children[["AEDECOD"]]@children[["dcd B.1.1.1.1"]]@content@children # nolint
  val_obj_clB <- obj_clB[["SEVERE"]]@leaf_value

  expect_equal(class(out_df)[1], "TableTree")
  expect_equal(length(obj_clA), 3)
  expect_identical(names(obj_clA), c("MILD", "MODERATE", "SEVERE"))
  expect_identical(names(val_obj_clA), c("A: Drug X", "B: Placebo", "C: Combination", "All Patients")) # nolint
  expect_identical(round(unlist(val_obj_clA[["A: Drug X"]]), 4), c(50, 0.3731))

  expect_equal(length(obj_clB), 3)
  expect_identical(names(obj_clB), c("MILD", "MODERATE", "SEVERE"))
  expect_identical(names(val_obj_clB), c("A: Drug X", "B: Placebo", "C: Combination", "All Patients")) # nolint
  expect_identical(round(unlist(val_obj_clB[["A: Drug X"]]), 4), c(47, 0.3507))

  expect_snapshot(out_df)
})

test_that("build_adae_by_sev_tox works with AETOXGR", {
  lyt <- build_adae_by_sev_tox(
    adae = adae,
    colsby = "ARM",
    grade_val = "AETOXGR",
    class_val = "AESOC",
    term_val = "AEDECOD"
  )

  out_df <- build_table(lyt = lyt$lyt, df = lyt$df_out, alt_counts_df = adsl)

  expect_equal(class(out_df)[1], "TableTree")
  expect_snapshot(out_df)

  lyt1 <- build_adae_by_sev_tox(
    adae = adae,
    colsby = "ARM",
    filter_cond = filters_to_cond(list(SEX = c("F"))),
    grade_val = "AETOXGR",
    class_val = "AESOC",
    term_val = "AEDECOD"
  )

  out_df1 <- build_table(lyt = lyt1$lyt, df = lyt1$df_out, alt_counts_df = adsl)

  expect_equal(class(out_df1)[1], "TableTree")
  expect_snapshot(out_df1)
})
