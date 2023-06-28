adsl <- random.cdisc.data::cadsl
adae <- random.cdisc.data::cadae

test_that("add_adae_flags works as expected", {
  out_df <- add_adae_flags(adae)

  expect_true(ncol(out_df) > ncol(adae))
  expect_equal(
    setdiff(names(out_df), names(adae)),
    c(
      "FATAL",
      "SER",
      "SERWD",
      "SERDSM",
      "RELSER",
      "WD",
      "DSM",
      "REL",
      "RELWD",
      "RELDSM",
      "CTC35",
      "CTC45"
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
    count_patients_with_event(
      vars = "USUBJID",
      filters = c("STUDYID" = as.character(unique(adae_[["STUDYID"]]))),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of patients with at least one adverse event")
    ) |>
    count_values(
      "STUDYID",
      values = as.character(unique(adae_[["STUDYID"]])),
      .stats = "count",
      .labels = c(count = "Total AEs"),
      table_names = "total_aes"
    ) |>
    count_patients_with_flags(
      "USUBJID",
      flag_variables = var_labels(adae_[, setdiff(names(adae_), names(adae))]),
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible"
    ) |>
    append_topleft(c("", "Adverse Events"))

  expect_identical(lyt$lyt, exp_lyt)
})

test_that("build_adae_summary works with filter condition", {
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
    count_patients_with_event(
      vars = "USUBJID",
      filters = c("STUDYID" = as.character(unique(adae_1[["STUDYID"]]))),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of patients with at least one adverse event")
    ) |>
    count_values(
      "STUDYID",
      values = as.character(unique(adae_1[["STUDYID"]])),
      .stats = "count",
      .labels = c(count = "Total AEs"),
      table_names = "total_aes"
    ) |>
    count_patients_with_flags(
      "USUBJID",
      flag_variables = var_labels(adae_1[, setdiff(names(adae_1), names(adae))]),
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible"
    ) |>
    append_topleft(c("", "Adverse Events"))

  expect_identical(lyt$lyt, exp_lyt)
  expect_equal(nrow(lyt$df_out), nrow(adae_1))
})

test_that("build_adae_by_sev_tox works with default view", {
  out_df <- build_adae_by_sev_tox(
    adsl = adsl,
    df_adae = adae,
    colsby = "ARM",
    grade_val = "AESEV",
    class_val = "AESOC",
    term_val = "AEDECOD",
    default_view = TRUE
  )

  obj_clA <-
    out_df@children[["cl A"]]@children[["AEDECOD"]]@children[["dcd A.1.1.1.1"]]@leaf_value
  obj_clB <-
    out_df@children[["cl B"]]@children[["AEDECOD"]]@children[["dcd B.1.1.1.1"]]@leaf_value

  expect_equal(class(out_df)[1], "TableTree")
  expect_equal(length(obj_clA), 12)
  expect_identical(round(unlist(obj_clA[[1]]), 4), c(50, 0.3731))

  expect_equal(length(obj_clB), 12)
  expect_identical(round(unlist(obj_clB[[1]]), 4), c(47, 0.3507))
})

test_that("build_adae_by_sev_tox works with alternate view", {
  out_df <- build_adae_by_sev_tox(
    adsl = adsl,
    df_adae = adae,
    colsby = "ARM",
    grade_val = "AESEV",
    class_val = "AESOC",
    term_val = "AEDECOD",
    default_view = FALSE
  )

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
})

test_that("build_adae_by_sev_tox works with AETOXGR", {
  out_df <- build_adae_by_sev_tox(
    adsl = adsl,
    df_adae = adae,
    colsby = "ARM",
    grade_val = "AETOXGR",
    class_val = "AESOC",
    term_val = "AEDECOD",
    default_view = TRUE
  )

  expect_equal(class(out_df)[1], "TableTree")

  obj_clA <-
    out_df@children[["cl A"]]@children[["AEDECOD"]]@children[["dcd A.1.1.1.1"]]@leaf_value
  obj_clB <-
    out_df@children[["cl B"]]@children[["AEDECOD"]]@children[["dcd B.1.1.1.1"]]@leaf_value

  expect_equal(length(obj_clA), 18)
  expect_equal(length(obj_clB), 18)

  expect_identical(round(unlist(obj_clA[[1]]), 4), c(50, 0.3731))
  expect_identical(round(unlist(obj_clB[[1]]), 4), c(47, 0.3507))

  out_df1 <- build_adae_by_sev_tox(
    adsl = adsl,
    df_adae = adae,
    colsby = "ARM",
    filter_cond = filters_to_cond(list(SEX = c("F"))),
    grade_val = "AETOXGR",
    class_val = "AESOC",
    term_val = "AEDECOD",
    default_view = TRUE
  )

  expect_equal(class(out_df1)[1], "TableTree")

  obj_clA_ <-
    out_df1@children[["cl A"]]@children[["AEDECOD"]]@children[["dcd A.1.1.1.1"]]@leaf_value
  obj_clB_ <-
    out_df1@children[["cl B"]]@children[["AEDECOD"]]@children[["dcd B.1.1.1.1"]]@leaf_value

  expect_identical(round(unlist(obj_clA_[[1]]), 4), c(34, 0.2537))
  expect_identical(round(unlist(obj_clB_[[1]]), 4), c(28, 0.209))
})
