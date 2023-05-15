adsl <- random.cdisc.data::cadsl
adae <- random.cdisc.data::cadae

test_that("adae_by_sev_tox works with default view", {
  out_df <- adae_by_sev_tox(
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

test_that("adae_by_sev_tox works with alternate view", {
  out_df <- adae_by_sev_tox(
    adsl = adsl,
    df_adae = adae,
    colsby = "ARM",
    grade_val = "AESEV",
    class_val = "AESOC",
    term_val = "AEDECOD",
    default_view = FALSE
  )

  obj_clA <-
    out_df@children[["AESOC"]]@children[["cl A"]]@children[["AEDECOD"]]@children[["dcd A.1.1.1.1"]]@content@children
  val_obj_clA <- obj_clA[["MILD"]]@leaf_value

  obj_clB <-
    out_df@children[["AESOC"]]@children[["cl B"]]@children[["AEDECOD"]]@children[["dcd B.1.1.1.1"]]@content@children
  val_obj_clB <- obj_clB[["SEVERE"]]@leaf_value

  expect_equal(class(out_df)[1], "TableTree")
  expect_equal(length(obj_clA), 3)
  expect_identical(names(obj_clA), c("MILD", "MODERATE", "SEVERE"))
  expect_identical(names(val_obj_clA), c("A: Drug X", "B: Placebo", "C: Combination", "All Patients"))
  expect_identical(round(unlist(val_obj_clA[["A: Drug X"]]), 4), c(50, 0.3731))

  expect_equal(length(obj_clB), 3)
  expect_identical(names(obj_clB), c("MILD", "MODERATE", "SEVERE"))
  expect_identical(names(val_obj_clB), c("A: Drug X", "B: Placebo", "C: Combination", "All Patients"))
  expect_identical(round(unlist(val_obj_clB[["A: Drug X"]]), 4), c(47, 0.3507))
})
