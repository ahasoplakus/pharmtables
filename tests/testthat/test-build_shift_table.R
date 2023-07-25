data("adsl")
data("adlb")

test_that("build_shift_table works with expected inputs", {
  out <- build_shift_table(
    adsl = adsl,
    bds_df = adlb,
    filter_cond = NULL,
    trt_var = "ARM",
    trt_label = c(ARM = "Description of Planned Arm"),
    group_var = "AVISIT",
    group_label = c(AVISIT = "Analysis Visit")
  )

  expect_true(nrow(out[["body"]][["dataset"]]) > 0)
  expect_equal(nrow(out[["body"]][["dataset"]]), 252)
  expect_equal(names(out[["body"]][["dataset"]])[1:4], c("PARAM", "ARM", "AVISIT", "BNRIND"))
  expect_equal(unique(out[["body"]][["dataset"]][["BNRIND"]]), c("HIGH", "LOW", "NORMAL", "Total"))
})

test_that("build_shift_table works with filter condtions", {
  out <- build_shift_table(
    adsl = adsl,
    bds_df = adlb,
    filter_cond = filters_to_cond(list(SEX = c("F"))),
    trt_var = "ARM",
    trt_label = c(ARM = "Description of Planned Arm"),
    group_var = "AVISIT",
    group_label = c(AVISIT = "Analysis Visit")
  )

  expect_true(nrow(out[["body"]][["dataset"]]) > 0)
  expect_equal(nrow(out[["body"]][["dataset"]]), 252)
  expect_equal(names(out[["body"]][["dataset"]])[1:4], c("PARAM", "ARM", "AVISIT", "BNRIND"))
  expect_equal(unique(out[["body"]][["dataset"]][["BNRIND"]]), c("HIGH", "LOW", "NORMAL", "Total"))

  out1 <- build_shift_table(
    adsl = adsl,
    bds_df = adlb,
    filter_cond = filters_to_cond(list(SEX = c("ABC"))),
    trt_var = "ARM",
    trt_label = c(ARM = "Description of Planned Arm"),
    group_var = "AVISIT",
    group_label = c(AVISIT = "Analysis Visit")
  )

  expect_equal(nrow(out1), 0)
  expect_equal(ncol(out1), 0)
})

test_that("build_shift_table works with alternate view", {
  out <- build_shift_table(
    adsl = adsl,
    bds_df = adlb,
    filter_cond = NULL,
    trt_var = "ARM",
    trt_label = c(ARM = "Description of Planned Arm"),
    group_var = "AVISIT",
    group_label = c(AVISIT = "Analysis Visit"),
    default_view = FALSE
  )

  expect_true(nrow(out[["body"]][["dataset"]]) > 0)
  expect_equal(nrow(out[["body"]][["dataset"]]), 84)
  expect_equal(names(out[["body"]][["dataset"]])[1:3], c("PARAM", "AVISIT", "BNRIND"))
  expect_equal(unique(out[["body"]][["dataset"]][["BNRIND"]]), c("HIGH", "LOW", "NORMAL", "Total"))
})
