test_that("filters_to_cond works", {
  filter_list <- list(
    sex = c("F", "M"),
    trtsdtm = c("2023-06-19 13:02:20", "2023-06-18 13:02:21")
  )

  exp <- filters_to_cond(filter_list)

  expect_equal(length(exp), 1)
  expect_equal(
    exp,
    "SEX %in% c('F','M') & as.character(TRTSDTM) %in% c('2023-06-19 13:02:20','2023-06-18 13:02:21')" #nolint
  )
})
