test_that("mod_data_read_server works", {
  testServer(mod_data_read_server,
             # Add here your module params
             args = list(id = "data_read_1", data_list = c("cadsl", "cadcm"))
             ,
             {
               ns <- session$ns
               expect_true(inherits(ns, "function"))
               expect_true(grepl(id, ns("data_read_1")))
               expect_true(grepl("test", ns("test")))

               expect_equal(length(read_df()), 2)
               expect_s3_class(read_df()[["cadsl"]], c("tbl_df", "tbl", "data.frame"))
               expect_s3_class(read_df()[["cadcm"]], c("tbl_df", "tbl", "data.frame"))
               expect_true(nrow(read_df()[["cadsl"]]) == 400)
               expect_equal(nrow(read_df()[["cadcm"]]), 3685)
             })
})

test_that("module ui works", {
  ui <- mod_data_read_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_data_read_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
