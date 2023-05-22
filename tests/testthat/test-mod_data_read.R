test_that("mod_data_read_server works", {
  testServer(mod_data_read_server,
             # Add here your module params
             args = list(id = "data_read_123")
             ,
             {
               ns <- session$ns
               expect_true(inherits(ns, "function"))
               expect_true(grepl(id, ns("data_read_123")))
               expect_true(grepl("test", ns("test")))

               session$setInputs(def_data = FALSE)
               session$setInputs(upload = NULL)
               session$setInputs(apply = 0)
               session$setInputs(glimpse = FALSE)

               expect_null(rv$df)
               expect_equal(rv$upload_state, "stale")
               expect_equal(rv$trig_reset, 1)

               session$setInputs(def_data = TRUE)

               expect_equal(rv$trig_reset, 2)
               expect_equal(rv$upload_state, "refresh")
               expect_true(length(rv$df) > 0)
               expect_equal(nrow(rv$df[["cadae"]]), 1934)
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
