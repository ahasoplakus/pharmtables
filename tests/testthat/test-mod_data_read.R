test_that("mod_data_read_server works", {
  testServer(mod_data_read_server,
    # Add here your module params
    args = list(id = "data_read_123"),
    {
      ns <- session$ns
      expect_true(inherits(ns, "function"))
      expect_true(grepl(id, ns("data_read_123")))
      expect_true(grepl("test", ns("test")))

      df <- list(
        name = "adsl.RDS",
        datapath = app_sys("extdata/adsl.RDS")
      )

      session$setInputs(def_data = FALSE)
      session$setInputs(upload = df)
      session$setInputs(apply = 1)

      expect_equal(length(rv$df), 1)
      expect_equal(nrow(rv$df[["adsl"]]), 400)
      expect_equal(rv$upload_state, "init")
      expect_equal(rv$trig_reset, 1)
      expect_equal(nrow(read_df()[["adsl"]]), 400)

      session$setInputs(def_data = TRUE)
      expect_false(is.null(read_df()))

      session$setInputs(apply = 2)
      expect_true(length(read_df()) > 0)
      expect_equal(rv$trig_reset, 1)
      expect_equal(nrow(read_df()[["adsl"]]), 400)
      expect_equal(nrow(read_df()[["admh"]]), 1934)
      expect_equal(nrow(read_df()[["adae"]]), 1934)
      expect_equal(nrow(read_df()[["adcm"]]), 3685)
    }
  )
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
