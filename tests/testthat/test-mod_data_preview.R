data(adsl)
data(adae)


test_that("mod_data_preview_server works", {
  testServer(
    mod_data_preview_server,
    # Add here your module params
    args = list(
      id = "data_preview_123",
      df = reactive(list(
        adae = adae,
        adsl = adsl
      ))
    ),
    {
      ns <- session$ns
      expect_true(
        inherits(ns, "function")
      )
      expect_true(
        grepl(id, ns(""))
      )
      expect_true(
        grepl("test", ns("test"))
      )
      expect_true(length(prev_data()) > 0)
      expect_equal(prev_data()[[1]]$class, "reactR_markup")
      expect_error(output$print_dom)
      expect_error(output$data_str)
      session$setInputs(print_dat__reactable__selected = 1L)
      adae_out <- output$data_str
      expect_true(is.character(output$data_str))
      expect_true(is.character(output$print_dom))
      session$setInputs(print_dat__reactable__selected = 2L)
      adsl_out <- output$data_str
      expect_true(is.character(output$data_str))
      expect_true(is.character(output$print_dom))
      expect_false(identical(adae_out, adsl_out))
    }
  )
})

test_that("module ui works", {
  ui <- mod_data_preview_ui(id = "data_preview_123")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_data_preview_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
