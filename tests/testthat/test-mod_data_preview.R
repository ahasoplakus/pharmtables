data(adsl)
data(adae)

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
    session$setInputs(data_up = "adsl")
    adsl_out <- output$data_str
    expect_true(is.character(adsl_out))
    session$setInputs(data_up = "adae")
    adae_out <- output$data_str
    expect_true(is.character(adae_out))
    expect_false(identical(adsl_out, adae_out))
  }
)

test_that("module ui works", {
  ui <- mod_data_preview_ui(id = "data_preview_123")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_data_preview_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
