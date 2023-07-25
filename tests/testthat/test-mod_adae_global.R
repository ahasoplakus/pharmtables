testServer(
  mod_adae_global_server,
  # Add here your module params
  args = list(),
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
  }
)

test_that("module ui works", {
  ui <- mod_adae_global_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_adae_global_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
