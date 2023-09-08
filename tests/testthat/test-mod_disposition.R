data(adsl)

test_that("mod_disposition_server works", {
  testServer(
    mod_disposition_server,
    # Add here your module params
    args = list(id = "disposition_abc", adsl = reactive(adsl), pop_fil = reactive("ITTFL")),
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

      session$setInputs(split_col = "ARM")
      session$setInputs(eos = "EOSSTT")
      session$setInputs(eot = "EOTSTT")
      session$setInputs(dcs_reas = "DCSREAS")
      session$setInputs(dct_reas = "DCTREAS")
      session$setInputs(run = 1)

      exp_tbl <- build_disp_table(
        adsl = adsl(),
        trt_var = "ARM",
        eos_var = "EOSSTT",
        eot_var = "EOTSTT",
        dcs_reas = "DCSREAS",
        dct_reas = "DCTREAS"
      )

      expect_true(!is.null(disp_df()$out_df))
      expect_identical(disp_df()$out_df, exp_tbl)
      expect_equal(
        as.character(output$table_title$html),
        "<strong>Table 1.2. Patient Disposition; Intent-To-Treat Population</strong>"
      )
    }
  )
})

test_that("module ui works", {
  ui <- mod_disposition_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_disposition_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
