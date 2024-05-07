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

      exp_lyt <- build_disp_table(
        adsl = adsl(),
        trt_var = "ARM",
        eos_var = "EOSSTT",
        eot_var = "EOTSTT",
        dcs_reas = "DCSREAS",
        dct_reas = "DCTREAS"
      )

      tbl1 <- build_table(lyt = disp_df()$lyt[[1]], df = disp_df()$out_df)
      tbl2 <- build_table(lyt = disp_df()$lyt[[2]], df = disp_df()$out_df)
      rtables::col_info(tbl1) <- rtables::col_info(tbl2)
      act_tbl <- rbind(tbl1, tbl2)

      tbl_1 <- build_table(lyt = exp_lyt$lyt[[1]], df = exp_lyt$df)
      tbl_2 <- build_table(lyt = exp_lyt$lyt[[2]], df = exp_lyt$df)
      rtables::col_info(tbl_1) <- rtables::col_info(tbl_2)
      exp_tbl <- rbind(tbl_1, tbl_2)

      expect_identical(disp_df()$out_df, exp_lyt$df)
      expect_identical(disp_df()$lyt, exp_lyt$lyt)
      expect_identical(act_tbl, exp_tbl)
      expect_snapshot(rbind(tbl1, tbl2))
      expect_equal(
        as.character(output$table_title$html),
        "<strong>Table 1.2 Patient Disposition; Intent-To-Treat Population</strong>"
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
