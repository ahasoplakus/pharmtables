data(adsl)

test_that("mod_adsl_display_server works", {
  testServer(mod_adsl_display_server,
    # Add here your module params
    args = list(id = "adsl_display_abc", adsl = reactive(adsl), pop_fil = reactive("SAFFL")),
    {
      ns <- session$ns
      expect_true(inherits(ns, "function"))
      expect_true(grepl(id, ns("")))
      expect_true(grepl("test", ns("test")))

      session$setInputs(split_col = "ARM")
      session$setInputs(summ_var = c("AGE", "SEX"))
      session$setInputs(
        stats =
          c(
            "n",
            "mean_sd",
            "se",
            "median",
            "range",
            "quantiles",
            "count_fraction"
          )
      )
      session$setInputs(run = 1)

      exp_lyt <- build_adsl_chars_table(
        title = "",
        subtitle = "",
        footer = "",
        split_cols_by = "ARM",
        summ_vars = c("AGE", "SEX")
      )

      exp_lyt1 <- build_adsl_chars_table(
        title = "",
        subtitle = "",
        footer = "",
        split_cols_by = "ARM",
        summ_vars = c("AGE", "SEX", "COUNTRY")
      )

      expect_equal(nrow(disp_df()$out_df), 400)
      expect_null(disp_df()$alt_df)
      expect_identical(disp_df()$lyt, exp_lyt)

      session$setInputs(summ_var = c("AGE", "SEX", "COUNTRY"))
      session$setInputs(run = 2)

      expect_identical(disp_df()$lyt, exp_lyt1)
      expect_false(identical(disp_df()$lyt, exp_lyt))
      expect_equal(
        as.character(output$table_title$html),
        "<strong>Table 1.1 Demographic Characteristics; Safety Population</strong>"
      )
    }
  )
})

test_that("module ui works", {
  ui <- mod_adsl_display_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_adsl_display_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
