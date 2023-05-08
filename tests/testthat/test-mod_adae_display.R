test_that("mod_adae_display_server works", {
  testServer(
    mod_adae_display_server,
    # Add here your module params
    args = list(
      id = "adae_display_abc",
      dataset = "cadae",
      df_out = reactive(
        list(cadsl = random.cdisc.data::cadsl,
             cadae = random.cdisc.data::cadae)
      ),
      adsl = reactive(random.cdisc.data::cadsl)
    )
    , {
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
      session$setInputs(class = "AESOC")
      session$setInputs(term = "AETERM")
      session$setInputs(summ_var = "AETOXGR")
      session$setInputs(run = 1)

      exp_lyt <- basic_table() |>
        split_cols_by(var = "ARM") |>
        add_colcounts() |>
        add_overall_col(label = "All Patients") |>
        add_colcounts() |>
        summarize_num_patients("USUBJID") |>
        split_rows_by("AESOC",
                      child_labels = "visible",
                      nested = TRUE,
                      indent_mod = 1,
                      split_fun = drop_split_levels) |>
        split_rows_by("AETERM",
                      child_labels = "visible",
                      nested = TRUE,
                      indent_mod = 2,
                      split_fun = drop_split_levels) |>
        summarize_occurrences_by_grade("AETOXGR")

      expect_equal(nrow(ae_explore()$out_df), 1934)
      expect_equal(nrow(ae_explore()$alt_df), 400)
      expect_identical(ae_explore()$lyt, exp_lyt)
    })
})

test_that("module ui works", {
  ui <- mod_adae_display_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_adae_display_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

