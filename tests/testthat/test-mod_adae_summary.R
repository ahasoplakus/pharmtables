data(adsl)
data(adae)

test_that("mod_adae_summary_server works", {
  filt <- reactiveVal()
  testServer(
    mod_adae_summary_server,
    # Add here your module params
    args = list(
      id = "adae_summary_abc",
      dataset = "adae",
      df_out = reactive(
        list(
          adsl = adsl,
          adae = adae
        )
      ),
      adsl = reactive(adsl),
      filters = filt
    ),
    {
      ns <- session$ns
      expect_true(inherits(ns, "function"))
      expect_true(grepl(id, ns("")))
      expect_true(grepl("test", ns("test")))

      df <- df_out()[[dataset]] |>
        filter(USUBJID %in% unique(adsl()$USUBJID)) |>
        add_adae_flags()

      disp_eve <- names(df)[93:107]

      exp_lyt <- basic_table(show_colcounts = TRUE) |>
        split_cols_by(var = "ARM", split_fun = drop_split_levels) |>
        add_overall_col(label = "All Patients") |>
        count_patients_with_flags(
          "USUBJID",
          flag_variables = var_labels(df[, "SER"]),
          .indent_mods = 1L,
          table_names = "sae"
        ) |>
        count_patients_with_flags(
          "USUBJID",
          flag_variables = var_labels(df[, c(
            "SAEFATAL", "SAELIFE", "SAEHOSP",
            "SAEDISAB", "SAECONG", "SAEMIE"
          )]),
          .indent_mods = 2L,
          table_names = "sae_fl"
        ) |>
        count_patients_with_flags(
          var = "USUBJID",
          flag_variables = var_labels(df[, c("WD", "WDSM")]),
          .indent_mods = 1L,
          table_names = "ae"
        ) |>
        count_patients_with_flags(
          var = "USUBJID",
          flag_variables = var_labels(df[, c("AEINT", "AERED", "AED", "AEMIE")]),
          .indent_mods = 2L,
          table_names = "ds"
        ) |>
        analyze_num_patients(
          vars = "USUBJID",
          .stats = "unique",
          .labels = c(unique = "Any AE"),
          .indent_mods = 1L,
          show_labels = "hidden"
        ) |>
        count_occurrences_by_grade(
          var = "AESEV",
          show_labels = "hidden",
          .indent_mods = 2L
        ) |>
        count_patients_with_flags(
          var = "USUBJID",
          flag_variables = var_labels(df[, c("CTC35", "CTC45")]),
          .indent_mods = 2L,
          table_names = "ctc"
        ) |>
        append_topleft(c("", "Adverse Events"))

      session$setInputs(split_col = "ARM")
      session$setInputs(events = names(select(df, all_of(disp_eve))))
      session$setInputs(run = 1)

      expect_equal(nrow(ae_summ()$out_df), 1934)
      expect_equal(nrow(ae_summ()$alt_df), 400)
      expect_identical(ae_summ()$lyt, exp_lyt)

      filt("AESER")
      session$flushReact()

      expect_equal(nrow(ae_summ()$out_df), 1934)
      expect_equal(nrow(ae_summ()$alt_df), 400)
    }
  )
})


test_that("module ui works", {
  ui <- mod_adae_summary_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_adae_summary_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
