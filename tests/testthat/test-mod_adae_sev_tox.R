data(adsl)
data(adae)

test_that("mod_adae_sev_tox_server works", {
  filt <- reactiveVal()
  testServer(
    mod_adae_sev_tox_server,
    # Add here your module params
    args = list(
      id = "adae_display_abc",
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
      session$setInputs(view = FALSE)
      session$setInputs(run = 1)

      exp_lyt <- basic_table() |>
        split_cols_by(var = "ARM", split_fun = drop_split_levels) |>
        add_colcounts() |>
        add_overall_col(label = "All Patients") |>
        add_colcounts() |>
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = str_glue("Total number of patients with at least one adverse event"),
            nonunique = str_glue("Total number of adverse events")
          )
        ) |>
        split_rows_by("AESOC",
          child_labels = "visible",
          nested = TRUE,
          label_pos = "topleft",
          split_label = obj_label(df_out()[[dataset]][["AESOC"]]),
          split_fun = drop_split_levels
        ) |>
        split_rows_by("AETERM",
          child_labels = "visible",
          nested = TRUE,
          label_pos = "topleft",
          split_label = obj_label(df_out()[[dataset]][["AETERM"]]),
          split_fun = drop_split_levels
        ) |>
        summarize_occurrences_by_grade("AETOXGR")

      exp_df <- build_table(
        lyt = exp_lyt,
        df = df_out()[[dataset]],
        alt_counts_df = adsl()
      )

      expect_identical(ae_explore()$out_df, exp_df)
      expect_equal(nrow(ae_explore()$alt_df), NULL)
      expect_identical(ae_explore()$lyt, NULL)

      filt("AESER")
      session$flushReact()

      expect_identical(ae_explore()$out_df, exp_df)
      expect_equal(nrow(ae_explore()$alt_df), NULL)
    }
  )
})

test_that("module ui works", {
  ui <- mod_adae_sev_tox_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_adae_sev_tox_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
