test_that("mod_adxx_bodsys_server works", {
  filt <- reactiveVal()
  testServer(
    mod_adxx_bodsys_server,
    # Add here your module params
    args = list(
      id = "adxx_bodsys_abc",
      dataset = "cadae",
      df_out = reactive(
        list(
          cadsl = random.cdisc.data::cadsl,
          cadae = random.cdisc.data::cadae
        )
      ),
      adsl = reactive(random.cdisc.data::cadsl),
      filters = filt
    ),
    {
      ns <- session$ns
      expect_true(inherits(ns, "function"))
      expect_true(grepl(id, ns("")))
      expect_true(grepl("test", ns("test")))

      exp_lyt <- basic_table() |>
        split_cols_by(var = "ACTARM") |>
        add_colcounts() |>
        add_overall_col(label = "All Patients") |>
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one event",
            nonunique = "Total number of events"
          )
        ) |>
        split_rows_by(
          "AEBODSYS",
          label_pos = "topleft",
          split_label = obj_label(df_out()[[dataset]][["AEBODSYS"]]),
          split_fun = drop_split_levels
        ) |>
        summarize_num_patients(
          var = "USUBJID",
          .stats = "unique",
          .labels = c(unique = NULL)
        ) |>
        count_occurrences(vars = "AEDECOD") |>
        append_topleft(paste(" ", obj_label(df_out()[[dataset]][["AEDECOD"]])))

      session$setInputs(split_col = "ACTARM")
      session$setInputs(class = "AEBODSYS")
      session$setInputs(term = "AEDECOD")
      session$setInputs(run = 1)

      expect_identical(xx_bodsys()$lyt, exp_lyt)
      expect_equal(nrow(xx_bodsys()$out_df), 1934)
      expect_equal(nrow(xx_bodsys()$alt_df), 400)

      filt("AESER")
      session$flushReact()

      expect_equal(nrow(xx_bodsys()$out_df), 1934)
      expect_equal(nrow(xx_bodsys()$alt_df), 400)
    }
  )
})

test_that("module ui works", {
  ui <- mod_adxx_bodsys_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_adxx_bodsys_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
