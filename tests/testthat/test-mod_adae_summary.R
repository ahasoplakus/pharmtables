test_that("mod_adae_summary_server works", {
  testServer(
    mod_adae_summary_server,
    # Add here your module params
    args = list(
      id = "adae_summary_abc",
      dataset = "cadae",
      df_out = reactive(
        list(cadsl = random.cdisc.data::cadsl,
             cadae = random.cdisc.data::cadae)
      ),
      adsl = reactive(random.cdisc.data::cadsl)
    )
    ,
    {
      ns <- session$ns
      expect_true(inherits(ns, "function"))
      expect_true(grepl(id, ns("")))
      expect_true(grepl("test", ns("test")))

      df <- df_out()[[dataset]] |>
        filter(USUBJID %in% unique(adsl()$USUBJID)) |>
        mutate(
          fl1 = TRUE,
          fl2 = TRTEMFL == "Y",
          fl3 = TRTEMFL == "Y" & AEOUT == "FATAL",
          fl4 = TRTEMFL == "Y" & AEOUT == "FATAL" & AEREL == "Y",
          fl5 = TRTEMFL == "Y" & AEACN == "DRUG WITHDRAWN",
          fl6 = TRTEMFL == "Y" & DCSREAS == "ADVERSE EVENT"
        )

      labels <- c(
        "fl1" = "Total AEs",
        "fl2" = "Total number of patients with at least one adverse event",
        "fl3" = "Total number of patients with fatal AEs",
        "fl4" = "Total number of patients with related fatal AEs",
        "fl5" = "Total number of patients with drug withdrawn due to AEs",
        "fl6" = "Total number of patients discontinued due to AEs"
      )

      formatters::var_labels(df)[names(labels)] <- labels

      exp_lyt <- basic_table() |>
        split_cols_by(var = "ARM") |>
        rtables::add_colcounts() |>
        rtables::add_overall_col(label = "All Patients") |>
        rtables::add_colcounts() |>
        tern::count_patients_with_flags("USUBJID",
                                        flag_variables =
                                          formatters::var_labels(df[,
                                                                    c("fl1",
                                                                      "fl2",
                                                                      "fl3",
                                                                      "fl4",
                                                                      "fl5",
                                                                      "fl6")]))

      exp_lyt1 <- basic_table() |>
        split_cols_by(var = "ARM") |>
        rtables::add_colcounts() |>
        rtables::add_overall_col(label = "All Patients") |>
        rtables::add_colcounts() |>
        tern::count_patients_with_flags("USUBJID",
                                        flag_variables =
                                          formatters::var_labels(df[,
                                                                    c("fl4",
                                                                      "fl5",
                                                                      "fl6")]))

      session$setInputs(split_col = "ARM")
      session$setInputs(events = names(select(df, starts_with("fl"))))
      session$setInputs(run = 1)

      expect_equal(nrow(ae_summ()$out_df), 1934)
      expect_equal(nrow(ae_summ()$alt_df), 400)
      expect_identical(ae_summ()$lyt, exp_lyt)

      session$setInputs(events = names(select(df, c("fl4", "fl5", "fl6"))))
      session$setInputs(run = 2)

      expect_identical(ae_summ()$lyt, exp_lyt1)
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
