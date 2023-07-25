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
        mutate(
          FATAL = AESDTH == "Y",
          SER = AESER == "Y",
          SERWD = AESER == "Y" & AEACN == "DRUG WITHDRAWN",
          SERDSM = AESER == "Y" & AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
          RELSER = AESER == "Y" & AEREL == "Y",
          WD = AEACN == "DRUG WITHDRAWN",
          DSM = AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
          REL = AEREL == "Y",
          RELWD = AEREL == "Y" & AEACN == "DRUG WITHDRAWN",
          RELDSM = AEREL == "Y" & AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
          CTC35 = AETOXGR %in% c("3", "4", "5"),
          CTC45 = AETOXGR %in% c("4", "5")
        ) |>
        var_relabel(
          FATAL = "AE with fatal outcome",
          SER = "Serious AE",
          SERWD = "Serious AE leading to withdrawal from treatment",
          SERDSM = "Serious AE leading to dose modification/interruption",
          RELSER = "Related Serious AE",
          WD = "AE leading to withdrawal from treatment",
          DSM = "AE leading to dose modification/interruption",
          REL = "Related AE",
          RELWD = "Related AE leading to withdrawal from treatment",
          RELDSM = "Related AE leading to dose modification/interruption",
          CTC35 = "Grade 3-5 AE",
          CTC45 = "Grade 4/5 AE"
        )

      disp_eve <- c(
        "FATAL", "SER", "SERWD", "SERDSM", "RELSER", "WD",
        "DSM", "REL", "RELWD", "RELDSM", "CTC35", "CTC45"
      )

      disp_eve1 <- c("FATAL", "SER", "SERWD")

      exp_lyt <- basic_table(show_colcounts = TRUE) |>
        split_cols_by(var = "ARM", split_fun = drop_split_levels) |>
        add_overall_col(label = "All Patients") |>
        count_patients_with_event(
          vars = "USUBJID",
          filters = c("STUDYID" = as.character(unique(df[["STUDYID"]]))),
          denom = "N_col",
          .labels = c(count_fraction = "Total number of patients with at least one adverse event")
        ) |>
        count_values(
          "STUDYID",
          values = as.character(unique(df[["STUDYID"]])),
          .stats = "count",
          .labels = c(count = "Total AEs"),
          table_names = "total_aes"
        ) |>
        count_patients_with_flags("USUBJID",
          flag_variables = var_labels(df[, disp_eve]),
          denom = "N_col",
          var_labels = "Total number of patients with at least one",
          show_labels = "visible"
        ) |>
        append_topleft(c("", "Adverse Events"))

      exp_lyt1 <- basic_table(show_colcounts = TRUE) |>
        split_cols_by(var = "ARM", split_fun = drop_split_levels) |>
        add_overall_col(label = "All Patients") |>
        count_patients_with_event(
          vars = "USUBJID",
          filters = c("STUDYID" = as.character(unique(df[["STUDYID"]]))),
          denom = "N_col",
          .labels = c(count_fraction = "Total number of patients with at least one adverse event")
        ) |>
        count_values(
          "STUDYID",
          values = as.character(unique(df[["STUDYID"]])),
          .stats = "count",
          .labels = c(count = "Total AEs"),
          table_names = "total_aes"
        ) |>
        count_patients_with_flags("USUBJID",
          flag_variables = var_labels(df[, disp_eve1]),
          denom = "N_col",
          var_labels = "Total number of patients with at least one",
          show_labels = "visible"
        ) |>
        append_topleft(c("", "Adverse Events"))

      session$setInputs(split_col = "ARM")
      session$setInputs(events = names(select(df, all_of(disp_eve))))
      session$setInputs(run = 1)

      expect_equal(nrow(ae_summ()$out_df), 1934)
      expect_equal(nrow(ae_summ()$alt_df), 400)
      expect_identical(ae_summ()$lyt, exp_lyt)

      session$setInputs(events = names(select(df, all_of(disp_eve1))))
      session$setInputs(run = 2)

      expect_identical(ae_summ()$lyt, exp_lyt1)

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
