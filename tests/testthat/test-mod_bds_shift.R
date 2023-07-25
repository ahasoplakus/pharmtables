data(adsl)
data(adlb)
data(advs)
dummy_lb <- select(adlb, -ANRIND)

test_that("mod_bds_shift_server works", {
  filt <- reactiveVal()
  testServer(
    mod_bds_shift_server,
    args = list(
      id = "bds_shift_abc",
      dataset = "adlb",
      df_out = reactive(list(
        adsl = adsl,
        adlb = adlb
      )),
      adsl = reactive(adsl),
      filters = filt
    ),
    {
      ns <- session$ns
      expect_true(inherits(ns, "function"))
      expect_true(grepl(id, ns("")))
      expect_true(grepl("test", ns("test")))

      exp_lyt <- build_shift_table(
        adsl = df_out()$adsl,
        bds_df = filter(df_out()$adlb, ANL01FL == "Y"),
        filter_cond = NULL,
        trt_var = "ARM",
        trt_label = c(ARM = "Description of Planned Arm"),
        group_var = "AVISIT",
        group_label = c(AVISIT = "Analysis Visit"),
        default_view = TRUE
      )

      session$setInputs(split_col = "ARM")
      session$setInputs(group_var = "AVISIT")
      session$setInputs(pop = "ANL01FL")
      session$setInputs(view = TRUE)
      session$setInputs(run = 1)

      expect_identical(xx_shift()$out_df, exp_lyt)
      expect_identical(
        xx_shift()$out_df[["body"]][["dataset"]],
        exp_lyt[["body"]][["dataset"]]
      )

      filt("AVISIT")
      session$flushReact()

      expect_identical(xx_shift()$out_df, exp_lyt)
      expect_identical(
        xx_shift()$out_df[["body"]][["dataset"]],
        exp_lyt[["body"]][["dataset"]]
      )
    }
  )
})

test_that("mod_bds_shift_server works when required variable is not there", {
  filt <- reactiveVal()
  testServer(
    mod_bds_shift_server,
    args = list(
      id = "bds_shift_abc1",
      dataset = "adlb",
      df_out = reactive(list(
        adsl = adsl,
        adlb = dummy_lb
      )),
      adsl = reactive(adsl),
      filters = filt
    ),
    {
      ns <- session$ns
      expect_true(inherits(ns, "function"))
      expect_true(grepl(id, ns("")))
      expect_true(grepl("test", ns("test")))

      session$setInputs(split_col = "ARM")
      session$setInputs(group_var = "AVISIT")
      session$setInputs(pop = "ANL01FL")
      session$setInputs(view = TRUE)
      session$setInputs(run = 1)

      expect_error(xx_shift()$out_df)
    }
  )
})

test_that("mod_bds_shift_server works with different dataset", {
  filt <- reactiveVal()
  testServer(
    mod_bds_shift_server,
    args = list(
      id = "bds_shift_abc2",
      dataset = "advs",
      df_out = reactive(list(
        adsl = adsl,
        advs = advs
      )),
      adsl = reactive(adsl),
      filters = filt
    ),
    {
      ns <- session$ns
      expect_true(inherits(ns, "function"))
      expect_true(grepl(id, ns("")))
      expect_true(grepl("test", ns("test")))

      exp_lyt <- build_shift_table(
        adsl = df_out()$adsl,
        bds_df = df_out()$advs,
        filter_cond = NULL,
        trt_var = "ARM",
        trt_label = c(ARM = "Description of Planned Arm"),
        group_var = "AVISIT",
        group_label = c(AVISIT = "Analysis Visit"),
        default_view = TRUE
      )

      session$setInputs(split_col = "ARM")
      session$setInputs(group_var = "AVISIT")
      session$setInputs(pop = NULL)
      session$setInputs(view = TRUE)
      session$setInputs(run = 1)

      expect_identical(xx_shift()$out_df, exp_lyt)
      expect_identical(
        xx_shift()$out_df[["body"]][["dataset"]],
        exp_lyt[["body"]][["dataset"]]
      )
    }
  )
})

test_that("module ui works", {
  ui <- mod_bds_shift_ui(id = "bds_shift_abc")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_bds_shift_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
