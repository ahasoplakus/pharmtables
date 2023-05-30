test_that("mod_process_adsl_server works", {
  trigger <- reactiveVal()
  gf <- reactiveVal()
  testServer(
    mod_process_adsl_server,
    # Add here your module params
    args = list(
      id = "process_adsl_xyz",
      dataset = "cadsl",
      df_out = reactive(list(cadsl = random.cdisc.data::cadsl)),
      global_filters = gf,
      apply = trigger
    ),
    {
      ns <- session$ns
      expect_true(inherits(ns, "function"))
      expect_true(grepl(id, ns("")))
      expect_true(grepl("test", ns("test")))

      # check adsl gets filtered when app loads with default global filters
      trigger(1)
      gf(
        list(
          pop = "SAFFL",
          sex = c("F"),
          race = "ASIAN",
          ethnic = c(
            " NOT REPORTED",
            "HISPANIC OR LATINO",
            "NOT HISPANIC OR LATINO",
            "UNKNOWN"
          ),
          country = levels(random.cdisc.data::cadsl[["COUNTRY"]]),
          age = 69,
          siteid = unique(random.cdisc.data::cadsl[["SITEID"]]),
          usubjid = unique(random.cdisc.data::cadsl[["USUBJID"]])
        )
      )
      session$flushReact()
      expect_equal(nrow(adsl()), 120)
      expect_equal(unique(as.character(adsl()$SEX)), "F")
      expect_equal(unique(as.character(adsl()$RACE)), "ASIAN")

      # check adsl gets filtered when global_filters update and apply is clicked
      trigger(2)
      gf(list_assign(gf(), sex = "M"))
      session$flushReact()

      expect_equal(nrow(adsl()), 88)
      expect_equal(unique(as.character(adsl()$SEX)), "M")

      # check adsl does not update if global filters update but apply is not clicked
      gf(list_assign(gf(), age = 40))
      session$flushReact()

      expect_equal(nrow(adsl()), 88)
      expect_true(max(adsl()$AGE, na.rm = TRUE) > 40)
      expect_equal(unique(as.character(adsl()$SEX)), "M")

      # check adsl updates when apply is clicked with most recent global filters
      trigger(3)
      session$flushReact()

      expect_equal(nrow(adsl()), 65)
      expect_true(max(adsl()$AGE, na.rm = TRUE) <= 40)
      expect_equal(unique(as.character(adsl()$SEX)), "M")
    }
  )
})

test_that("module ui works", {
  ui <- mod_process_adsl_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_process_adsl_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
