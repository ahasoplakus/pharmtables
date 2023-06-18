test_that("mod_filter_reactivity_server works", {
  filt <- reactiveVal()
  testServer(
    mod_filter_reactivity_server,
    # Add here your module params
    args = list(
      id = "filter_reactivity_123",
      df = reactive(list(cadae = random.cdisc.data::cadae)),
      dataset = "cadae",
      filters = filt,
      trt_var = "ARM"
    ),
    {
      ns <- session$ns
      expect_true(inherits(ns, "function"))
      expect_true(grepl(id, ns("")))
      expect_true(grepl("test", ns("test")))

      filt("AESEV")
      session$flushReact()

      session$setInputs(aesev = c("MILD", "MODERATE", "SEVERE"))

      expect_equal(rv$filters$aesev, c("MILD", "MODERATE", "SEVERE"))
      expect_equal(
        rv$filter_cond,
        "AESEV %in% c('MILD','MODERATE','SEVERE')"
      )
      expect_identical(rv$filters, rv$cached_filters)
      expect_equal(rv$trig_report, 0)

      session$setInputs(aesev = c("MILD"))
      expect_equal(rv$filters$aesev, c("MILD"))
      expect_equal(rv$filter_cond, "AESEV %in% c('MILD')")
      expect_identical(rv$filters, rv$cached_filters)
      expect_equal(rv$trig_report, 0)

      filt("AESER")
      session$flushReact()

      session$setInputs(aeser = c("N", "Y"))
      expect_equal(rv$trig_report, 1)
      expect_equal(rv$filters$aeser, c("N", "Y"))
      expect_identical(rv$filters, rv$cached_filters)
      expect_equal(rv$filter_cond, "AESER %in% c('N','Y')")

      filt(c("AESER", "AESEV"))
      session$flushReact()

      expect_equal(rv$trig_report, 1)
      expect_equal(length(rv$filters), 2)
      expect_false(identical(rv$filters, rv$cached_filters))

      session$setInputs(
        aeser = c("N", "Y"),
        aesev = c("MILD", "MODERATE", "SEVERE")
      )

      expect_true(identical(rv$filters, rv$cached_filters))
      expect_equal(rv$trig_report, 2)
      expect_equal(
        rv$filter_cond,
        "AESER %in% c('N','Y') & AESEV %in% c('MILD','MODERATE','SEVERE')"
      )

      filt("AESEV")
      session$flushReact()

      session$setInputs(aesev = c("MILD", "MODERATE", "SEVERE"))

      expect_equal(rv$filters$aesev, c("MILD", "MODERATE", "SEVERE"))
      expect_equal(
        rv$filter_cond,
        "AESEV %in% c('MILD','MODERATE','SEVERE')"
      )
      expect_identical(rv$filters, rv$cached_filters)
      expect_equal(rv$trig_report, 3)
    }
  )
})

test_that("module ui works", {
  ui <- mod_filter_reactivity_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_filter_reactivity_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
