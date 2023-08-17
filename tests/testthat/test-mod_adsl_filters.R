data(adsl)

test_that("mod_global_filters_server works", {
  testServer(
    mod_adsl_filters_server,
    # Add here your module params
    args = list(
      id = "global_filters_abc",
      dataset = "adsl",
      load_data = reactive(list(adsl = adsl)),
      filter_list = reactive(c("SEX", "RACE", "COUNTRY", "AGE"))
    ),
    {
      ns <- session$ns
      expect_true(inherits(ns, "function"))
      expect_true(grepl(id, ns("global_filters_abc")))
      expect_true(grepl("test", ns("test")))

      session$setInputs(pop = "ITTFL")
      session$setInputs(sex = levels(load_data()[[dataset]]$SEX))
      session$setInputs(race = levels(load_data()[[dataset]]$RACE))
      session$setInputs(country = levels(load_data()[[dataset]]$COUNTRY)[1])
      session$setInputs(age = max(load_data()[[dataset]]$AGE, na.rm = TRUE))
      session$setInputs(age1 = max(load_data()[[dataset]]$AGE, na.rm = TRUE))

      all_race <- c(
        "ASIAN",
        "BLACK OR AFRICAN AMERICAN",
        "WHITE",
        "AMERICAN INDIAN OR ALASKA NATIVE",
        "MULTIPLE",
        "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
        "OTHER",
        "UNKNOWN"
      )

      expect_true(length(filters()) > 0)
      expect_equal(filters()$sex, c("F", "M"))
      expect_equal(length(filters()$race), 8)
      expect_equal(filters()$race, all_race)
      expect_equal(filters()$country, "CHN")
      expect_equal(filters()$age, 69)

      session$setInputs(pop = "SAFFL")
      session$setInputs(sex = levels(load_data()[[dataset]]$SEX)[1])
      expect_equal(
        filters(),
        list(
          sex = "F",
          race = all_race,
          country = "CHN",
          age = 69
        )
      )
      expect_equal(rv$filters$pop, "SAFFL")
      expect_equal(
        rv$filters,
        list(
          sex = "F",
          race = all_race,
          country = "CHN",
          age = 69,
          pop = "SAFFL"
        )
      )
    }
  )
})

test_that("module ui works", {
  ui <- mod_adsl_filters_ui(id = "global_filters_abc")
  golem::expect_shinytag(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_adsl_filters_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
