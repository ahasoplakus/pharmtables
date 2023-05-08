test_that("mod_global_filters_server works", {
  testServer(
    mod_global_filters_server,
    # Add here your module params
    args = list(
      id = "global_filters_abc",
      dataset = "cadsl",
      load_data = random.cdisc.data::cadsl
    )
    ,
    {
      ns <- session$ns
      expect_true(inherits(ns, "function"))
      expect_true(grepl(id, ns("global_filters_abc")))
      expect_true(grepl("test", ns("test")))

      session$setInputs(pop = "ITTFL")
      session$setInputs(sex = levels(load_data$SEX))
      session$setInputs(race = levels(load_data$RACE))
      session$setInputs(country = levels(load_data$COUNTRY)[1])

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
      expect_equal(filters()$pop, "ITTFL")
      expect_equal(filters()$sex, c("F", "M"))
      expect_equal(length(filters()$race), 8)
      expect_equal(filters()$race, all_race)
      expect_equal(filters()$country, "CHN")

      session$setInputs(pop = "SAFFL")
      session$setInputs(sex = levels(load_data$SEX)[1])
      session$setInputs(age = 50)
      expect_equal(
        filters(),
        list(
          pop = "SAFFL",
          sex = "F",
          race = all_race,
          ethnic = NULL,
          country = "CHN",
          age = 50,
          siteid = NULL,
          usubjid = NULL
        )
      )
    }
  )
})

test_that("module ui works", {
  ui <- mod_global_filters_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_global_filters_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
