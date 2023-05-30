#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  Sys.sleep(2)
  load_data <- mod_data_read_server("data_read_1")

  observe({
    req(load_data$df_read())
    updateTabsetPanel(session, inputId = "tabcard", "Demographics")
  }) |>
    bindEvent(load_data$df_read())

  study_filters <-
    mod_global_filters_server("global_filters_1",
      dataset = "cadsl",
      load_data = load_data$df_read
    )

  processed_adsl <-
    mod_process_adsl_server(
      "process_adsl_1",
      dataset = "cadsl",
      df_out = load_data$df_read,
      global_filters = study_filters$filters,
      apply = study_filters$apply
    )

  mod_adsl_display_server("adsl_display_1",
    adsl = processed_adsl
  )

  mod_adae_global_server(
    "adae_global_1",
    dataset = "cadae",
    df_out = load_data$df_read,
    adsl = processed_adsl
  )

  mod_adxx_bodsys_server(
    "admh_bodsys_1",
    dataset = "cadmh",
    df_out = load_data$df_read,
    adsl = processed_adsl
  )

  mod_adxx_bodsys_server(
    "adcm_bodsys_1",
    dataset = "cadcm",
    df_out = load_data$df_read,
    adsl = processed_adsl
  )
}
