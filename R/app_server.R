#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  load_data <-
    mod_data_read_server("data_read_1",
                         data_list = c("cadsl", "cadae"))
  global_filters <-
    mod_global_filters_server("global_filters_1",
                              dataset = "cadsl",
                              load_data = load_data)
  filt_adsl <-
    mod_process_adsl_server(
      "process_adsl_1",
      dataset = "cadsl",
      df_out = load_data,
      global_filters = global_filters$filters,
      apply = global_filters$apply
    )

  mod_adsl_display_server("adsl_display_1",
                          adsl = filt_adsl)

  mod_adae_summary_server(
    "adae_summary_1",
    dataset = "cadae",
    df_out = load_data,
    adsl = filt_adsl
  )

  mod_adae_bodsys_server(
    "adae_bodsys_1",
    dataset = "cadae",
    df_out = load_data,
    adsl = filt_adsl
  )

  mod_adae_display_server(
    "adae_display_1",
    dataset = "cadae",
    df_out = load_data,
    adsl = filt_adsl
  )
}
