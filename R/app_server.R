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
                         data_list = c("cadsl", "cadcm"))
  global_filters <-
    mod_global_filters_server("global_filters_1",
                              dataset = "cadsl",
                              load_data = load_data)
  mod_adsl_display_server("adsl_display_1",
                          dataset = "cadsl",
                          df_out = load_data,
                          global_filters = global_filters$filters,
                          apply = global_filters$apply)
}
