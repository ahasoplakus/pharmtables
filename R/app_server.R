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

  mod_dt_table_server("dt_table_1",
                      load_data = load_data)
}
