#' data_read UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_read_ui <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

#' data_read Server Functions
#'
#' @noRd
mod_data_read_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    read_df <- reactive({
      req(data_list)
      logger::log_info("mod_data_read_server: reading {data_list}")
      df <- data_list |>
        map(~ paste0("random.cdisc.data::", .x)) |>
        map(~ eval(parse_expr(.x))) |>
        set_names(data_list)
    })

    return(read_df)
  })
}
