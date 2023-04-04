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
  tagList(reactable::reactableOutput(ns("out_data")))
}

#' data_read Server Functions
#'
#' @noRd
mod_data_read_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    read_df <- reactive({
      logger::log_info("mod_data_read_server: reading random cdisc data")
      df <- random.cdisc.data::cadsl
    })

    output$out_data <- reactable::renderReactable({
      req(read_df())
      logger::log_info("mod_data_read_server: data has {nrow(read_df())} rows")
      reactable::reactable(read_df())
    })

  })
}

## To be copied in the UI
# mod_data_read_ui("data_read_1")

## To be copied in the server
# mod_data_read_server("data_read_1")
