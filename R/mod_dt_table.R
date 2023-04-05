#' dt_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dt_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("out_data"))
  )
}

#' dt_table Server Functions
#'
#' @noRd
mod_dt_table_server <- function(id, display_df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$out_data <- DT::renderDataTable({
      req(display_df())
      logger::log_info("mod_dt_table_server: display data")
      DT::datatable(display_df())
    })
  })
}

## To be copied in the UI
# mod_dt_table_ui("dt_table_1")

## To be copied in the server
# mod_dt_table_server("dt_table_1")
