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
mod_dt_table_server <- function(id,load_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$out_data <- DT::renderDataTable({
      req(load_data())
      logger::log_info("mod_data_read_server: data has {nrow(load_data()[['cadsl']])} rows")
      DT::datatable(load_data()[["cadsl"]])
    })
  })
}

## To be copied in the UI
# mod_dt_table_ui("dt_table_1")

## To be copied in the server
# mod_dt_table_server("dt_table_1")
