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
    htmlOutput(ns("out_data"))
  )
}

#' dt_table Server Functions
#'
#' @noRd
#'
#' @importFrom rtables build_table as_html
mod_dt_table_server <- function(id, display_df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$out_data <- renderPrint({
      req(display_df()$adsl)
      logger::log_info("mod_dt_table_server: display data")
      as_html(build_table(display_df()$lyt, display_df()$adsl))
    })
  })
}

## To be copied in the UI
# mod_dt_table_ui("dt_table_1")

## To be copied in the server
# mod_dt_table_server("dt_table_1")
