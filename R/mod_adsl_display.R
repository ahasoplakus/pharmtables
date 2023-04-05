#' adsl_display UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_adsl_display_ui <- function(id) {
  ns <- NS(id)
  tagList(mod_dt_table_ui(ns("dt_table_1")))
}

#' adsl_display Server Functions
#'
#' @noRd
mod_adsl_display_server <-
  function(id, dataset, df_out, global_filters = NULL, apply) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      adsl <- reactive({
        req(df_out())
        req(global_filters())
        logger::log_info("mod_adsl_display_server: data has {nrow(df_out()[[dataset]])} rows")

        df <- df_out()[[dataset]] |>
          dplyr::filter(RACE %in% global_filters())

        logger::log_info("mod_adsl_display_server: filtered data has {nrow(df)} rows")

        df
      }) |>
        bindEvent(list(apply()))

      mod_dt_table_server("dt_table_1",
                          display_df = adsl)
    })
  }

## To be copied in the UI
# mod_adsl_display_ui("adsl_display_1")

## To be copied in the server
# mod_adsl_display_server("adsl_display_1")
