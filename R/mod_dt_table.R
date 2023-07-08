#' dt_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dt_table_ui <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("out_data")))
}

#' dt_table Server Functions
#'
#' @noRd
#'
#' @importFrom rtables build_table as_html
mod_dt_table_server <- function(id, display_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df_out <- reactive({
      req(display_df()$out_df)
      if (is.data.frame(display_df()$out_df)) {
        df <- tt_to_flextable(
          build_table(
            lyt = display_df()$lyt,
            df = display_df()$out_df,
            alt_counts_df = display_df()$alt_df
          )
        )
      } else {
        df <- tt_to_flextable(display_df()$out_df)
      }
      df
    }) |> bindEvent(display_df())

    output$out_data <- renderUI({
      req(df_out())
      logger::log_info("mod_dt_table_server: display data")
      df_out() |>
        flextable::autofit() |>
        flextable::theme_zebra(odd_body = "#F3F4ED", odd_header = "#ECF8F9") |>
        flextable::htmltools_value()
    })
  })
}
