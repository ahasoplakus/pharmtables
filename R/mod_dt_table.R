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
      logger::log_info("mod_dt_table_server: formatting table")
      req(display_df()$out_df)
      if (isTRUE(inherits(display_df()$out_df, "flextable"))) {
        df <- display_df()$out_df |>
          table_options()
      } else {
        if (nrow(display_df()$out_df) < 1) {
          show_toast(
            title = "Filtered data has no observation",
            text = "Try applying a different filter",
            type = "error",
            position = "center",
            width = "600px"
          )
        }
        req(nrow(display_df()$out_df) > 0)

        if (is.list(display_df()$lyt)) {
          tbl1 <- build_table(lyt = display_df()$lyt[[1]], df = display_df()$out_df)
          tbl2 <- build_table(lyt = display_df()$lyt[[2]], df = display_df()$out_df)
          rtables::col_info(tbl1) <- rtables::col_info(tbl2)

          tt <- rbind(tbl1, tbl2)
        } else {
          tt <- build_table(
            lyt = display_df()$lyt,
            df = display_df()$out_df,
            alt_counts_df = display_df()$alt_df
          )
        }
        df <- tt_to_flextable(
          tt,
          counts_in_newline = TRUE,
          border = flextable::fp_border_default(color = "#343a40", width = 0.5),
          theme = rtables::theme_docx_default(tt, font_size = 12, bold = "header")
        )
      }
      df
    }) |>
      bindEvent(display_df())

    output$out_data <- renderUI({
      req(df_out())
      logger::log_info("mod_dt_table_server: display data")
      df_out() |>
        flextable::htmltools_value(ft.align = "center")
    })
  })
}
