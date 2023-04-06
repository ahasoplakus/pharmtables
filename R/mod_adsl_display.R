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
  tagList(
    box(
      sidebar = boxSidebar(id = "demog_side"),
      maximizable = TRUE,
      width = 12,
      mod_dt_table_ui(ns("dt_table_1"))
    )
  )
}

#' adsl_display Server Functions
#'
#' @noRd
#'
#' @importFrom rtables basic_table split_cols_by
#' @importFrom tern summarize_vars
mod_adsl_display_server <-
  function(id,
           dataset,
           df_out,
           global_filters = NULL,
           apply) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      adsl <- reactive({
        req(df_out())
        logger::log_info("mod_adsl_display_server: data has {nrow(df_out()[[dataset]])} rows")

        df <- df_out()[[dataset]]

        if (!is.null(global_filters())) {
          df <- df |>
            dplyr::filter(RACE %in% global_filters())
        }

        logger::log_info("mod_adsl_display_server: filtered data has {nrow(df)} rows")

        lyt <- build_adsl(split_by = "ARM",
                          summ_vars = c("AGE", "SEX", "COUNTRY", "RACE"))

        return(list(adsl = df, lyt = lyt))
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
