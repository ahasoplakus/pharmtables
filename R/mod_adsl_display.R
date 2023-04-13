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
      sidebar = boxSidebar(
        id = "demog_side",
        background = "#EFF5F5",
        width = 25,
        h2("Table Options"),
        selectInput(
          ns("split_col"),
          "Split Cols by",
          choices = c("ARM", "ACTARM", "TRT01P", "TRT02P", "TRT01A", "TRT02A"),
          selected = c("ARM"),
          width = 300
        ),
        selectInput(
          ns("split_row"),
          "Split Rows by",
          choices = c("", "SEX", "RACE", "ETHNIC", "COUNTRY"),
          selected = NULL,
          width = 300
        ),
        selectInput(
          ns("summ_var"),
          "Summarize",
          choices = c("AGE", "SEX", "COUNTRY", "RACE", "ETHNIC"),
          selected = c("AGE", "SEX", "COUNTRY"),
          multiple = TRUE,
          width = 300
        ),
        tagAppendAttributes(actionButton(ns("run"), "Apply"),
                            class = "side_apply")
      ),
      maximizable = TRUE,
      width = 12,
      height = "800px",
      mod_dt_table_ui(ns("dt_table_1"))
    )
  )
}

#' adsl_display Server Functions
#'
#' @noRd
#'
#' @importFrom rtables basic_table split_cols_by split_rows_by add_overall_col
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
        req(!purrr::every(global_filters(), is.null))
        req(input$split_col)
        req(input$summ_var)
        logger::log_info("mod_adsl_display_server: data has {nrow(df_out()[[dataset]])} rows")

        df <- filter_adsl(
          df_out()[[dataset]],
          global_filters()$pop,
          global_filters()$sex,
          global_filters()$race,
          global_filters()$ethnic,
          global_filters()$country,
          global_filters()$age,
          global_filters()$siteid,
          global_filters()$usubjid
        )

        logger::log_info("mod_adsl_display_server: filtered data has {nrow(df)} rows")

        lyt <- build_adsl(
          split_cols_by = input$split_col,
          split_rows_by = input$split_row,
          summ_vars = input$summ_var
        )

        return(list(adsl = df, lyt = lyt))
      }) |>
        bindEvent(list(apply(), input$run))

      mod_dt_table_server("dt_table_1",
                          display_df = adsl)
    })
  }

## To be copied in the UI
# mod_adsl_display_ui("adsl_display_1")

## To be copied in the server
# mod_adsl_display_server("adsl_display_1")
