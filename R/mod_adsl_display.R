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
      id = ns("box_adsl"),
      title = "Demographic Characteristics",
      sidebar = boxSidebar(
        id = ns("demog_side"),
        background = "#EFF5F5",
        width = 25,
        h2("Table Options"),
        selectInput(
          ns("split_col"),
          "Split Cols by",
          choices = NULL,
          selected = NULL,
          width = 300
        ),
        selectInput(
          ns("split_row"),
          "Split Rows by",
          choices = NULL,
          selected = NULL,
          width = 300
        ),
        selectInput(
          ns("summ_var"),
          "Summarize",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          width = 300
        ),
        tagAppendAttributes(actionButton(ns("run"), "Update"),
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
mod_adsl_display_server <- function(id, adsl) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(trig_report = FALSE)

    observe({
      req(adsl())
      logger::log_info("mod_adsl_display_server: updating table options")

      trt_choices <-
        names(select(adsl(), setdiff(starts_with(c("ARM", "TRT0")), ends_with("DTM"))))
      rowgrp_choices <-
        names(discard(adsl(), is.numeric))
      summ_vars <-
        names(discard(adsl(), is.character))

      updateSelectInput(session,
                        "split_col",
                        choices = trt_choices,
                        selected = trt_choices[1])

      updateSelectInput(session,
                        "split_row",
                        choices = c("", rowgrp_choices),
                        selected = "")

      updateSelectInput(session,
                        "summ_var",
                        choices = summ_vars,
                        selected = summ_vars[1])
    }) |>
      bindEvent(adsl())

    observe({
      req(input$split_col != "")
      req(input$summ_var)
      rv$trig_report <- TRUE
    })

    disp_df <- reactive({
      req(adsl())
      req(input$split_col != "")
      req(input$summ_var)
      logger::log_info("mod_adsl_display_server: processed adsl has {nrow(adsl())} rows")

      lyt <- build_adsl(
        title = "",
        subtitle = "",
        footer = "",
        split_cols_by = input$split_col,
        split_rows_by = input$split_row,
        summ_vars = input$summ_var
      )

      logger::log_info("mod_adsl_display_server: sending adsl layout for display")

      return(list(
        out_df = adsl(),
        alt_df = NULL,
        lyt = lyt
      ))
    }) |>
      bindEvent(list(adsl(), rv$trig_report, input$run))

    mod_dt_table_server("dt_table_1",
                        display_df = disp_df)
  })
}
