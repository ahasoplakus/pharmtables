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
      title = tags$strong("Demographic Characteristics"),
      sidebar = boxSidebar(
        id = ns("demog_side"),
        background = "#EFF5F5",
        width = 35,
        h2(tags$strong("Table Options")),
        selectInput(
          ns("split_col"),
          "Split Cols by",
          choices = NULL,
          selected = NULL,
          width = 400
        ),
        selectInput(
          ns("summ_var"),
          "Summarize",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          width = 400
        ),
        tagAppendAttributes(actionButton(ns("run"), "Update"),
          class = "side_apply"
        )
      ),
      maximizable = TRUE,
      width = 12,
      height = "800px",
      div(withSpinner(mod_dt_table_ui(ns("dt_table_1")), type = 6, color = "#3BACB6"),
        style = "overflow-x: scroll;"
      )
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
        sort(names(discard(adsl(), is.numeric)))
      summ_vars <-
        c(
          sort(names(keep(adsl(), is.numeric))),
          sort(names(keep(adsl(), is.factor)))
        )

      updateSelectInput(session,
        "split_col",
        choices = trt_choices,
        selected = trt_choices[1]
      )

      updateSelectInput(session,
        "summ_var",
        choices = summ_vars,
        selected = c("SEX", "AGE", "RACE", "ETHNIC")
      )
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

      lyt <- build_adsl_chars_table(
        title = "",
        subtitle = "",
        footer = "",
        split_cols_by = input$split_col,
        summ_vars = input$summ_var
      )

      logger::log_info("mod_adsl_display_server: sending adsl layout for display")

      return(list(
        out_df = adsl(),
        alt_df = NULL,
        lyt = lyt
      ))
    }) |>
      bindCache(list(adsl(), input$split_col, input$summ_var)) |>
      bindEvent(list(adsl(), rv$trig_report, input$run))

    mod_dt_table_server("dt_table_1",
      display_df = disp_df
    )
  })
}
