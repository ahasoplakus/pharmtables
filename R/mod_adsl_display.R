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
      title = uiOutput(ns("table_title")),
      sidebar = boxSidebar(
        id = ns("demog_side"),
        background = "#EFF5F5",
        icon = icon("table-cells"),
        width = 35,
        div(
          accordion(
            id = ns("adsl_accord"),
            tagAppendAttributes(accordionItem(
              title = tags$strong("Table Display Options"),
              collapsed = FALSE,
              selectInput(
                ns("split_col"),
                "Treatment Variable",
                choices = NULL,
                selected = NULL,
                width = "100vw"
              ),
              selectizeInput(
                ns("group_var"),
                "Additional Grouping Variable(s)",
                choices = NULL,
                selected = NULL,
                options = list(maxItems = 2)
              ),
              selectInput(
                ns("summ_var"),
                "Summarize",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                width = "100vw"
              ),
              prettyCheckboxGroup(
                inputId = ns("stats"),
                label = "Show/Hide Statistic",
                choiceNames = NULL,
                choiceValues = NULL,
                selected = NULL,
                animation = "pulse",
                shape = "curve",
                status = "info"
              )
            ), class = "side_accord")
          ),
          style = "display: flex; justify-content: center;"
        ),
        fluidRow(
          div(
            tagAppendAttributes(actionButton(ns("run"), "Update"),
              class = "side_apply"
            ),
            style = "display: flex; justify-content: center; width: 100vw;"
          )
        )
      ),
      maximizable = TRUE,
      collapsible = FALSE,
      width = 12,
      headerBorder = FALSE,
      div(shinycssloaders::withSpinner(mod_dt_table_ui(ns("dt_table_1")), color = "#3BACB6"),
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
#' @importFrom tern analyze_vars
mod_adsl_display_server <- function(id, adsl, pop_fil) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(trig_report = FALSE)

    observe({
      req(adsl())
      logger::log_info("mod_adsl_display_server: updating table options")

      trt_choices <-
        names(select(adsl(), setdiff(
          starts_with(c("ACT", "ARM", "TRT", "TR0", "TR1", "TR2")),
          ends_with(c("DTM", "DUR", "PN", "AN", "DT", "FL"))
        )))
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

      updateSelectizeInput(
        session,
        "group_var",
        "Additional Grouping Variable(s)",
        choices = summ_vars,
        selected = NULL,
        options = list(maxItems = 2)
      )

      updateSelectInput(session,
        "summ_var",
        choices = summ_vars,
        selected = c("SEX", "AGE", "RACE", "ETHNIC")
      )

      updatePrettyCheckboxGroup(
        inputId = "stats",
        label = "Show/Hide Statistic",
        choiceNames =
          c("n", "Mean, SD", "Standard Error", "Median", "Min-Max", "IQR", "Count Fraction"),
        choiceValues = c("n", "mean_sd", "se", "median", "range", "quantiles", "count_fraction"),
        selected = c("n", "mean_sd", "se", "median", "range", "quantiles", "count_fraction"),
        prettyOptions = list(
          animation = "pulse",
          status = "info",
          shape = "curve"
        )
      )
    }) |>
      bindEvent(adsl())

    observe({
      req(input$split_col != "")
      req(input$summ_var)
      req(input$stats)
      rv$trig_report <- TRUE
    })

    disp_df <- reactive({
      req(adsl())
      req(input$split_col != "")
      req(input$summ_var)
      req(input$stats)
      logger::log_info("mod_adsl_display_server: processed adsl has {nrow(adsl())} rows")

      lyt <- build_adsl_chars_table(
        title = "",
        subtitle = "",
        footer = "",
        split_cols_by = input$split_col,
        group_by = input$group_var,
        summ_vars = input$summ_var,
        disp_stat = input$stats
      )

      logger::log_info("mod_adsl_display_server: sending adsl layout for display")

      return(list(
        out_df = adsl(),
        alt_df = NULL,
        lyt = lyt
      ))
    }) |>
      bindCache(list(adsl(), input$split_col, input$group_var, input$summ_var, input$stats)) |>
      bindEvent(list(adsl(), rv$trig_report, input$run))

    output$table_title <- renderUI({
      req(disp_df())
      req(pop_fil())
      tags$strong(
        paste0(
          "Table 1.1 Demographic Characteristics; ",
          str_replace_all(str_to_title(attr(adsl()[[pop_fil()]], "label")), " Flag", "")
        )
      )
    })

    mod_dt_table_server("dt_table_1",
      display_df = disp_df
    )
  })
}
