#' setup_filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_setup_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(column(
    width = 4,
    box(
      title = "Demographics",
      width = 12,
      selectInput(
        ns("dm_trt"),
        "Treatment Variables",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      selectInput(
        ns("dm_rows"),
        "Row group Variables",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      selectInput(
        ns("dm_summ"),
        "Summarize Variables",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      )
    )
  ),
  column(
    width = 4,
    box(title = "Adverse Events",
        width = 12)
  )))
}

#' setup_filters Server Functions
#'
#' @noRd
mod_setup_filters_server <- function(id, df_in) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(df_in()[["cadsl"]])
      df <- df_in()[["cadsl"]]
      logger::log_info("mod_setup_filters_server: setting up filter choices")
      trt_choices <-
        names(select(df, setdiff(starts_with(
          c("ARM", "TRT0")
        ), ends_with("DTM"))))
      rowgrp_choices <- names(purrr::discard(df, is.numeric))
      summ_vars <- names(purrr::discard(df, is.character))

      updateSelectInput(
        session,
        "dm_trt",
        "Treatment Variables",
        choices = trt_choices,
        selected = trt_choices[1]
      )

      updateSelectInput(
        session,
        "dm_rows",
        "Row group Variables",
        choices = rowgrp_choices,
        selected = "SEX"
      )

      updateSelectInput(
        session,
        "dm_summ",
        "Summarize Variables",
        choices = summ_vars,
        selected = "AGE"
      )
      logger::log_info("mod_setup_filters_server: filters choices setup complete")
    }) |>
      bindEvent(df_in())

    return(list(
      dm_filt = list(
        trt_filt = reactive(input$dm_trt),
        row_filt = reactive(input$dm_rows),
        summ_filt = reactive(input$dm_summ)
      ),
      ae_filt = NULL
    ))

  })
}

## To be copied in the UI
# mod_setup_filters_ui("setup_filters_1")

## To be copied in the server
# mod_setup_filters_server("setup_filters_1")
