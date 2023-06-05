#' global_filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput updatePickerInput
mod_global_filters_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("glob_filt_ui"))
}

#' global_filters Server Functions
#'
#' @noRd
mod_global_filters_server <- function(id, dataset, load_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(filters = NULL)

    output$glob_filt_ui <- renderUI({
      req(load_data()[[dataset]])
      logger::log_info("mod_global_filters_server: update filters")

      tagList(
        create_flag_widget(c("SAFFL", "ITTFL"), ns),
        create_widget(
          c(
            "SEX", "RACE", "ETHNIC", "COUNTRY",
            "AGE", "SITEID", "USUBJID"
          ),
          load_data(),
          dataset,
          ns
        ),
        actionButton(ns("apply"), "Update")
      )
    })

    observe({
      rv$filters <-
        set_names(tolower(c("SEX", "RACE", "ETHNIC", "COUNTRY", "AGE", "SITEID", "USUBJID"))) |>
        map(\(x) input[[x]])
    })

    filters <- reactive({
      req(!every(rv$filters, is.null))
      logger::log_info("mod_global_filters_server: store filters")
      rv$filters[["pop"]] <- input$pop
      rv$filters
    })

    return(list(
      filters = filters,
      apply = reactive(input$apply)
    ))
  })
}
