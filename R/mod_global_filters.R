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
mod_global_filters_server <- function(id, dataset, load_data, filter_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(filters = NULL)

    output$glob_filt_ui <- renderUI({
      req(load_data()[[dataset]])
      req(filter_list())

      logger::log_info("mod_global_filters_server: initialise study filters")

      tagList(
        create_flag_widget(c("SAFFL", "ITTFL"), ns),
        create_widget(
          filter_list(),
          load_data(),
          dataset,
          ns
        ),
        actionButton(ns("apply"), "Update")
      )
    })

    outputOptions(output, "glob_filt_ui", priority = 975, suspendWhenHidden = FALSE)

    observe({
      req(load_data()[[dataset]])
      req(filter_list())
      filters <-
        set_names(tolower(filter_list())) |>
        map(\(x) input[[x]])
      filters[["pop"]] <- input$pop
      req(none(filters, is.null))
      logger::log_info("mod_global_filters_server: update study filters")
      rv$filters <- filters
    }, priority = 950)

    return(list(
      filters = eventReactive(rv$filters, rv$filters),
      apply = reactive(input$apply)
    ))
  })
}
