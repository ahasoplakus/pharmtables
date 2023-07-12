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

    rv <- reactiveValues(
      filters = NULL,
      cached_filters = NULL
    )

    observe({
      req(load_data()[[dataset]])
      req(filter_list())
      req(is.null(rv$cached_filters) || length(setdiff(
        filter_list(),
        toupper(names(rv$cached_filters))
      )) > 0)

      flag_vars <- names(select(
        load_data()[[dataset]],
        setdiff(
          ends_with("FL"),
          starts_with(c("DIS", "DTH", "DS"))
        )
      ))

      rv$widget <- tagList(
        create_flag_widget(load_data()[[dataset]], flag_vars, ns),
        create_widget(
          filter_list(),
          load_data(),
          dataset,
          ns
        ),
        actionButton(ns("apply"), "Update")
      )
    })

    output$glob_filt_ui <- renderUI({
      req(rv$widget)
      logger::log_info("mod_global_filters_server: initialise study filters")
      rv$widget
    })

    outputOptions(output, "glob_filt_ui", priority = 975)

    filters <- reactive({
      req(load_data()[[dataset]])
      req(filter_list())
      req(length(reactiveValuesToList(input)) > 0)
      filters <-
        set_names(tolower(filter_list())) |>
        map(\(x) input[[x]])
    })

    observe(
      {
        req(filters())
        req(none(filters(), is.null))
        req(!identical(
          filters(),
          rv$cached_filters[names(filters())]
        ) ||
          !identical(rv$cached_pop, input$pop))
        logger::log_info("mod_global_filters_server: update study filters")
        rv$filters <- filters()
        rv$filters$pop <- input$pop
        init <- reactiveValuesToList(input)
        rv$cached_filters <-
          union(names(rv$filters), names(init)) |>
          set_names() |>
          map(\(x) {
            if (x != "pop" && !toupper(x) %in% names(load_data()[[dataset]])) {
              init[[x]] <- NULL
            }
            init[[x]]
          }) |>
          discard(is.null)
        rv$cached_pop <- input$pop
      },
      priority = 950
    ) |>
      bindEvent(list(filters(), input$pop))

    return(list(
      filters = reactive({
        req(rv$filters)
        rv$filters
      }),
      apply = reactive(input$apply)
    ))
  })
}
