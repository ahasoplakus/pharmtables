#' filter_reactivity UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param domain Name of domain or `ADaM` dataset
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_filter_reactivity_ui <- function(id, domain = "ADAE", logo = "head-side-cough") {
  ns <- NS(id)
  tagList(
    div(
      id = ns("domain_filters"),
      accordion(
        id = ns("acc_filt_react"),
        accordionItem(
          title = tags$span(icon(logo), tags$strong(str_glue("{domain} Filters"))),
          collapsed = FALSE,
          uiOutput(ns("xx_filt_ui"))
        )
      ),
      style = "width: 350px;"
    )
  )
}

#' filter_reactivity Server Functions
#'
#' @noRd
mod_filter_reactivity_server <- function(id, df, dataset, filters, trt_var) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(trig_report = 0)

    observe({
      req(df()[[dataset]])
      req(filters())
      req(!identical(filters(), rv$cached_filter_list))

      rv$widget <- tagList(
        create_widget(
          filters(),
          df(),
          dataset,
          ns
        )
      )
    })

    output$xx_filt_ui <- renderUI({
      req(rv$widget)
      logger::log_info("mod_filter_reactivity_server: initialise {dataset} filters")
      rv$widget
    })

    outputOptions(output, "xx_filt_ui", priority = 925)

    observe(
      {
        req(df()[[dataset]])
        req(filters())
        req(length(reactiveValuesToList(input)) > 0)
        req(trt_var != "")

        rv$filters <-
          set_names(tolower(filters())) |>
          map(\(x) input[[x]])
        req(none(rv$filters, is.null))
        req(!identical(rv$filters, rv$cached_filters))

        logger::log_info("mod_filter_reactivity_server: update {dataset} filter condtion")
        rv$filter_cond <- filters_to_cond(rv$filters)
        rv$cached_filter_list <- filters()
      },
      priority = 920
    )

    observe({
      req(rv$filters)
      req(rv$filter_cond)

      filt_update <- isTRUE(unique(map_lgl(
        names(rv$filters),
        \(x) identical(rv$filters[[x]], levels(unique(df()[[dataset]][[toupper(x)]])))
      )))

      if (!is.null(rv$cached_filters) &&
        length(rv$filters) > length(rv$cached_filters)) {
        req(filt_update)
        logger::log_info("mod_filter_reactivity_server: triggering report (filters added)")
        rv$trig_report <- rv$trig_report + 1
      } else if (!is.null(rv$cached_filters) &&
        length(rv$filters) < length(rv$cached_filters)) {
        if (isTRUE(filt_update)) {
          trig_stop <- FALSE
        } else {
          trig_stop <- any(unique(map_lgl(
            names(rv$filters), \(x) identical(rv$filters[[x]], rv$cached_filters[[x]])
          )))
        }
        req(!trig_stop)
        logger::log_info("mod_filter_reactivity_server: triggering report (filters removed)")
        rv$trig_report <- rv$trig_report + 1
      } else if (!is.null(rv$cached_filters) &&
        !identical(names(rv$filters), names(rv$cached_filters))) {
        req(filt_update)
        logger::log_info("mod_filter_reactivity_server: triggering report (filters replaced)")
        rv$trig_report <- rv$trig_report + 1
      }

      rv$cached_filters <- rv$filters
    }) |>
      bindEvent(rv$filter_cond)

    return(list(
      filter_cond = reactive(rv$filter_cond),
      trig_report = reactive(rv$trig_report)
    ))
  })
}
