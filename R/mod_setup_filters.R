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
  tagList(
    fluidRow(
      column(
        width = 3,
        selectizeInput(
          ns("adsl_var"),
          "Study Filters",
          choices = NULL,
          selected = NULL,
          width = 300,
          options = list(maxItems = 8)
        )
      ),
      column(
        width = 3,
        selectizeInput(
          ns("adae_var"),
          "Adverse Events Filters",
          choices = NULL,
          selected = NULL,
          width = 300,
          options = list(maxItems = 4)
        )
      )
    )
  )
}

#' setup_filters Server Functions
#'
#' @noRd
mod_setup_filters_server <- function(id, load_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(load_data$cadsl)

      logger::log_info(
        "mod_setup_filters_server: setup adsl filters"
      )

      choices <-
        names(select(load_data$cadsl, !ends_with("FL")))
      selected <-
        intersect(c("SEX", "RACE", "ETHNIC", "COUNTRY", "AGE", "SITEID", "USUBJID"), choices)

      updateSelectizeInput(
        session,
        "adsl_var",
        "Study Filters",
        choices = choices,
        selected = selected,
        options = list(maxItems = 8)
      )
    })

    observe({
      req(load_data$cadsl)
      req(load_data$cadae)

      logger::log_info(
        "mod_setup_filters_server: setup adae filters"
      )

      choices <- setdiff(names(load_data$cadae), names(load_data$cadsl))
      selected <- NULL

      updateSelectizeInput(
        session,
        "adae_var",
        "Adverse Events Filters",
        choices = choices,
        selected = selected,
        options = list(maxItems = 4)
      )
    })

    return(list(adsl_filt = reactive(input$adsl_var),
                adae_filt = reactive(input$adae_var)))
  })
}
