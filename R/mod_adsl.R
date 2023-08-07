#' adsl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_adsl_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabBox(
      id = ns("adsl_tabs"),
      type = "pills",
      width = 12,
      collapsible = FALSE,
      tabPanel(
        "Baseline Characteristics",
        mod_adsl_display_ui(ns("adsl_display_1"))
      ),
      tabPanel(
        "Disposition Summary",
        mod_disposition_ui(ns("disposition_1"))
      )
    )
  )
}

#' adsl Server Functions
#'
#' @noRd
mod_adsl_server <- function(id, adsl) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_adsl_display_server("adsl_display_1",
      adsl = adsl
    )

    mod_disposition_server("disposition_1",
      adsl = adsl
    )
  })
}
