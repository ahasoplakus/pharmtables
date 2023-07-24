#' lab_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_lab_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(tabBox(
    id = ns("lab_tabs"),
    type = "pills",
    width = 12,
    collapsible = FALSE,
    tabPanel(
      "Summary of Laboratory Tests",
      mod_adxx_param_ui(
        ns("adlb_param_1"),
        title = "Summary of Laboratory Tests by Parameter and Visit",
        domain = "ADLB",
        logo = "flask-vial"
      )
    ),
    tabPanel(
      "Laboratory Tests Shift Table",
      mod_bds_shift_ui(ns("bds_shift_1"))
    )
  ))
}

#' lab_analysis Server Functions
#'
#' @noRd
mod_lab_analysis_server <- function(id, dataset, df_out, adsl, filters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_adxx_param_server(
      "adlb_param_1",
      dataset = dataset,
      df_out = df_out,
      adsl = adsl,
      filters = filters()
    )

    mod_bds_shift_server(
      "bds_shift_1",
      dataset = dataset,
      df_out = df_out,
      adsl = adsl,
      filters = filters()
    )
  })
}
