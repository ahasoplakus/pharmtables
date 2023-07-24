#' vitals_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_vitals_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(tabBox(
    id = ns("vs_tabs"),
    type = "pills",
    width = 12,
    collapsible = FALSE,
    tabPanel(
      "Summary of Vital Signs",
      mod_adxx_param_ui(
        ns("advs_param_1"),
        title = "Summary of Vital Signs by Parameter and Visit",
        domain = "ADVS"
      )
    ),
    tabPanel(
      "Vital Signs Shift Table",
      mod_bds_shift_ui(ns("bds_shift_1"))
    )
  ))
}

#' vitals_analysis Server Functions
#'
#' @noRd
mod_vitals_analysis_server <- function(id, dataset, df_out, adsl, filters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_adxx_param_server(
      "advs_param_1",
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
