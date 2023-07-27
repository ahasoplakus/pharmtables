#' adae_global UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_adae_global_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabBox(
      id = ns("ae_tabs"),
      type = "pills",
      width = 12,
      collapsible = FALSE,
      tabPanel(
        "Adverse Events Summary",
        mod_adae_summary_ui(ns("adae_summary_1"))
      ),
      tabPanel(
        "Adverse Events Summary by Body System or Organ Class",
        mod_adxx_bodsys_ui(ns("adae_bodsys_1"))
      ),
      tabPanel(
        "Adverse Events Summary by Toxicity/Severity",
        mod_adae_sev_tox_ui(ns("adae_sev_tox_1"))
      )
    )
  )
}

#' adae_global Server Functions
#'
#' @noRd
mod_adae_global_server <- function(id, dataset, df_out, adsl, filters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mod_adae_summary_server(
      "adae_summary_1",
      dataset = dataset,
      df_out = df_out,
      adsl = adsl,
      filters = filters()
    )

    mod_adxx_bodsys_server(
      "adae_bodsys_1",
      dataset = dataset,
      df_out = df_out,
      adsl = adsl,
      filters = filters()
    )

    mod_adae_sev_tox_server(
      "adae_sev_tox_1",
      dataset = dataset,
      df_out = df_out,
      adsl = adsl,
      filters = filters()
    )
  })
}
