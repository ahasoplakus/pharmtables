#' lab_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bds_analysis_ui <-
  function(id,
           tab_title = "",
           title = "",
           domain = "ADLB",
           logo = "flask-vial") {
    ns <- NS(id)
    tagList(tabBox(
      id = ns("bds_tabs"),
      type = "pills",
      width = 12,
      collapsible = FALSE,
      tabPanel(
        tab_title,
        mod_adxx_param_ui(
          ns("bds_param_1"),
          title = title,
          domain = domain,
          logo = logo
        )
      ),
      tabPanel(
        "Shift Table",
        mod_bds_shift_ui(
          ns("bds_shift_1"),
          title = paste0(domain, " Shift Table"),
          domain = domain,
          logo = logo
        )
      )
    ))
  }

#' lab_analysis Server Functions
#'
#' @noRd
mod_bds_analysis_server <-
  function(id, dataset, df_out, adsl, filters) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      mod_adxx_param_server(
        "bds_param_1",
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
