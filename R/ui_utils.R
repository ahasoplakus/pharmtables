#' Dashboard Header UI
#'
#' @return html component of dashboard header
#'
#' @noRd
dashboard_header <- function() {
  dashboardHeader(
    title = div(
      id = "logo-id",
      img(
        src = "www/logo.png",
        style = "height:45px; width:40px"
      ),
      "clinTables"
    ),
    status = "white",
    border = TRUE,
    skin = "light",
    bs4Dash::navbarMenu(
      id = "navmenu",
      tooltip(
        navbarTab(
          tabName = "Tab1",
          text = tags$span(icon("home"), "")
        ),
        title = "Home",
        placement = "bottom"
      ),
      tooltip(
        navbarTab(
          tabName = "Tab2",
          text = tags$span(icon("eye"), "")
        ),
        title = "Preview Data"
      ),
      tooltip(
        navbarTab(
          tabName = "Tab3",
          text = tags$span(icon("id-card"), "")
        ),
        title = "Demographics"
      ),
      tooltip(
        navbarTab(
          tabName = "Tab4",
          text = tags$span(icon("head-side-cough"), "")
        ),
        title = "Adverse Events"
      ),
      tooltip(
        navbarTab(
          tabName = "Tab5",
          text = tags$span(icon("file-medical"), "")
        ),
        title = "Medical History"
      ),
      tooltip(
        navbarTab(
          tabName = "Tab6",
          text = tags$span(icon("capsules"), "")
        ),
        title = "Concomitant Medications"
      ),
      tooltip(
        navbarTab(
          tabName = "Tab7",
          text = tags$span(icon("stethoscope"), "")
        ),
        title = "Vital Signs"
      ),
      tooltip(
        navbarTab(
          tabName = "Tab8",
          text = tags$span(icon("flask-vial"), "")
        ),
        title = "Laboratory Analysis"
      ),
      tooltip(
        navbarTab(
          tabName = "Tab9",
          text = tags$span(icon("heart-pulse"), "")
        ),
        title = "ECG Analysis"
      )
    )
  )
}

#' Dashboard Sidebar UI
#'
#' @return html component of sidebar
#' @noRd
dashboard_sidebar <- function() {
  dashboardSidebar(
    status = "info",
    width = "300px",
    minified = FALSE,
    accordion(
      id = "acc_st_filt",
      accordionItem(
        title = tags$span(icon("id-card"), tags$strong("ADSL Filters")),
        collapsed = FALSE,
        mod_adsl_filters_ui("adsl_filters_1")
      )
    )
  )
}

#' Dashboard Body UI
#'
#' @return html component of dashboard body
#'
#' @noRd
dashboard_body <- function() {
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Tab1",
        mod_data_read_ui("data_read_1")
      ),
      tabItem(
        tabName = "Tab2",
        mod_data_preview_ui("data_preview_1")
      ),
      tabItem(
        tabName = "Tab3",
        fluidRow(
          mod_process_adsl_ui("process_adsl_1"),
          mod_adsl_display_ui("adsl_display_1")
        )
      ),
      tabItem(
        tabName = "Tab4",
        mod_adae_global_ui("adae_global_1")
      ),
      tabItem(
        tabName = "Tab5",
        mod_adxx_bodsys_ui("admh_bodsys_1",
          title = "Summary of Medical History By Body System or Organ Class and Dictionary-Derived
          Term",
          domain = "ADMH",
          logo = "file-medical"
        )
      ),
      tabItem(
        tabName = "Tab6",
        mod_adxx_bodsys_ui("adcm_bodsys_1",
          title = "Summary of Concomitant Medications by Medication Class and Standardized
          Medication Name",
          domain = "ADCM",
          logo = "capsules"
        )
      ),
      tabItem(
        tabName = "Tab7",
        mod_bds_analysis_ui("vitals_analysis_1",
          tab_title = "Vital Signs Tests Summary",
          summ_title = "Summary of Vital Signs Tests by Parameter, Analysis Value and Visit",
          shift_title = "Table to display the shift at post dose for Vital Signs",
          domain = "ADVS",
          logo = "stethoscope"
        )
      ),
      tabItem(
        tabName = "Tab8",
        mod_bds_analysis_ui("lab_analysis_1",
          tab_title = "Laboratory Tests Summary",
          summ_title = "Summary of Laboratory Tests by Parameter, Analysis Value and Visit",
          shift_title = "Table to display the shift at post dose for Laboratory Tests"
        )
      ),
      tabItem(
        tabName = "Tab9",
        mod_bds_analysis_ui("ecg_analysis_1",
          tab_title = "ECG Tests Summary",
          summ_title = "Summary of ECG Tests by Parameter, Analysis Value and Visit",
          shift_title = "Table to display the shift at post dose for ECG Tests",
          domain = "ADEG",
          logo = "heart-pulse"
        )
      )
    )
  )
}
