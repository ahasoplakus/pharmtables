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
      "PHARMTABLES"
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
        title = "Home"
      ),
      tooltip(
        navbarTab(
          tabName = "Tab2",
          text = tags$span(icon("id-card"), "ADSL")
        ),
        title = "Demographics"
      ),
      tooltip(
        navbarTab(
          tabName = "Tab3",
          text = tags$span(icon("head-side-cough"), "ADAE")
        ),
        title = "Adverse Events"
      ),
      tooltip(
        navbarTab(
          tabName = "Tab4",
          text = tags$span(icon("file-medical"), "ADMH")
        ),
        title = "Medical History"
      ),
      tooltip(
        navbarTab(
          tabName = "Tab5",
          text = tags$span(icon("capsules"), "ADCM")
        ),
        title = "Concomitant Medications"
      ),
      tooltip(
        navbarTab(
          tabName = "Tab6",
          text = tags$span(icon("stethoscope"), "ADVS")
        ),
        title = "Vital Signs"
      ),
      tooltip(
        navbarTab(
          tabName = "Tab7",
          text = tags$span(icon("flask-vial"), "ADLB")
        ),
        title = "Laboratory Analysis"
      ),
      tooltip(
        navbarTab(
          tabName = "Tab8",
          text = tags$span(icon("heart-pulse"), "ADEG")
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
        title = tags$span(icon("id-card"), tags$strong("ADSL (Subject-Level)")),
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
        fluidRow(
          mod_process_adsl_ui("process_adsl_1"),
          mod_adsl_ui("adsl_1")
        )
      ),
      tabItem(
        tabName = "Tab3",
        mod_adae_global_ui("adae_global_1")
      ),
      tabItem(
        tabName = "Tab4",
        mod_occ_summary_ui("admh_bodsys_1",
          domain = "ADMH",
          logo = "file-medical"
        )
      ),
      tabItem(
        tabName = "Tab5",
        mod_occ_summary_ui("adcm_bodsys_1",
          domain = "ADCM",
          logo = "capsules"
        )
      ),
      tabItem(
        tabName = "Tab6",
        mod_bds_analysis_ui("vitals_analysis_1",
          tab_title = "Vital Signs Tests Summary",
          domain = "ADVS",
          logo = "stethoscope"
        )
      ),
      tabItem(
        tabName = "Tab7",
        mod_bds_analysis_ui("lab_analysis_1",
          tab_title = "Laboratory Tests Summary"
        )
      ),
      tabItem(
        tabName = "Tab8",
        mod_bds_analysis_ui("ecg_analysis_1",
          tab_title = "ECG Tests Summary",
          domain = "ADEG",
          logo = "heart-pulse"
        )
      )
    )
  )
}

#' Apply flextable styling
#'
#' @param ft flextabe object
#'
#' @return Flextable with applied style
#'
#' @noRd
#'
table_options <- function(ft) {
  ft |>
    flextable::autofit() |>
    flextable::align(align = "center", part = "header") |>
    flextable::align(align = "left", j = 1, part = "header") |>
    flextable::style(
      pr_t = officer::fp_text(font.size = 12),
      part = "body"
    ) |>
    flextable::style(
      pr_t = officer::fp_text(
        font.size = 12,
        bold = TRUE,
      ),
      part = "header"
    ) |>
    flextable::border_outer(
      border = officer::fp_border(color = "#343a40"),
      part = "all"
    )
}
