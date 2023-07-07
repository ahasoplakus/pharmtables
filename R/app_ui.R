#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bs4Dash dashboardPage dashboardHeader dashboardSidebar
#' sidebarMenu menuItem menuItemOutput renderMenu dashboardBody tabBox box
#' boxSidebar
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      header = dashboardHeader(
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
      ),
      sidebar = dashboardSidebar(
        skin = "light",
        status = "info",
        width = "275px",
        minified = FALSE,
        accordion(
          id = "acc_st_filt",
          accordionItem(
            title = tags$span(icon("user-gear"), tags$strong("ADSL Filters")),
            collapsed = FALSE,
            mod_global_filters_ui("global_filters_1")
          )
        )
      ),
      body = dashboardBody(
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
              title = "Summary of Medical History By Body System Class",
              domain = "ADMH"
            )
          ),
          tabItem(
            tabName = "Tab6",
            mod_adxx_bodsys_ui("adcm_bodsys_1",
              title = "Summary of Concomitant Medications by Categories",
              domain = "ADCM"
            )
          ),
          tabItem(
            tabName = "Tab7",
            mod_adxx_param_ui("advs_param_1",
              title = "Vital Signs: Summary Table for Analysis Value, Change from Baseline",
              domain = "ADVS"
            )
          ),
          tabItem(
            tabName = "Tab8",
            mod_adxx_param_ui("adlb_param_1",
              title = "Laboratory Analysis: Summary Table for Analysis Value, Change from Baseline",
              domain = "ADLB"
            )
          ),
          tabItem(
            tabName = "Tab9",
            mod_adxx_param_ui("adeg_param_1",
              title = "ECG: Summary Table for Analysis Value, Change from Baseline",
              domain = "ADEG"
            )
          )
        )
      ),
      fullscreen = TRUE,
      dark = NULL,
      help = NULL,
      scrollToTop = TRUE,
      preloader = list(html = tagList(waiter::spin_ball(), "Loading clinTables ..."), color = "#27374D")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "clinTables"
    ),
    useShinyjs()
  )
}
