#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bs4Dash
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      header = dashboardHeader(
        title = "clinTables",
        status = "white",
        border = TRUE,
        skin = "light"
      ),
      sidebar = dashboardSidebar(
        skin = "light",
        status = "info",
        sidebarMenu(
          id = "sidebarmenu",
          # add global filters module ui here
          mod_global_filters_ui("global_filters_1")
        )
      ),
      body = dashboardBody(
        tabBox(
          id = "tabcard",
          type = "pills",
          width = 12,
          collapsible = FALSE,
          tabPanel(
            "Demographics",
            fluidRow(
              mod_data_read_ui("data_read_1"),
              mod_adsl_display_ui("adsl_display_1")
            )
          ),
          tabPanel(
            "Adverse Events"
          )
        )
      ),
      dark = NULL
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
  add_resource_path("www",
                    app_sys("app/www"))

  tags$head(favicon(),
            bundle_resources(path = app_sys("app/www"),
                             app_title = "clinTables"))
  # Add here other external resources
  # for example, you can add shinyalert::useShinyalert()
}
