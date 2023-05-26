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
          img(src = "www/logo.png",
              style = "height:45px; width:40px"),
          "clinTables"
        ),
        status = "white",
        border = TRUE,
        skin = "light"
      ),
      sidebar = dashboardSidebar(
        skin = "light",
        status = "info",
        sidebarMenu(id = "sidebarmenu",
                    # add global filters module ui here
                    mod_global_filters_ui("global_filters_1"))
      ),
      body = dashboardBody(
        tabBox(
          id = "tabcard",
          type = "pills",
          width = 12,
          collapsible = FALSE,
          tabPanel("Study Setup",
                   mod_data_read_ui("data_read_1")),
          tabPanel(
            "Demographics",
            fluidRow(
              mod_process_adsl_ui("process_adsl_1"),
              mod_adsl_display_ui("adsl_display_1")
            )
          ),
          tabPanel(
            "Adverse Events",
            mod_adae_global_ui("adae_global_1")
          ),
          tabPanel(
            "Medical History",
            mod_adxx_bodsys_ui("admh_bodsys_1",
                               title = "Summary of Medical History By Body System Class")
          ),
          tabPanel(
            "Concomitant Medications",
            mod_adxx_bodsys_ui("adcm_bodsys_1",
                               title = "Summary of Concomitant Medications by Categories")
          )
        )
      ),
      dark = NULL,
      preloader =  list(html = spin_ball(), color = "#fff")
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

  tags$head(
    favicon(),
    bundle_resources(path = app_sys("app/www"),
                     app_title = "clinTables"),
    shinyjs::useShinyjs()
  )
}
