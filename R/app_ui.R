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
      header = dashboard_header(),
      sidebar = dashboard_sidebar(),
      body = dashboard_body(),
      fullscreen = TRUE,
      dark = NULL,
      help = NULL,
      scrollToTop = TRUE,
      preloader = list(html = tagList(
        waiter::spin_dots(),
        ""
      ), color = "#526D82")
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
    # favicon(), # nolint
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "pharmtables"
    ),
    useShinyjs()
  )
}
