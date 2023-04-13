#' global_filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput updatePickerInput
mod_global_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(menuItemOutput(ns("glob_filt_ui")))
}

#' global_filters Server Functions
#'
#' @noRd
mod_global_filters_server <- function(id, dataset, load_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$glob_filt_ui <- renderMenu({
      req(load_data())
      logger::log_info("mod_global_filters_server: update filters")

      make_widget <-
        create_widget(c("SEX", "RACE", "ETHNIC", "COUNTRY", "AGE", "SITEID", "USUBJID"),
                  load_data(),
                  dataset,
                  ns)

      menuItem(
        text = "Study Filters",
        create_flag_widget(c("SAFFL", "ITTFL"), ns),
        make_widget[["SEX"]],
        make_widget[["RACE"]],
        make_widget[["ETHNIC"]],
        make_widget[["COUNTRY"]],
        make_widget[["AGE"]],
        make_widget[["SITEID"]],
        make_widget[["USUBJID"]],
        actionButton(ns("apply"), "Apply")
      )
    })

    filters <- reactive({
      logger::log_info("mod_global_filters_server: store filters")
      list(pop = input$pop,
           sex = input$sex,
           race = input$race,
           ethnic = input$ethnic,
           country = input$country,
           age = input$age,
           siteid = input$siteid,
           usubjid = input$usubjid)
    })

    return(list(filters = filters,
                apply = reactive(input$apply)))

  })
}

## To be copied in the UI
# mod_global_filters_ui("global_filters_1")

## To be copied in the server
# mod_global_filters_server("global_filters_1")
