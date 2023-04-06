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
mod_global_filters_ui <- function(id){
  ns <- NS(id)
  tagList(
    menuItem(
      text = "Study Filters",
      pickerInput(
        ns("pop_fil"),
        "",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = NULL,
        choicesOpt = NULL
      ),
      actionButton(
        ns("apply"),
        "Apply"
      )
    )
  )
}

#' global_filters Server Functions
#'
#' @noRd
mod_global_filters_server <- function(id, dataset, load_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      req(load_data())
      logger::log_info("mod_global_filters_server: update filters")

      race <- levels(unique(load_data()[[dataset]][["RACE"]]))

      updatePickerInput(
        session = session,
        inputId = "pop_fil",
        label = "Race",
        choices = race,
        selected = race,
        options = list(
          `actions-box` = TRUE, size = 10
        ),
        choicesOpt = list(
          content = stringr::str_trunc(race, width = 20)
        )
      )
    })

    filters <- reactive({
      logger::log_info("mod_global_filters_server: store filters")
      input$pop_fil
    })

    return(list(filters = filters,
                apply = reactive(input$apply)))

  })
}

## To be copied in the UI
# mod_global_filters_ui("global_filters_1")

## To be copied in the server
# mod_global_filters_server("global_filters_1")
