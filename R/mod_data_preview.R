#' data_preview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_preview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      shinycssloaders::withSpinner(reactable::reactableOutput(ns("print_dat")),
        color = "#3BACB6"
      ),
      style = "overflow-x: scroll; overflow-y: scroll; border-style: outset;"
    ),
    uiOutput(ns("data_in")),
    div(verbatimTextOutput(ns("data_str")), style = "border-style: outset;")
  )
}

#' data_preview Server Functions
#'
#' @noRd
mod_data_preview_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    prev_data <- reactive({
      req(df())
      logger::log_info("mod_data_preview_server: preview data")

      react_df <- tibble::tibble(
        `Data` = names(df()),
        `Dimension` = map(df(), \(x) paste0(dim(x)[1], " (Rows), ", dim(x)[2], " (Columns)"))
      )
      reactable::reactable(
        react_df,
        filterable = TRUE,
        bordered = TRUE,
        striped = TRUE,
        highlight = TRUE,
        defaultPageSize = 10,
        paginationType = "jump",
        details = function(rowNum) {
          sub_df <- df()[[rowNum]]
          div(
            style = "padding: 1rem",
            reactable::reactable(
              sub_df,
              columns = list(USUBJID = reactable::colDef(sticky = "left")),
              filterable = TRUE,
              bordered = TRUE,
              striped = TRUE,
              highlight = TRUE,
              defaultPageSize = 5,
              paginationType = "jump"
            )
          )
        }
      )
    })

    output$data_in <- renderUI({
      req(df())
      req(prev_data())
      div(
        selectInput(
          ns("data_up"),
          label = "View Dataset Structure",
          choices = names(df()),
          selected = "adsl",
          multiple = FALSE
        ),
        style = "padding-top: 2vh;"
      )
    })

    output$data_str <- renderPrint({
      req(input$data_up)
      str(df()[[input$data_up]])
    })

    output$print_dat <- reactable::renderReactable({
      req(prev_data())
      prev_data()
    })
  })
}
