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
    div(
      uiOutput(ns("valid_error"))
    ),
    div(
      id = ns("list_dat"),
      reactable::reactableOutput(ns("print_dom")),
      style = "overflow-x: scroll; overflow-y: scroll; padding-top: 2vh;"
    ),
    div(
      id = ns("int_str"),
      verbatimTextOutput(ns("data_str")),
      style = "padding-top: 2vh;"
    )
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
        selection = "single"
      )
    })

    output$print_dat <- reactable::renderReactable({
      req(prev_data())
      prev_data()
    })

    selected <- reactive(reactable::getReactableState("print_dat", "selected"))

    output$data_str <- renderPrint({
      req(selected())
      utils::str(df()[[selected()]])
    })

    observe({
      if (length(selected()) > 0) {
        show("list_dat")
        show("int_str")
      } else {
        hide("list_dat")
        hide("int_str")
      }
    })

    output$print_dom <- reactable::renderReactable({
      req(selected())

      reactable::reactable(
        df()[[selected()]],
        columns = list(USUBJID = reactable::colDef(sticky = "left")),
        filterable = TRUE,
        bordered = TRUE,
        striped = TRUE,
        highlight = TRUE,
        defaultPageSize = 5,
        paginationType = "jump"
      )
    })
  })
}
