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
  tagList(box(
    id = ns("box_adxx_param"),
    title = tags$strong("Preview Data"),
    maximizable = TRUE,
    width = 12,
    div(
      shinycssloaders::withSpinner(reactable::reactableOutput(ns("print_dat")),
        color = "#3BACB6"
      ),
      style = "overflow-x: scroll; overflow-y: scroll;"
    )
  ))
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
        `Name` = str_sub(names(df()), start = 2),
        `N_Rows` = map(df(), \(x) nrow(x)),
        `N_Cols` = map(df(), \(x) ncol(x))
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

    output$print_dat <- reactable::renderReactable({
      req(prev_data())
      prev_data()
    })
  })
}
