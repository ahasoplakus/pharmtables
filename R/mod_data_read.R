#' data_read UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_read_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    column(
      width = 4,
      shinyWidgets::prettySwitch(
        ns("def_data"),
        label = "Load Default Data (random.cdisc.data)",
        value = FALSE,
        status = "info",
        inline = TRUE,
        fill = TRUE
      ),
      fileInput(
        ns("upload"),
        "",
        multiple = TRUE,
        accept = ".RDS",
        width = NULL,
        buttonLabel = "Upload...",
        placeholder = "No file selected",
        capture = NULL
      )
    ),
    column(
      width = 2,
      div(actionButton(ns("apply"), "Run Application"), style = "padding-left: 50px; padding-top: 45px;")
    ),
    column(
      width = 6,
      div(uiOutput(ns("glimpse_dat")),
          style = "padding-left: 50px; padding-top: 15px;")
    )
  ),
  fluidRow(column(
    width = 12,
    div(dataTableOutput(ns("print_dat")), style = "overflow-x: scroll;")
  )))
}

#' data_read Server Functions
#'
#' @noRd
mod_data_read_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(data_list = character(0),
                         df = NULL,
                         state = "init",
                         trig_upload = TRUE,
                         trig_null = 0)

    observe({
      logger::log_info("mod_data_read_server: data_list")

      if (isTRUE(input$def_data)) {
        rv$data_list <-
          str_remove_all(list.files(app_sys("extdata")), ".RDS")
        rv$df <- rv$data_list |>
          map(\(x) readRDS(paste0(
            app_sys("extdata"), "/", x, ".RDS"
          ))) |>
          set_names(rv$data_list)
        shinyjs::reset("upload")
        rv$data_list <- character(0)
        logger::log_info("mod_data_read_server: data read complete with {nrow(rv$df[[1]])} rows")
      } else {
        if (isTRUE(rv$trig_upload)) {
          rv$data_list <- str_remove_all(input$upload$name, ".RDS")
        }
        if (!identical(rv$data_list, character(0))) {
          rv$df <- map(input$upload$datapath, readRDS) |>
            set_names(rv$data_list)
          logger::log_info("mod_data_read_server: data read complete with {nrow(rv$df[[1]])} rows")
        } else {
          rv$df <- NULL
          rv$trig_null <- rv$trig_null + 1
          logger::log_info("mod_data_read_server: no data has been read yet")
        }
      }
    }) |>
      bindEvent(list(input$def_data, input$upload))

    observe({
      logger::log_info("mod_data_read_server: update state")
      rv$state <- "reset"
    }) |>
      bindEvent(rv$trig_null, ignoreInit = TRUE)

    output$glimpse_dat <- renderUI({
      req(rv$df)
      selectInput(
        ns("glimpse"),
        "Preview data",
        choices = c("", names(rv$df)),
        selected = "",
        multiple = FALSE,
        width = 300
      )
    })

    output$print_dat <- renderDataTable({
      req(input$glimpse != "")
      req(rv$df)
      datatable(
        rv$df[[input$glimpse]],
        filter = "top",
        options = list(pageLength = 5, autoWidth = TRUE)
        )
    })

    read_df <- reactive({
      if (is.null(rv$df)) {
        show_toast(
          title = "No data to display",
          text = "Please upload data",
          type = "error",
          position = "center",
          width = "600px"
        )
      }
      req(!is.null(rv$df))
      if (is.null(rv$df[["cadsl"]])) {
        show_toast(
          title = "ADSL dataset is required",
          text = "Please upload ADSL data",
          type = "error",
          position = "center",
          width = "600px"
        )
      }
      req(!is.null(rv$df[["cadsl"]]))
      logger::log_info("mod_data_read_server: sending data")

      rv$state <- "init"
      rv$trig_upload <- FALSE
      rv$df
    }) |>
      bindEvent(list(input$apply, rv$state), ignoreInit = TRUE)

    return(read_df)
  })
}
