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
  tagList(
    fluidRow(column(
      width = 4,
      shinyWidgets::prettySwitch(
        ns("def_data"),
        label = "Load Default Data (random.cdisc.data)",
        value = FALSE,
        status = "info",
        inline = TRUE,
        fill = TRUE,
        slim = TRUE
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
    )),
    fluidRow(column(width = 1, uiOutput(
      ns("glimpse_dat")
    )),
    column(
      width = 3,
      div(actionButton(ns("apply"), "Run Application"),
          style = "padding-bottom: 30px; text-align: right;")
    )),
    box(
      id = ns("box_preview"),
      width = 12,
      maximizable = TRUE,
      collapsible = TRUE,
      collapsed = FALSE,
      div(withSpinner(reactable::reactableOutput(ns("print_dat")), type = 6, color = "#3BACB6"),
          style = "overflow-x: scroll; overflow-y: scroll;")
    )
  )
}

#' data_read Server Functions
#'
#' @noRd
mod_data_read_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      data_list = character(0),
      df = NULL,
      trig_reset = 0,
      upload_state = "stale"
    )

    observe({
      shinyjs::enable("upload")
      shinyjs::runjs(
        "$('#data_read_1-upload').parent().removeClass('btn-disabled').addClass('btn-default');"
      )
      req(isTRUE(input$def_data))
      logger::log_info("mod_data_read_server: reset fileinput")
      shinyjs::reset("upload")
      shinyjs::disable("upload")
      shinyjs::runjs(
        "$('#data_read_1-upload').parent().removeClass('btn-default').addClass('btn-disabled');"
      )
      show_toast(
        title = "Data uploaded from package system folder",
        text = "Default datasets have been loaded from random.cdisc.data",
        type = "success",
        position = "center",
        width = "600px"
      )
      if (!is.null(input$upload)) {
        rv$upload <- purrr::list_assign(input$upload, name = NULL)
      }
      rv$upload_state <- "refresh"
      rv$trig_reset <- rv$trig_reset + 1
    }, priority = 1000) |>
      bindEvent(input$def_data)

    observe({
      req(input$upload)
      logger::log_info("mod_data_read_server: uploading data")
      rv$upload <- input$upload
      rv$upload_state <- "stale"
    }) |>
      bindEvent(input$upload)

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
        logger::log_info(
          "mod_data_read_server: data read complete from system folder with {nrow(rv$df[[1]])} rows"
        )
      } else {
        rv$data_list <- str_remove_all(rv$upload$name, ".RDS")
        if (!identical(rv$data_list, character(0))) {
          rv$df <- map(rv$upload$datapath, readRDS) |>
            set_names(rv$data_list)
          logger::log_info("mod_data_read_server: data read complete with {nrow(rv$df[[1]])} rows")
        } else {
          rv$df <- NULL
          rv$trig_reset <- rv$trig_reset + 1
          logger::log_info("mod_data_read_server: no data has been read yet")
        }
      }
    }, priority = 999) |>
      bindEvent(list(rv$upload, input$def_data))

    output$glimpse_dat <- renderUI({
      req(rv$df)
      shinyWidgets::prettySwitch(
        ns("glimpse"),
        label = "Preview data",
        value = TRUE,
        status = "info",
        inline = TRUE,
        fill = TRUE,
        slim = TRUE
      )
    })

    output$print_dat <- reactable::renderReactable({
      req(rv$df)
      req(isTRUE(input$glimpse))
      source <- "Local"
      if (is.null(rv$upload$name))
        source <- "random.cdisc.data"
      df <- tibble::tibble(
        `Name` = names(rv$df),
        `N_Rows` = map(rv$df, \(x) nrow(x)),
        `Colnames` = map(rv$df, \(x) names(x)),
        `Source` = source
      )
      reactable::reactable(
        df,
        filterable = TRUE,
        bordered = TRUE,
        striped = TRUE,
        highlight = TRUE,
        columns = list(
          `Name` = reactable::colDef(minWidth = 50),
          # 50% width, 200px minimum
          `N_Rows` = reactable::colDef(minWidth = 50),
          # 25% width, 100px minimum
          `Colnames` = reactable::colDef(minWidth = 250),
          # 25% width, 100px minimum
          `Source` = reactable::colDef(minWidth = 50)
        ),
        details = function(rowNum) {
          sub_df <- rv$df[[rowNum]]
          div(
            style = "padding: 1rem",
            reactable::reactable(
              sub_df,
              columns = list(USUBJID = reactable::colDef(sticky = "left")),
              filterable = TRUE,
              bordered = TRUE,
              striped = TRUE,
              highlight = TRUE
            )
          )
        }
      )
    })

    read_df <- reactive({
      if (!is.null(rv$df) && rv$upload_state == "refresh") {
        rv$upload_state <- "stale"
        return(NULL)
      }
      req(rv$upload_state == "stale")
      if (is.null(rv$df) && rv$trig_reset > 1) {
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

      rv$df
    }) |>
      bindEvent(list(input$apply, rv$trig_reset), ignoreNULL = TRUE)

    return(list(df_read = read_df))
  })
}
