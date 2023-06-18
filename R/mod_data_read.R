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
      prettySwitch(
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
    fluidRow(
      column(width = 1),
      column(
        width = 3,
        div(actionButton(ns("apply"), "Run"),
          style = "padding-bottom: 30px; text-align: right;"
        )
      )
    ),
    tabBox(
      id = ns("box_preview"),
      type = "pills",
      width = 12,
      collapsible = FALSE,
      tabPanel(
        "Preview Data",
        mod_data_preview_ui(ns("data_preview_1"))
      ),
      tabPanel(
        "Setup Filters",
        mod_setup_filters_ui(ns("setup_filters_1"))
      )
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
      upload_state = "stale",
      setup_filters = NULL
    )

    observe(
      {
        enable("upload")
        runjs(
          "$('#data_read_1-upload').parent().removeClass('btn-disabled').addClass('btn-default');"
        )
        req(isTRUE(input$def_data))
        logger::log_info("mod_data_read_server: reset fileinput")
        reset("upload")
        disable("upload")
        runjs(
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
          rv$upload <- list_assign(input$upload, name = NULL)
        }
        rv$upload_state <- "refresh"
        rv$trig_reset <- rv$trig_reset + 1
      },
      priority = 1000
    ) |>
      bindEvent(input$def_data)

    observe({
      req(input$upload)
      logger::log_info("mod_data_read_server: uploading data")
      rv$upload <- input$upload
      rv$upload_state <- "stale"
    }) |>
      bindEvent(input$upload)

    observe(
      {
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
            "mod_data_read_server: data read complete from system folder with {nrow(rv$df[[1]])} rows" # nolint
          )
        } else {
          rv$data_list <- str_remove_all(rv$upload$name, ".RDS")
          if (!identical(rv$data_list, character(0))) {
            rv$df <- map(rv$upload$datapath, readRDS) |>
              set_names(rv$data_list)
            logger::log_info("mod_data_read_server: data read complete with {nrow(rv$df[[1]])} rows") # nolint
          } else {
            rv$df <- NULL
            rv$trig_reset <- rv$trig_reset + 1
            logger::log_info("mod_data_read_server: no data has been read yet")
          }
        }
      },
      priority = 999
    ) |>
      bindEvent(list(rv$upload, input$def_data))

    mod_data_preview_server(
      "data_preview_1",
      eventReactive(rv$df, rv$df)
    )

    observe(
      {
        req(!is.null(rv$df[["cadsl"]]))
        rv$setup_filters <- mod_setup_filters_server(
          "setup_filters_1",
          rv$df
        )
      },
      priority = 990
    ) |>
      bindEvent(rv$df)

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

      map(rv$df, \(x) df_explicit_na(x))
    }) |>
      bindEvent(list(input$apply, rv$trig_reset), ignoreNULL = TRUE)

    observe({
      req(rv$setup_filters$adsl_filt())
      rv$all_filt <- list(
        rv$setup_filters$adsl_filt(),
        rv$setup_filters$adae_filt(),
        rv$setup_filters$admh_filt(),
        rv$setup_filters$adcm_filt()
      )
    }) |> bindEvent(input$apply)

    observe({
      req(read_df())
      req(rv$setup_filters$adsl_filt())

      if (identical(
        list(
          rv$setup_filters$adsl_filt(),
          rv$setup_filters$adae_filt(),
          rv$setup_filters$admh_filt(),
          rv$setup_filters$adcm_filt()
        ),
        rv$all_filt
      )) {
        disable("apply")
      } else {
        enable("apply")
      }
    })

    observe({
      req(read_df())
      updateActionButton(session, "apply", label = "Reload")
    }) |>
      bindEvent(input$apply, once = TRUE)

    return(list(
      df_read = read_df,
      study_filters = eventReactive(
        input$apply,
        rv$setup_filters$adsl_filt()
      ),
      adae_filters = eventReactive(
        input$apply,
        rv$setup_filters$adae_filt()
      ),
      admh_filters = eventReactive(
        input$apply,
        rv$setup_filters$admh_filt()
      ),
      adcm_filters = eventReactive(
        input$apply,
        rv$setup_filters$adcm_filt()
      )
    ))
  })
}
