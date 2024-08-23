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
    tabBox(
      id = ns("about"),
      title = "",
      type = "pills",
      collapsible = FALSE,
      width = 12,
      tabPanel(
        title = "About",
        includeMarkdown(app_sys("about.md"))
      ),
      tabPanel(
        title = "Setup",
        fluidRow(
          column(width = 3, offset = 1),
          column(
            width = 8,
            fluidRow(
              checkboxInput(
                ns("def_data"),
                label = "Use Mock Datasets (pharmaverseadam)",
                value = FALSE,
                width = "400px"
              )
            ),
            fluidRow(
              fileInput(
                ns("upload"),
                HTML("&nbsp;&nbsp;Upload ADaM(s)"),
                multiple = TRUE,
                accept = c(".RDS", ".sas7bdat"),
                width = "49%",
                buttonLabel = tags$span(icon("upload")),
                placeholder = "No file selected",
                capture = NULL
              )
            )
          ),
          style = "height: 135px;"
        ),
        fluidRow(
          column(width = 3, offset = 1),
          column(
            width = 8,
            mod_setup_filters_ui(ns("setup_filters_1"))
          )
        ),
        fluidRow(
          column(width = 3, offset = 1),
          column(
            width = 4,
            div(actionButton(ns("apply"), "Launch", icon = icon("rocket")),
              style = "justify-content: center; display: flex;"
            )
          )
        )
      ),
      tabPanel(
        title = "Preview",
        mod_data_preview_ui(ns("data_preview_1"))
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
      upload_state = "init",
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
        if (!is.null(input$upload)) {
          rv$upload <- list_assign(input$upload, name = NULL)
        }
        rv$upload_state <- "refresh"
      },
      priority = 1000
    ) |>
      bindEvent(input$def_data)

    observe({
      req(input$upload)
      logger::log_info("mod_data_read_server: uploading data")
      rv$upload <- input$upload
      rv$upload_state <- "init"
    }) |>
      bindEvent(input$upload)

    observe(
      {
        logger::log_info("mod_data_read_server: data_list")

        if (isTRUE(input$def_data)) {
          rv$data_list <-
            str_remove_all(list.files(app_sys("extdata")), ".RDS")
          rv$df <- rv$data_list |>
            map(\(x) {
              readRDS(paste0(
                app_sys("extdata"), "/", x, ".RDS"
              ))
            }) |>
            set_names(rv$data_list)
          disable("def_data")
          logger::log_info(
            "mod_data_read_server: data read complete from system folder with {nrow(rv$df[[1]])} rows" # nolint
          )
        } else {
          rv$data_list <- str_remove_all(rv$upload$name, ".RDS|.sas7bdat")
          if (!identical(rv$data_list, character(0))) {
            disable("def_data")
            rv$df <- read_data_list(rv$upload$datapath, rv$upload$name, rv$data_list)
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

    rv$setup_filters <- mod_setup_filters_server(
      "setup_filters_1",
      eventReactive(rv$df, map(rv$df, \(x) drop_missing_cols(x)))
    )

    read_df <- reactive({
      if (is.null(rv$df) && rv$trig_reset > 1) {
        show_toast(
          title = "No data to display",
          text = "Please upload data",
          type = "error",
          position = "center",
          width = "600px"
        )
      }

      if (!is.null(rv$df) && is.null(rv$df[["adsl"]])) {
        show_toast(
          title = "ADSL dataset is required",
          text = "Please upload ADSL data",
          type = "error",
          position = "center",
          width = "600px"
        )
      }
      logger::log_info("mod_data_read_server: sending data")

      map(rv$df, \(x) drop_missing_cols(x))
    }) |>
      bindEvent(list(input$apply, rv$trig_reset), ignoreNULL = TRUE)

    observe({
      req(rv$setup_filters$adsl_filt())
      rv$all_filt <- c(map(rv$setup_filters, \(x) x()), rv$df)
      disable("def_data")
    }) |> bindEvent(list(input$apply, rv$df), ignoreNULL = FALSE)

    observe({
      req(read_df())
      req(rv$setup_filters$adsl_filt())

      if (identical(c(map(rv$setup_filters, \(x) x()), rv$df), rv$all_filt)) {
        disable("apply")
      } else {
        enable("apply")
      }
    })

    observe({
      req(read_df())
      updateActionButton(session, "apply", label = "Reload", icon = icon("rotate"))
    }) |>
      bindEvent(input$apply, once = TRUE)

    observe({
      if (is.null(rv$df)) {
        disable("apply")
      } else {
        enable("apply")
        disable("upload")
        runjs(
          "$('#data_read_1-upload').parent().removeClass('btn-default').addClass('btn-disabled');"
        )
        show_toast(
          title = "Datasets have been loaded",
          text = "Hit the launch button to start",
          timer = 2000,
          type = "success",
          position = "bottom-end",
          width = "40vw"
        )
      }
    })

    observe({
      toggleState(
        selector = "#data_read_1-about > li:nth-child(3)",
        condition = !is.null(rv$df),
        asis = TRUE
      )
    })

    mod_data_preview_server(
      "data_preview_1",
      eventReactive(rv$df, rv$df)
    )

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
      ),
      advs_filters = eventReactive(
        input$apply,
        rv$setup_filters$advs_filt()
      ),
      adlb_filters = eventReactive(
        input$apply,
        rv$setup_filters$adlb_filt()
      ),
      adeg_filters = eventReactive(
        input$apply,
        rv$setup_filters$adeg_filt()
      )
    ))
  })
}
