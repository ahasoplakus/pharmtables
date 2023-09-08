#' bds_shift UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bds_shift_ui <- function(id,
                             domain = "ADLB",
                             logo = "flask-vial") {
  ns <- NS(id)
  tagList(
    box(
      id = ns("box_bds_shift"),
      title = uiOutput(ns("table_title")),
      sidebar = boxSidebar(
        id = ns("bds_side_shift"),
        background = "#EFF5F5",
        icon = icon("filter"),
        width = 35,
        div(uiOutput(ns(
          "analysis_flag_ui"
        ))),
        mod_filter_reactivity_ui(
          ns("filter_reactivity_1"),
          domain = domain,
          logo = logo
        ),
        div(
          accordion(
            id = ns("shift_accord"),
            tagAppendAttributes(accordionItem(
              title = tags$span(icon("table-cells"), tags$strong("Table Options")),
              collapsed = FALSE,
              selectInput(
                ns("split_col"),
                "Treatment Variable",
                choices = NULL,
                selected = NULL,
                width = "100vw"
              ),
              selectInput(
                ns("group_var"),
                "Additonal Grouping Variables",
                choices = NULL,
                selected = NULL,
                width = "100vw",
                multiple = TRUE
              )
            ), class = "side_accord")
          ),
          style = "display: flex; justify-content: center;"
        ),
        fluidRow(
          div(
            tagAppendAttributes(actionButton(ns("run"), "Update"),
              class = "side_apply"
            ),
            style = "display: flex; justify-content: center; width: 100vw;"
          )
        )
      ),
      maximizable = TRUE,
      width = 12,
      shinyWidgets::prettySwitch(
        ns("view"),
        label = "Toggle View",
        value = TRUE,
        status = "info",
        inline = TRUE,
        fill = TRUE,
        slim = FALSE
      ),
      div(
        shinycssloaders::withSpinner(mod_dt_table_ui(ns("dt_table_shift")),
          color = "#3BACB6"
        ),
        style = "overflow-x: scroll; height: 100vh;"
      )
    )
  )
}

#' bds_shift Server Functions
#'
#' @noRd
mod_bds_shift_server <- function(id,
                                 dataset,
                                 df_out,
                                 adsl,
                                 filters = reactive(NULL),
                                 pop_fil) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(pop_trigger = FALSE)

    observe({
      req(df_out()[[dataset]])
      if (is.null(filters())) {
        hide("filter_reactivity_1-domain_filters")
      } else {
        show("filter_reactivity_1-domain_filters")
      }
    })

    observe({
      req(adsl())
      req(df_out()[[dataset]])
      anl_flags <-
        names(select(df_out()[[dataset]], starts_with("ANL0")))
      req(length(anl_flags) > 0)
      req(is.null(rv$lyt))

      rv$widget <- tagList(div(accordion(
        id = ns("flag_accord"),
        tagAppendAttributes(accordionItem(
          title = tags$span(
            icon("magnifying-glass-chart"),
            tags$strong("Analysis Flags")
          ),
          collapsed = FALSE,
          create_flag_widget(df_out()[[dataset]], anl_flags, ns, "")
        ), class = "side_accord")
      )))
    })

    output$analysis_flag_ui <- renderUI({
      req(rv$widget)
      rv$widget
    })

    outputOptions(output, "analysis_flag_ui", priority = 970)

    observe({
      req(adsl())
      req(df_out()[[dataset]])
      req(!identical(df_out()[[dataset]], rv$bds_cached))
      logger::log_info("mod_bds_shift_server: updating table options for {dataset}")

      df <- df_out()[[dataset]]

      trt_choices <-
        names(select(
          adsl(),
          setdiff(
            starts_with(
              c("ACT", "ARM", "TRT", "TR0", "TR1", "TR2")
            ),
            ends_with(
              c("DTM", "DUR", "PN", "AN", "DT", "FL")
            )
          )
        ))

      group_choices <- df |>
        select(!ends_with(c(
          "DTM", "DUR", "PN", "AN", "DT", "CD", "TEST", "DY", "SEQ"
        ))) |>
        select(!contains(
          c(
            trt_choices,
            "PARAM",
            "ANRIND",
            "BNRIND",
            "STUDYID",
            "SUBJID",
            "SITEID",
            "FL",
            "AVAL",
            "CHG"
          )
        )) |>
        names()

      updateSelectInput(session,
        "split_col",
        choices = trt_choices,
        selected = trt_choices[1]
      )

      updateSelectInput(session,
        "group_var",
        choices = group_choices,
        selected = NULL
      )

      rv$bds_cached <- df_out()[[dataset]]
    }) |>
      bindEvent(list(adsl(), df_out()[[dataset]]))

    filt_react <-
      mod_filter_reactivity_server(
        "filter_reactivity_1",
        df = reactive({
          req(df_out()[[dataset]])
          df_out()
        }),
        dataset = dataset,
        filters = reactive({
          req(filters())
          filters()
        }),
        trt_var = input$split_col
      )

    observe({
      req(df_out()[[dataset]])
      anl_flags <-
        names(select(df_out()[[dataset]], starts_with("ANL0")))
      if (length(anl_flags) == 0) {
        rv$pop_trigger <- TRUE
      } else {
        req(input$pop)
        rv$pop_trigger <- TRUE
      }
    })

    xx_shift <- reactive({
      req(df_out()[[dataset]])
      req(adsl())
      req(input$split_col)
      req(rv$pop_trigger)

      df_adsl <- adsl() |>
        select(USUBJID, input$split_col) |>
        unique()

      logger::log_info("mod_bds_shift_server: alt_data has {nrow(df_adsl)} rows")

      df <- df_out()[[dataset]]

      trt_label <-
        compact(map(set_names(input$split_col), \(x) obj_label(adsl()[[x]])))
      group_label <- NULL

      if (!is.null(input$group_var)) {
        group_label <-
          compact(map(set_names(input$group_var), \(x) obj_label(df[[x]])))
      }

      if (!is.null(input$pop)) {
        df <- df |>
          filter(.data[[input$pop]] == "Y")
      }

      df <- df |>
        filter(USUBJID %in% unique(df_adsl$USUBJID))

      var_check <-
        all(c("USUBJID", "PARAMCD", "BNRIND", "ANRIND") %in% names(df))

      if (!var_check) {
        show_toast(
          title = "Required Variables are not present to create Shift Table",
          text = "Please update your data",
          type = "error",
          position = "center",
          width = "600px"
        )
      }
      req(var_check)
      logger::log_info("mod_bds_shift_server: creating shift table for {dataset}")

      lyt <- build_shift_table(
        adsl = df_adsl,
        bds_df = df,
        filter_cond = filt_react$filter_cond(),
        trt_var = input$split_col,
        trt_label = trt_label,
        group_var = input$group_var,
        group_label = group_label,
        default_view = input$view
      )

      rv$lyt <- lyt

      return(list(
        out_df = lyt,
        alt_df = NULL,
        lyt = NULL
      ))
    }) |>
      bindCache(
        list(
          adsl(),
          dataset,
          input$pop,
          input$split_col,
          input$group_var,
          input$view,
          filt_react$filter_cond()
        )
      ) |>
      bindEvent(list(
        adsl(),
        filt_react$trig_report(),
        input$run,
        input$view,
        rv$pop_trigger
      ))

    output$table_title <- renderUI({
      req(xx_shift())
      req(pop_fil())
      if (dataset == "advs") {
        text <- "Table 5.2 Shift at post dose for Vital Signs; "
      } else if (dataset == "adlb") {
        text <- "Table 6.2 Shift at post dose for Laboratory Tests; "
      } else {
        text <- "Table 7.2 Shift at post dose for ECG Tests; "
      }
      tags$strong(
        paste0(
          text,
          str_replace_all(str_to_title(attr(adsl()[[pop_fil()]], "label")), " Flag", "")
        )
      )
    })

    mod_dt_table_server("dt_table_shift",
      display_df = xx_shift
    )
  })
}
