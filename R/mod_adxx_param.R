#' adxx_param UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_adxx_param_ui <- function(id,
                              title = "",
                              domain = "ADVS",
                              logo = "stethoscope") {
  ns <- NS(id)
  tagList(
    box(
      id = ns("box_adxx_param"),
      title = tags$strong(title),
      sidebar = boxSidebar(
        id = ns("adxx_side_param"),
        background = "#EFF5F5",
        width = 35,
        div(uiOutput(ns("analysis_flag_ui"))),
        mod_filter_reactivity_ui(ns("filter_reactivity_1"), domain = domain, logo = logo),
        div(
          accordion(
            id = ns("param_accord"),
            tagAppendAttributes(accordionItem(
              title = tags$span(icon("table-cells"), tags$strong("Table Options")),
              collapsed = FALSE,
              selectInput(
                ns("split_col"),
                "Treatment Variable",
                choices = NULL,
                selected = NULL,
                width = 400
              ),
              selectInput(
                ns("param"),
                "Parameter Value",
                choices = NULL,
                selected = NULL,
                width = 400,
              ),
              selectInput(
                ns("visit"),
                "Analysis Visit",
                choices = NULL,
                selected = NULL,
                width = 400,
              ),
              selectInput(
                ns("summ_var"),
                "Analysis Variables",
                choices = NULL,
                selected = NULL,
                width = 400,
                multiple = TRUE
              )
            ), class = "side_accord")
          ),
          style = "width: 350px;"
        ),
        tagAppendAttributes(actionButton(ns("run"), "Update"),
          class = "side_apply"
        )
      ),
      maximizable = TRUE,
      width = 12,
      height = "800px",
      div(
        shinycssloaders::withSpinner(
          mod_dt_table_ui(ns(
            "dt_table_param"
          )),
          color = "#3BACB6"
        ),
        style = "overflow-x: scroll;"
      )
    )
  )
}

#' adxx_param Server Functions
#'
#' @noRd
mod_adxx_param_server <- function(id,
                                  dataset,
                                  df_out,
                                  adsl,
                                  filters = reactive(NULL)) {
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
      anl_flags <- names(select(df_out()[[dataset]], starts_with("ANL0")))
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
      logger::log_info("mod_adxx_param_server: updating table options for {dataset}")

      df <- df_out()[[dataset]]

      trt_choices <-
        names(select(
          adsl(),
          setdiff(
            starts_with(c("ACT", "ARM", "TRT", "TR0", "TR1", "TR2")),
            ends_with(c("DTM", "DUR", "PN", "AN", "DT", "FL"))
          )
        ))
      param_choices <- unique(df$PARAM)
      visit_choices <- names(select(df, ends_with("VISIT")))
      summ_choices <- c("AVAL", sort(names(select(df, contains("CHG")))))

      updateSelectInput(session,
        "split_col",
        choices = trt_choices,
        selected = trt_choices[1]
      )

      updateSelectInput(session,
        "param",
        choices = param_choices,
        selected = param_choices[1]
      )

      updateSelectInput(session,
        "visit",
        choices = visit_choices,
        selected = visit_choices[1]
      )

      updateSelectInput(session,
        "summ_var",
        choices = summ_choices,
        selected = summ_choices[1:2]
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
      anl_flags <- names(select(df_out()[[dataset]], starts_with("ANL0")))
      if (length(anl_flags) == 0) {
        rv$pop_trigger <- TRUE
      } else {
        req(input$pop)
        rv$pop_trigger <- TRUE
      }
    })

    xx_param <- reactive({
      req(df_out()[[dataset]])
      req(adsl())
      req(input$split_col)
      req(input$visit)
      req(input$param)
      req(input$summ_var)
      req(rv$pop_trigger)

      df_adsl <- adsl() |>
        select(USUBJID, input$split_col) |>
        unique()

      logger::log_info("mod_adxx_param_server: alt_data has {nrow(df_adsl)} rows")

      df <- df_out()[[dataset]]

      if (!is.null(input$pop)) {
        df <- df |>
          filter(.data[[input$pop]] == "Y")
      }

      df <- df |>
        left_join(df_adsl) |>
        filter(USUBJID %in% unique(df_adsl$USUBJID))

      lyt <- build_generic_bds_table(
        bds_df = df,
        filter_cond = filt_react$filter_cond(),
        param = input$param,
        trt_var = input$split_col,
        visit = input$visit,
        disp_vars = input$summ_var
      )

      rv$lyt <- lyt$lyt

      return(list(
        out_df = lyt$df_out,
        alt_df = df_adsl,
        lyt = lyt$lyt
      ))
    }) |>
      bindCache(list(
        adsl(),
        dataset,
        input$pop,
        input$split_col,
        input$visit,
        input$param,
        input$summ_var,
        filt_react$filter_cond()
      )) |>
      bindEvent(list(adsl(), filt_react$trig_report(), input$run, rv$pop_trigger))

    mod_dt_table_server("dt_table_param",
      display_df = xx_param
    )
  })
}
