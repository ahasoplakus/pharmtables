#' adae_sev_tox UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_adae_sev_tox_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      id = ns("box_adae"),
      title = "Summary of Treatment-Emergent Adverse Events (TEAES) By Body System And Severity",
      sidebar = boxSidebar(
        id = ns("adae_side"),
        background = "#EFF5F5",
        width = 25,
        h2("Table Options"),
        div(
          id = ns("domain_filters"),
          uiOutput(ns("xx_filt_ui")),
          style = "width: 200px; overflow-x: scroll;"
        ),
        selectInput(
          ns("split_col"),
          "Split Cols by",
          choices = NULL,
          selected = NULL,
          width = 300
        ),
        selectInput(
          ns("class"),
          "Class",
          choices = NULL,
          selected = NULL,
          width = 300
        ),
        selectInput(
          ns("term"),
          "Term",
          choices = NULL,
          selected = NULL,
          width = 300
        ),
        selectInput(
          ns("summ_var"),
          "Summarize",
          choices = NULL,
          selected = NULL,
          width = 300
        ),
        tagAppendAttributes(actionButton(ns("run"), "Update"),
          class = "side_apply"
        )
      ),
      maximizable = TRUE,
      width = 12,
      height = "800px",
      shinyWidgets::prettySwitch(
        ns("view"),
        label = "Default View",
        value = TRUE,
        status = "info",
        inline = TRUE,
        fill = TRUE,
        slim = TRUE
      ),
      div(withSpinner(mod_dt_table_ui(ns("dt_table_2")), type = 6, color = "#3BACB6"),
        style = "overflow-x: scroll;"
      )
    )
  )
}

#' adae_sev_tox Server Functions
#'
#' @importFrom tern summarize_num_patients summarize_occurrences_by_grade
#' @importFrom rtables add_colcounts add_overall_col drop_split_levels
#'
#' @noRd
mod_adae_sev_tox_server <- function(id,
                                    dataset,
                                    df_out,
                                    adsl,
                                    filters = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(trig_report = 0)

    observe({
      req(df_out()[[dataset]])
      if (is.null(filters())) hide("domain_filters") else show("domain_filters")
    })

    output$xx_filt_ui <- renderUI({
      req(df_out()[[dataset]])
      req(filters())

      logger::log_info("mod_adae_sev_tox_server: initialise {dataset} filters")

      tagList(
        create_widget(
          filters(),
          df_out(),
          dataset,
          ns
        )
      )
    })

    outputOptions(output, "xx_filt_ui", priority = 925)

    observe(
      {
        req(df_out()[[dataset]])
        req(filters())
        req(length(reactiveValuesToList(input)) > 0)
        req(input$split_col != "")

        rv$filters <-
          set_names(tolower(filters())) |>
          map(\(x) input[[x]])
        req(none(rv$filters, is.null))
        req(!identical(rv$filters, rv$cached_filters))

        logger::log_info("mod_adae_sev_tox_server: update {dataset} filter condtion")
        rv$filter_cond <- filters_to_cond(rv$filters)
      },
      priority = 920
    )

    observe({
      req(rv$filters)
      req(rv$filter_cond)

      filt_update <- isTRUE(unique(map_lgl(
        names(rv$filters),
        \(x) identical(rv$filters[[x]], levels(unique(df_out()[[dataset]][[toupper(x)]])))
      )))

      if (!is.null(rv$cached_filters) &&
        length(rv$filters) > length(rv$cached_filters)) {
        req(filt_update)
        logger::log_info("mod_adae_sev_tox_server: triggering report")
        rv$trig_report <- rv$trig_report + 1
      } else if (!is.null(rv$cached_filters) &&
        length(rv$filters) < length(rv$cached_filters)) {
        if (isTRUE(filt_update)) {
          trig_stop <- FALSE
        } else {
          trig_stop <- any(unique(map_lgl(
            names(rv$filters), \(x) identical(rv$filters[[x]], rv$cached_filters[[x]])
          )))
        }
        req(!trig_stop)
        logger::log_info("mod_adae_sev_tox_server: triggering report")
        rv$trig_report <- rv$trig_report + 1
      } else if (!is.null(rv$cached_filters) &&
        !identical(names(rv$filters), names(rv$cached_filters))) {
        req(filt_update)
        logger::log_info("mod_adae_sev_tox_server: triggering report")
        rv$trig_report <- rv$trig_report + 1
      }

      rv$cached_filters <- rv$filters
    }) |>
      bindEvent(rv$filter_cond)

    observe({
      req(adsl())
      req(df_out()[[dataset]])
      logger::log_info("mod_adae_sev_tox_server: updating table options for {dataset}")

      df <- df_out()[[dataset]]

      trt_choices <-
        names(select(adsl(), setdiff(starts_with(
          c("ARM", "TRT0")
        ), ends_with("DTM"))))
      class_choices <-
        names(select(df, union(ends_with(
          c("SOC", "BODSYS", "CAT")
        ), starts_with("ATC"))))
      term_choices <-
        names(select(df, ends_with(c(
          "TERM", "DECOD"
        ))))
      summ_var <-
        names(select(df, ends_with(c("SEV", "TOXGR"))))

      updateSelectInput(session,
        "split_col",
        choices = trt_choices,
        selected = trt_choices[1]
      )

      updateSelectInput(session,
        "class",
        choices = class_choices,
        selected = class_choices[1]
      )

      updateSelectInput(session,
        "term",
        choices = term_choices,
        selected = term_choices[1]
      )

      updateSelectInput(session,
        "summ_var",
        choices = summ_var,
        selected = summ_var[1]
      )
    }) |>
      bindEvent(list(adsl(), df_out()[[dataset]]))

    ae_explore <- reactive({
      req(df_out()[[dataset]])
      req(adsl())
      req(input$split_col)
      req(input$class)
      req(input$term)
      req(input$summ_var)

      df_adsl <- adsl() |>
        select(USUBJID, ends_with("ARM"), starts_with("TRT")) |>
        unique()

      logger::log_info("mod_adae_sev_tox_server: alt_data has
                         {nrow(df_adsl)} rows")

      df <- df_out()[[dataset]] |>
        filter(USUBJID %in% unique(df_adsl$USUBJID))

      if (!is.null(rv$filter_cond)) {
        df <- df |>
          filter(!!!parse_exprs(rv$filter_cond))
      }

      logger::log_info("mod_adae_sev_tox_server: adae has
                         {nrow(df)} rows")

      out_df <- adae_by_sev_tox(
        adsl = df_adsl,
        df_adae = df,
        colsby = input$split_col,
        grade_val = input$summ_var,
        class_val = input$class,
        term_val = input$term,
        default_view = input$view
      )

      return(list(
        out_df = out_df,
        alt_df = NULL,
        lyt = NULL
      ))
    }) |>
      bindCache(
        list(
          adsl(), input$split_col, input$class, input$term,
          input$summ_var, input$view, rv$filter_cond
        )
      ) |>
      bindEvent(list(adsl(), rv$trig_report, input$run, input$view))

    mod_dt_table_server("dt_table_2",
      display_df = ae_explore
    )
  })
}
