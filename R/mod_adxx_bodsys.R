#' adxx_bodsys UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_adxx_bodsys_ui <-
  function(id,
           title = "Summary of Treatment-Emergent Adverse Events (TEAES) By Body System Class") {
    ns <- NS(id)
    tagList(
      box(
        id = ns("box_adxx_bodsys"),
        title = title,
        sidebar = boxSidebar(
          id = ns("adxx_side_bodsys"),
          background = "#EFF5F5",
          width = 25,
          h2("Table Options"),
          div(uiOutput(ns("xx_filt_ui")), style = "width: 200px; overflow-x: scroll;"),
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
          tagAppendAttributes(actionButton(ns("run"), "Update"),
            class = "side_apply"
          )
        ),
        maximizable = TRUE,
        width = 12,
        height = "800px",
        shinyWidgets::prettySwitch(
          ns("aeser"),
          label = "Only Serious Adverse Events",
          value = FALSE,
          status = "info",
          inline = TRUE,
          fill = TRUE,
          slim = TRUE
        ),
        div(withSpinner(mod_dt_table_ui(ns("dt_table_bodsys")), type = 6, color = "#3BACB6"),
          style = "overflow-x: scroll;"
        )
      )
    )
  }

#' adxx_bodsys Server Functions
#'
#' @noRd
mod_adxx_bodsys_server <- function(id,
                                   dataset,
                                   df_out,
                                   adsl,
                                   filters = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(trig_report = 0)

    observe({
      req(dataset != "cadae")
      shinyjs::hide("aeser")
    })

    output$xx_filt_ui <- renderUI({
      req(df_out()[[dataset]])
      req(filters)

      logger::log_info("mod_adxx_bodsys_server: initialise {dataset} filters")

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

    observe({
      req(df_out()[[dataset]])
      req(filters)
      req(length(reactiveValuesToList(input)) > 0)

      rv$filters <-
        set_names(tolower(filters())) |>
        map(\(x) input[[x]])
      req(none(rv$filters, is.null))
      req(!identical(rv$filters, rv$cached_filters))

      logger::log_info("mod_adxx_bodsys_server: update {dataset} filter condtion")
      domain_filters <- map(names(rv$filters), \(x) {
        if (!is.numeric(rv$filters[[x]])) {
          vals <- paste0(rv$filters[[x]], collapse = "','")
          vals <- str_glue("{toupper(x)} %in% c('{vals}')")
        } else {
          vals <- rv$filters[[x]]
          vals <- str_glue("{toupper(x)} <= {vals}")
        }
      })

      rv$filter_cond <- reduce(domain_filters, paste, sep = " & ")
    }, priority = 920)

    observe({
      req(rv$filters)
      req(rv$filter_cond)

      if (!is.null(rv$cached_filters) &&
          length(rv$filters) > length(rv$cached_filters)) {
        logger::log_info("mod_adxx_bodsys_server: triggering report")
        rv$trig_report <- rv$trig_report + 1
      } else if (!is.null(rv$cached_filters) &&
                 length(rv$filters) < length(rv$cached_filters)) {
        trig_stop <- isTRUE(unique(map_lgl(
          seq_along(rv$filters),
          \(x) identical(rv$filters[[x]], rv$cached_filters[[x]])
        )))
        req(!trig_stop)
        logger::log_info("mod_adxx_bodsys_server: triggering report")
        rv$trig_report <- rv$trig_report + 1
      }

      rv$cached_filters <- rv$filters
    }) |>
      bindEvent(rv$filter_cond)

    observe({
      req(adsl())
      req(df_out()[[dataset]])
      logger::log_info("mod_adxx_bodsys_server: updating table options for {dataset}")

      df <- df_out()[[dataset]]

      trt_choices <-
        names(select(adsl(), setdiff(starts_with(
          c("ARM", "TRT0")
        ), ends_with("DTM"))))
      class_choices <-
        sort(names(select(df, union(ends_with(
          c("SOC", "BODSYS")
        ), starts_with("ATC")))))
      term_choices <-
        names(select(df, ends_with(c(
          "TERM", "DECOD"
        ))))

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
    }) |>
      bindEvent(adsl())

    xx_bodsys <- reactive({
      req(df_out()[[dataset]])
      req(adsl())
      req(input$split_col)
      req(input$class)
      req(input$term)

      df_adsl <- adsl() |>
        select(USUBJID, ends_with("ARM"), starts_with("TRT")) |>
        unique()

      logger::log_info("mod_adxx_bodsys_server: alt_data has {nrow(df_adsl)} rows")

      df <- df_out()[[dataset]] |>
        filter(USUBJID %in% unique(df_adsl$USUBJID))

      if (!is.null(rv$filter_cond)) {
        df <- df |>
          filter(!!!parse_exprs(rv$filter_cond))
      }

      if (isTRUE(input$aeser)) {
        df <- df |>
          filter(AESER == "Y")
      }

      logger::log_info("mod_adxx_bodsys_server: {dataset} has {nrow(df)} rows")

      lyt <- basic_table() |>
        split_cols_by(var = input$split_col) |>
        add_colcounts() |>
        add_overall_col(label = "All Patients")

      if (dataset == "cadae") {
        lyt <- lyt |>
          summarize_num_patients(
            var = "USUBJID",
            .stats = c("unique", "nonunique"),
            .labels = c(
              unique = "Total number of patients with at least one event",
              nonunique = "Total number of events"
            )
          )
      }

      lyt <- lyt |>
        split_rows_by(
          input$class,
          child_labels = "visible",
          nested = FALSE,
          indent_mod = -1L,
          label_pos = "topleft",
          split_label = obj_label(df[[input$class]]),
          split_fun = drop_split_levels
        ) |>
        count_occurrences(vars = input$term, .indent_mods = -1L) |>
        append_varlabels(df, input$term, indent = 1L)

      return(list(
        out_df = df,
        alt_df = df_adsl,
        lyt = lyt
      ))
    }) |>
      bindCache(list(adsl(), input$split_col, input$class, input$term, input$aeser, rv$filter_cond)) |>
      bindEvent(list(adsl(), rv$trig_report, input$run, input$aeser))

    mod_dt_table_server("dt_table_bodsys",
      display_df = xx_bodsys
    )
  })
}
