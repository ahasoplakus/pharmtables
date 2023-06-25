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
        title = tags$strong(title),
        sidebar = boxSidebar(
          id = ns("adxx_side_bodsys"),
          background = "#EFF5F5",
          width = 25,
          h2("Table Options"),
          mod_filter_reactivity_ui(ns("filter_reactivity_1")),
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
        div(
          withSpinner(
            mod_dt_table_ui(ns(
              "dt_table_bodsys"
            )),
            type = 6, color = "#3BACB6"
          ),
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
                                   filters = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
      logger::log_info("mod_adxx_bodsys_server: updating table options for {dataset}")

      df <- df_out()[[dataset]]

      trt_choices <-
        names(select(adsl(), setdiff(starts_with(
          c("ARM", "TRT0")
        ), ends_with("DTM"))))
      class_choices <-
        sort(names(select(
          df, union(ends_with(c(
            "SOC", "BODSYS"
          )), starts_with("ATC"))
        )))
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

      if (!is.null(filt_react$filter_cond())) {
        df <- df |>
          filter(!!!parse_exprs(filt_react$filter_cond()))
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
      bindCache(list(
        adsl(),
        input$split_col,
        input$class,
        input$term,
        filt_react$filter_cond()
      )) |>
      bindEvent(list(adsl(), filt_react$trig_report(), input$run))

    mod_dt_table_server("dt_table_bodsys",
      display_df = xx_bodsys
    )
  })
}
