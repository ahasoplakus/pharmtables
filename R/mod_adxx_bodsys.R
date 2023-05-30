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
                                   adsl) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(trig_report = FALSE)

    observe({
      req(dataset != "cadae")
      shinyjs::hide("aeser")
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

    observe({
      req(input$split_col != "")
      req(input$class != "")
      req(input$term != "")
      rv$trig_report <- TRUE
    })

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
      bindCache(list(input$split_col, input$class, input$term, input$aeser)) |>
      bindEvent(list(adsl(), rv$trig_report, input$run, input$aeser))

    mod_dt_table_server("dt_table_bodsys",
      display_df = xx_bodsys
    )
  })
}
