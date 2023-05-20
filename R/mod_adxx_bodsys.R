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
           title = "Summary of Treatment-Emergent Adverse Events (TEAES) By Body System Class",
           trt_choices = c("ARM", "ACTARM", "TRT01P", "TRT02P", "TRT01A", "TRT02A"),
           class_choices = c("AESOC", "AEBODSYS"),
           term_choices = c("AETERM", "AEDECOD")) {
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
            choices = trt_choices,
            selected = trt_choices[1],
            width = 300
          ),
          selectInput(
            ns("class"),
            "Class",
            choices = class_choices,
            selected = class_choices[1],
            width = 300
          ),
          selectInput(
            ns("term"),
            "Term",
            choices = term_choices,
            selected = term_choices[1],
            width = 300
          ),
          tagAppendAttributes(actionButton(ns("run"), "Update"),
                              class = "side_apply")
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
          fill = TRUE
        ),
        mod_dt_table_ui(ns("dt_table_bodsys"))
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

    observe({
      req(dataset != "cadae")
      req(!"AESER" %in% names(df_out()[[dataset]]))
      shinyjs::hide("aeser")
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

      logger::log_info("mod_adxx_bodsys_server: alt_data has
                         {nrow(df_adsl)} rows")

      df <- df_out()[[dataset]] |>
        filter(USUBJID %in% unique(df_adsl$USUBJID))

      if (isTRUE(input$aeser)) {
        df <- df |>
          filter(AESER == "Y")
      }

      logger::log_info("mod_adxx_bodsys_server: adae has
                         {nrow(df)} rows")

      lyt <- basic_table() |>
        split_cols_by(var = input$split_col) |>
        add_colcounts() |>
        add_overall_col(label = "All Patients")

      if (dataset == "cadae") {
        lyt <- lyt |>
          summarize_num_patients(
            var = "USUBJID",
            .stats = c("unique", "nonunique"),
            .labels = c(unique = "Total number of patients with at least one event",
                        nonunique = "Total number of events")
          )
      }

      lyt <- lyt |>
        split_rows_by(
          input$class,
          child_labels = "visible",
          nested = FALSE,
          indent_mod = -1L,
          split_fun = drop_split_levels
        ) |>
        count_occurrences(vars = input$term)

      return(list(
        out_df = df,
        alt_df = df_adsl,
        lyt = lyt
      ))
    }) |>
      bindEvent(list(adsl(), input$run, input$aeser))

    mod_dt_table_server("dt_table_bodsys",
                        display_df = xx_bodsys)
  })
}
