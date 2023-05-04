#' adae_display UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_adae_display_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      id = "box_adae",
      sidebar = boxSidebar(
        id = "adae_side",
        background = "#EFF5F5",
        width = 25,
        h2("Table Options"),
        selectInput(
          ns("split_col"),
          "Split Cols by",
          choices = c("ARM", "ACTARM", "TRT01P", "TRT02P", "TRT01A", "TRT02A"),
          selected = c("ARM"),
          width = 300
        ),
        selectInput(
          ns("class"),
          "Class",
          choices = c("AEBODSYS", "AESOC"),
          selected = "AESOC",
          width = 300
        ),
        selectInput(
          ns("term"),
          "Term",
          choices = c("AETERM", "AEDECOD"),
          selected = "AEDECOD",
          width = 300
        ),
        selectInput(
          ns("summ_var"),
          "Summarize",
          choices = c("AESEV", "AETOXGR"),
          selected = c("AESEV"),
          multiple = FALSE,
          width = 300
        ),
        tagAppendAttributes(actionButton(ns("run"), "Apply"),
                            class = "side_apply")
      ),
      maximizable = TRUE,
      width = 12,
      height = "800px",
      mod_dt_table_ui(ns("dt_table_2"))
    )
  )
}

#' adae_display Server Functions
#'
#' @importFrom tern summarize_num_patients summarize_occurrences_by_grade
#' @importFrom rtables add_colcounts add_overall_col drop_split_levels
#'
#' @noRd
mod_adae_display_server <- function(id,
                                    dataset,
                                    df_out,
                                    adsl,
                                    apply) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ae_explore <- reactive({
      req(df_out())
      req(adsl())
      req(input$split_col)
      req(input$class)
      req(input$term)
      req(input$summ_var)

      df_adsl <- adsl() |>
        dplyr::select(USUBJID, dplyr::ends_with("ARM")) |>
        unique()

      logger::log_info("mod_adae_display_server: alt_data has
                         {nrow(df_adsl)} rows")

      df <- df_out()[[dataset]] |>
        dplyr::filter(USUBJID %in% unique(df_adsl$USUBJID))

      logger::log_info("mod_adae_display_server: adae has
                         {nrow(df)} rows")

      lyt <- basic_table() |>
        split_cols_by(var = input$split_col) |>
        add_colcounts() |>
        add_overall_col(label = "All Patients") |>
        add_colcounts() |>
        summarize_num_patients("USUBJID") |>
        split_rows_by(input$class,
                      child_labels = "visible",
                      nested = TRUE,
                      indent_mod = 1,
                      split_fun = drop_split_levels) |>
        split_rows_by(input$term,
                      child_labels = "visible",
                      nested = TRUE,
                      indent_mod = 2,
                      split_fun = drop_split_levels) |>
        summarize_occurrences_by_grade(input$summ_var)

      return(list(adsl = df, alt_df = df_adsl, lyt = lyt))
    }) |>
      bindEvent(list(adsl(), input$run))

    mod_dt_table_server("dt_table_2",
                        display_df = ae_explore)
  })
}
