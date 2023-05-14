#' adae_bodsys UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_adae_bodsys_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      id = "box_adae_bod",
      title = "Summary of Treatment-Emergent Adverse Events (TEAES) By Body System Class",
      sidebar = boxSidebar(
        id = "adae_side_bodsys",
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
        tagAppendAttributes(actionButton(ns("run"), "Update"),
                            class = "side_apply")
      ),
      maximizable = TRUE,
      width = 12,
      height = "800px",
      mod_dt_table_ui(ns("dt_table_bodsys"))
    )
  )
}

#' adae_bodsys Server Functions
#'
#' @noRd
mod_adae_bodsys_server <- function(id,
                                   dataset,
                                   df_out,
                                   adsl) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ae_bodsys <- reactive({
      req(df_out())
      req(adsl())
      req(input$split_col)
      req(input$class)
      req(input$term)

      df_adsl <- adsl() |>
        select(USUBJID, ends_with("ARM"), starts_with("TRT")) |>
        unique()

      logger::log_info("mod_adae_bodsys_server: alt_data has
                         {nrow(df_adsl)} rows")

      df <- df_out()[[dataset]] |>
        filter(USUBJID %in% unique(df_adsl$USUBJID))

      logger::log_info("mod_adae_bodsys_server: adae has
                         {nrow(df)} rows")

      lyt <- basic_table() |>
        split_cols_by(var = input$split_col) |>
        add_colcounts() |>
        add_overall_col(label = "All Patients") |>
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(unique = "Total number of patients with at least one AE",
                      nonunique = "Overall total number of events")
        ) |>
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
      bindEvent(list(adsl(), input$run))

    mod_dt_table_server("dt_table_bodsys",
                        display_df = ae_bodsys)
  })
}
