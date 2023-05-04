#' ae_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_adae_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      id = "box_adae_summ",
      sidebar = boxSidebar(
        id = "adae_summ_side",
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
        tagAppendAttributes(actionButton(ns("run"), "Apply"),
                            class = "side_apply")
      ),
      maximizable = TRUE,
      width = 12,
      height = "800px",
      mod_dt_table_ui(ns("dt_table_ae_summ"))
    )
  )
}

#' ae_summary Server Functions
#'
#' @noRd
mod_adae_summary_server <- function(id,
                                    dataset,
                                    df_out,
                                    adsl,
                                    apply) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ae_summ <- reactive({
      req(df_out())
      req(adsl())
      req(input$split_col)

      df_adsl <- adsl() |>
        dplyr::select(USUBJID, ends_with("ARM")) |>
        unique()

      logger::log_info("mod_adae_summary_server: alt_data has
                         {nrow(df_adsl)} rows")

      df <- df_out()[[dataset]] |>
        dplyr::filter(USUBJID %in% unique(df_adsl$USUBJID)) |>
        mutate(
          fl1 = TRUE,
          fl2 = TRTEMFL == "Y",
          fl3 = TRTEMFL == "Y" & AEOUT == "FATAL",
          fl4 = TRTEMFL == "Y" & AEOUT == "FATAL" & AEREL == "Y",
          fl5 = TRTEMFL == "Y" & AEACN == "DRUG WITHDRAWN",
          fl6 = TRTEMFL == "Y" & DCSREAS == "ADVERSE EVENT"
        )

      logger::log_info("mod_adae_summary_server: adae has
                         {nrow(df)} rows")

      labels <- c(
        "fl1" = "Total AEs",
        "fl2" = "Total number of patients with at least one adverse event",
        "fl3" = "Total number of patients with fatal AEs",
        "fl4" = "Total number of patients with related fatal AEs",
        "fl5" = "Total number of patients with drug withdrawn due to AEs",
        "fl6" = "Total number of patients discontinued due to AEs"
      )

      formatters::var_labels(df)[names(labels)] <- labels

      lyt <- basic_table() |>
        split_cols_by(var = input$split_col) |>
        rtables::add_colcounts() |>
        rtables::add_overall_col(label = "All Patients") |>
        rtables::add_colcounts() |>
        tern::count_patients_with_flags("USUBJID",
                                        flag_variables = formatters::var_labels(df[, c("fl1", "fl2", "fl3",
                                                                                       "fl4", "fl5", "fl6")]))

      return(list(
        adsl = df,
        alt_df = df_adsl,
        lyt = lyt
      ))
    }) |>
      bindEvent(list(adsl(), input$run))

    mod_dt_table_server("dt_table_ae_summ",
                        display_df = ae_summ)
  })
}

## To be copied in the UI
# mod_ae_summary_ui("ae_summary_1")

## To be copied in the server
# mod_ae_summary_server("ae_summary_1")
