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
      id = ns("box_adae_summ"),
      title = "Summary of Adverse Events",
      sidebar = boxSidebar(
        id = ns("adae_summ_side"),
        background = "#EFF5F5",
        width = 35,
        h2("Table Options"),
        selectInput(
          ns("split_col"),
          "Split Cols by",
          choices = NULL,
          selected = NULL,
          width = 300
        ),
        shinyWidgets::prettyCheckboxGroup(
          ns("events"),
          label = NULL,
          choiceNames = NULL,
          choiceValues = NULL,
          selected = NULL,
          animation = "pulse",
          status = "info",
          shape = "curve"
        ),
        tagAppendAttributes(actionButton(ns("run"), "Update"),
                            class = "side_apply")
      ),
      maximizable = TRUE,
      width = 12,
      height = "800px",
      div(withSpinner(mod_dt_table_ui(ns("dt_table_ae_summ")), type = 6, color = "#3BACB6"),
          style = "overflow-x: scroll;")
    )
  )
}

#' ae_summary Server Functions
#'
#' @noRd
mod_adae_summary_server <- function(id,
                                    dataset,
                                    df_out,
                                    adsl) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ae_summ_init <- reactive({
      req(df_out()[[dataset]])
      req(adsl())

      df_adsl <- adsl() |>
        select(USUBJID, ends_with("ARM"), starts_with("TRT")) |>
        unique()

      logger::log_info("mod_adae_summary_server: alt_data has
                         {nrow(df_adsl)} rows")

      df <- df_out()[[dataset]] |>
        filter(USUBJID %in% unique(df_adsl$USUBJID)) |>
        mutate(
          fl1 = TRUE,
          fl2 = TRTEMFL == "Y",
          fl21 = TRTEMFL == "Y" & AESER == "Y",
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
        "fl21" = "Total number of patients with at least one serious adverse event",
        "fl3" = "Total number of patients with fatal AEs",
        "fl4" = "Total number of patients with related fatal AEs",
        "fl5" = "Total number of patients with drug withdrawn due to AEs",
        "fl6" = "Total number of patients discontinued due to AEs"
      )

      formatters::var_labels(df)[names(labels)] <- labels

      return(list(
        out_df = df,
        alt_df = df_adsl,
        labs = labels
      ))
    }) |>
      bindEvent(list(adsl()))

    observe({
      req(ae_summ_init())
      logger::log_info("mod_adae_summary_server: update show/hide events")

      df <- ae_summ_init()$out_df
      choices <- names(select(df, starts_with("fl")))
      selected <- choices
      labs <- as.character(ae_summ_init()$labs)
      trt_choices <-
        names(select(adsl(), setdiff(starts_with(c("ARM", "TRT0")), ends_with("DTM"))))

      shinyWidgets::updatePrettyCheckboxGroup(
        inputId = "events",
        label = "Show/Hide Events",
        choiceNames = labs,
        choiceValues = choices,
        selected = selected,
        prettyOptions = list(animation = "pulse",
                             status = "info",
                             shape = "curve")
      )

      updateSelectInput(session,
                        "split_col",
                        choices = trt_choices,
                        selected = trt_choices[1])
    }) |>
      bindEvent(ae_summ_init())

    ae_summ <- reactive({
      req(ae_summ_init())
      req(input$split_col != "")
      req(input$events)

      disp_eve <- c("fl1", "fl2", "fl21", "fl3", "fl4", "fl5", "fl6")
      disp_eve <- disp_eve[disp_eve %in% input$events]

      lyt <- basic_table() |>
        split_cols_by(var = input$split_col) |>
        rtables::add_colcounts() |>
        rtables::add_overall_col(label = "All Patients") |>
        rtables::add_colcounts() |>
        tern::count_patients_with_flags("USUBJID",
                                        flag_variables =
                                          var_labels(ae_summ_init()$out_df[, disp_eve]))

      return(list(
        out_df = ae_summ_init()$out_df,
        alt_df = ae_summ_init()$alt_df,
        lyt = lyt
      ))
    }) |>
      bindEvent(list(ae_summ_init(), input$run))

    mod_dt_table_server("dt_table_ae_summ",
                        display_df = ae_summ)
  })
}
