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
      title = tags$strong("Summary of Adverse Events"),
      sidebar = boxSidebar(
        id = ns("adae_summ_side"),
        background = "#EFF5F5",
        width = 35,
        mod_filter_reactivity_ui(ns("filter_reactivity_1")),
        h2(tags$strong("Table Options")),
        selectInput(
          ns("split_col"),
          "Split Cols by",
          choices = NULL,
          selected = NULL,
          width = 400
        ),
        prettyCheckboxGroup(
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
          class = "side_apply"
        )
      ),
      maximizable = TRUE,
      width = 12,
      height = "800px",
      div(withSpinner(mod_dt_table_ui(ns("dt_table_ae_summ")), type = 6, color = "#3BACB6"),
        style = "overflow-x: scroll;"
      )
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

    ae_summ_init <- reactive({
      req(df_out()[[dataset]])
      req(adsl())

      df_adsl <- adsl() |>
        select(USUBJID, ends_with("ARM"), starts_with("TRT")) |>
        unique()

      logger::log_info("mod_adae_summary_server: alt_data has
                         {nrow(df_adsl)} rows")

      df <- df_out()[[dataset]] |>
        filter(USUBJID %in% unique(df_adsl$USUBJID))

      df <- add_adae_flags(df)

      logger::log_info("mod_adae_summary_server: adae has
                         {nrow(df)} rows")

      aesi_vars <- c(
        "FATAL", "SER", "SERWD", "SERDSM", "RELSER", "WD",
        "DSM", "REL", "RELWD", "RELDSM", "CTC35", "CTC45"
      )

      labels <- var_labels(df[, aesi_vars])

      return(list(
        out_df = df,
        alt_df = df_adsl,
        labs = labels,
        aesi_vars = aesi_vars
      ))
    }) |>
      bindEvent(list(adsl(), df_out()[[dataset]]))

    observe({
      req(ae_summ_init())
      logger::log_info("mod_adae_summary_server: update show/hide events")

      df <- ae_summ_init()$out_df
      choices <- names(select(df, all_of(ae_summ_init()$aesi_vars)))
      selected <- choices
      labs <- as.character(ae_summ_init()$labs)
      trt_choices <-
        names(select(adsl(), setdiff(starts_with(c("ARM", "TRT0")), ends_with("DTM"))))

      updatePrettyCheckboxGroup(
        inputId = "events",
        label = "Show/Hide Events",
        choiceNames = labs,
        choiceValues = choices,
        selected = selected,
        prettyOptions = list(
          animation = "pulse",
          status = "info",
          shape = "curve"
        )
      )

      updateSelectInput(session,
        "split_col",
        choices = trt_choices,
        selected = trt_choices[1]
      )
    }) |>
      bindEvent(ae_summ_init())

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

    ae_summ <- reactive({
      req(ae_summ_init())
      req(input$split_col != "")
      req(input$events)

      df <- ae_summ_init()$out_df

      if (!is.null(filt_react$filter_cond())) {
        df <- df |>
          filter(!!!parse_exprs(filt_react$filter_cond()))
      }

      disp_eve <- ae_summ_init()$aesi_vars
      disp_eve <- disp_eve[disp_eve %in% input$events]

      lyt <- basic_table(show_colcounts = TRUE) |>
        split_cols_by(var = input$split_col) |>
        add_overall_col(label = "All Patients") |>
        count_patients_with_event(
          vars = "USUBJID",
          filters = c("STUDYID" = as.character(unique(ae_summ_init()$out_df[["STUDYID"]]))),
          denom = "N_col",
          .labels = c(count_fraction = "Total number of patients with at least one adverse event")
        ) |>
        count_values(
          "STUDYID",
          values = as.character(unique(ae_summ_init()$out_df[["STUDYID"]])),
          .stats = "count",
          .labels = c(count = "Total AEs"),
          table_names = "total_aes"
        ) |>
        count_patients_with_flags("USUBJID",
          flag_variables = var_labels(ae_summ_init()$out_df[, disp_eve]),
          denom = "N_col",
          var_labels = "Total number of patients with at least one",
          show_labels = "visible"
        )

      return(list(
        out_df = df,
        alt_df = ae_summ_init()$alt_df,
        lyt = lyt
      ))
    }) |>
      bindCache(list(ae_summ_init(), input$split_col, input$events, filt_react$filter_cond())) |>
      bindEvent(list(ae_summ_init(), input$run, filt_react$trig_report()))

    mod_dt_table_server("dt_table_ae_summ",
      display_df = ae_summ
    )
  })
}
