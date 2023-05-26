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
                            class = "side_apply")
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
          style = "overflow-x: scroll;")
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
                                    adsl) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(trig_report = FALSE)

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
                        selected = trt_choices[1])

      updateSelectInput(session,
                        "class",
                        choices = class_choices,
                        selected = class_choices[1])

      updateSelectInput(session,
                        "term",
                        choices = term_choices,
                        selected = term_choices[1])

      updateSelectInput(session,
                        "summ_var",
                        choices = summ_var,
                        selected = summ_var[1])
    }) |>
      bindEvent(adsl())

    observe({
      req(input$split_col != "")
      req(input$class != "")
      req(input$term != "")
      req(input$summ_var != "")
      rv$trig_report <- TRUE
    })

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
      bindCache(list(input$split_col, input$class, input$term, input$view)) |>
      bindEvent(list(adsl(), rv$trig_report, input$run, input$view))

    mod_dt_table_server("dt_table_2",
                        display_df = ae_explore)
  })
}
