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
      title =
        tags$strong("Summary of Treatment-Emergent Adverse Events (TEAES) By
                    Body System And Severity"),
      sidebar = boxSidebar(
        id = ns("adae_side"),
        background = "#EFF5F5",
        width = 35,
        mod_filter_reactivity_ui(ns("filter_reactivity_1")),
        div(
          accordion(
            id = ns("sevtox_accord"),
            accordionItem(
              title = tags$span(icon("table-cells"), tags$strong("Table Options")),
              collapsed = FALSE,
              selectInput(
                ns("split_col"),
                "Treatment Variable",
                choices = NULL,
                selected = NULL,
                width = 400
              ),
              selectInput(
                ns("class"),
                "Higher Level Term",
                choices = NULL,
                selected = NULL,
                width = 400
              ),
              selectInput(
                ns("term"),
                "Lower Level Term",
                choices = NULL,
                selected = NULL,
                width = 400
              ),
              selectInput(
                ns("summ_var"),
                "Summarize",
                choices = NULL,
                selected = NULL,
                width = 400
              )
            )
          ),
          style = "width: 350px;"
        ),
        tagAppendAttributes(actionButton(ns("run"), "Update"),
          class = "side_apply"
        )
      ),
      maximizable = TRUE,
      width = 12,
      height = "800px",
      shinyWidgets::prettySwitch(
        ns("view"),
        label = "Default View",
        value = FALSE,
        status = "info",
        inline = TRUE,
        fill = TRUE,
        slim = TRUE
      ),
      div(shinycssloaders::withSpinner(mod_dt_table_ui(ns("dt_table_2")), color = "#3BACB6"),
        style = "overflow-x: scroll;"
      )
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
                                    adsl,
                                    filters = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(occ_cached = NULL)

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
      req(!identical(df_out()[[dataset]], rv$occ_cached))
      logger::log_info("mod_adae_sev_tox_server: updating table options for {dataset}")

      df <- df_out()[[dataset]]

      trt_choices <-
        names(select(
          adsl(),
          setdiff(
            starts_with(c("ACT", "ARM", "TRT")),
            ends_with(c("DTM", "DUR", "PN", "AN", "DT", "FL"))
          )
        ))
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

      updateSelectInput(session,
        "summ_var",
        choices = summ_var,
        selected = summ_var[1]
      )

      rv$occ_cached <- df_out()[[dataset]]
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

    ae_explore <- reactive({
      req(df_out()[[dataset]])
      req(adsl())
      req(input$split_col)
      req(input$class)
      req(input$term)
      req(input$summ_var)

      df_adsl <- adsl() |>
        select(USUBJID, input$split_col) |>
        unique()

      logger::log_info("mod_adae_sev_tox_server: alt_data has
                         {nrow(df_adsl)} rows")

      df <- df_out()[[dataset]] |>
        left_join(df_adsl) |>
        filter(USUBJID %in% unique(df_adsl$USUBJID))

      out_df <- build_adae_by_sev_tox(
        adsl = df_adsl,
        df_adae = df,
        colsby = input$split_col,
        filter_cond = filt_react$filter_cond(),
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
      bindCache(
        list(
          adsl(), input$split_col, input$class, input$term,
          input$summ_var, input$view, filt_react$filter_cond()
        )
      ) |>
      bindEvent(list(adsl(), filt_react$trig_report(), input$run, input$view))

    mod_dt_table_server("dt_table_2",
      display_df = ae_explore
    )
  })
}
