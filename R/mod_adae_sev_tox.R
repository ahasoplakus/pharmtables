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
      title = uiOutput(ns("table_title")),
      sidebar = boxSidebar(
        id = ns("adae_side"),
        background = "#EFF5F5",
        width = 35,
        icon = icon("filter"),
        mod_filter_reactivity_ui(ns("filter_reactivity_1")),
        div(
          accordion(
            id = ns("sevtox_accord"),
            tagAppendAttributes(accordionItem(
              title = tags$span(icon("table-cells"), tags$strong("Table Options")),
              collapsed = FALSE,
              selectInput(
                ns("split_col"),
                "Treatment Variable",
                choices = NULL,
                selected = NULL,
                width = "100vw"
              ),
              selectInput(
                ns("class"),
                "Higher Level Term",
                choices = NULL,
                selected = NULL,
                width = "100vw"
              ),
              selectInput(
                ns("term"),
                "Lower Level Term",
                choices = NULL,
                selected = NULL,
                width = "100vw"
              ),
              selectInput(
                ns("summ_var"),
                "Severity/Toxicity",
                choices = NULL,
                selected = NULL,
                width = "100vw"
              )
            ), class = "side_accord")
          ),
          style = "display: flex; justify-content: center;"
        ),
        fluidRow(
          div(
            tagAppendAttributes(actionButton(ns("run"), "Update"),
              class = "side_apply"
            ),
            style = "display: flex; justify-content: center; width: 100vw;"
          )
        )
      ),
      maximizable = TRUE,
      collapsible = FALSE,
      width = 12,
      headerBorder = FALSE,
      footer =
        HTML("AE: adverse event<br>MedDRA: Medical Dictionary for Regulatory
             Activities<br> N: number of patients in treatment arm<br>n: number of patients with
             at least one event<br> SAE: serious adverse event"),
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
                                    filters = reactive(NULL),
                                    pop_fil) {
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
            starts_with(c("ACT", "ARM", "TRT", "TR0", "TR1", "TR2")),
            ends_with(c("DTM", "DUR", "PN", "AN", "DT", "FL"))
          )
        ))
      class_choices <-
        names(select(df, union(ends_with(
          c("SOC", "BODSYS", "CAT")
        ), starts_with("ATC"))))
      term_choices <-
        names(select(df, ends_with(c(
          "DECOD", "TERM"
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

      lyt <- build_adae_by_sev_tox(
        adae = df,
        colsby = input$split_col,
        filter_cond = filt_react$filter_cond(),
        grade_val = input$summ_var,
        class_val = input$class,
        term_val = input$term
      )

      return(list(
        out_df = lyt$df_out,
        alt_df = df_adsl,
        lyt = lyt$lyt
      ))
    }) |>
      bindCache(
        list(
          adsl(),
          dataset,
          input$split_col,
          input$class,
          input$term,
          input$summ_var,
          filt_react$filter_cond()
        )
      ) |>
      bindEvent(list(adsl(), filt_react$trig_report(), input$run))

    output$table_title <- renderUI({
      req(ae_explore())
      req(pop_fil())
      tags$strong(
        paste0(
          "Table 2.3 Summary of Adverse Events by Body System or Organ Class,
               Dictionary-Derived Term and Severity/Toxicity; ",
          str_replace_all(str_to_title(attr(adsl()[[pop_fil()]], "label")), " Flag", "")
        )
      )
    })

    mod_dt_table_server("dt_table_2",
      display_df = ae_explore
    )
  })
}
