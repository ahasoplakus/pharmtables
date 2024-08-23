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
           domain = "ADAE",
           logo = "head-side-cough") {
    ns <- NS(id)
    tagList(
      box(
        id = ns("box_adxx_bodsys"),
        title = uiOutput(ns("table_title")),
        sidebar = boxSidebar(
          id = ns("adxx_side_bodsys"),
          background = "#EFF5F5",
          icon = icon("table-cells"),
          width = 35,
          mod_filter_reactivity_ui(ns("filter_reactivity_1"), domain = domain, logo = logo),
          div(
            accordion(
              id = ns("bodsys_accord"),
              tagAppendAttributes(accordionItem(
                title = tags$strong("Table Display Options"),
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
        headerBorder = FALSE,
        width = 12,
        div(
          shinycssloaders::withSpinner(
            mod_dt_table_ui(ns(
              "dt_table_bodsys"
            )),
            color = "#3BACB6"
          ),
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
      logger::log_info("mod_adxx_bodsys_server: updating table options for {dataset}")

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
        sort(names(select(
          df, union(ends_with(c(
            "SOC", "BODSYS"
          )), starts_with("ATC"))
        )))
      term_choices <-
        names(select(df, ends_with(c(
          "DECOD", "TERM"
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

    xx_bodsys <- reactive({
      req(df_out()[[dataset]])
      req(adsl())
      req(input$split_col)
      req(input$class)
      req(input$term)

      df_adsl <- adsl() |>
        select(USUBJID, input$split_col) |>
        unique()

      logger::log_info("mod_adxx_bodsys_server: alt_data has {nrow(df_adsl)} rows")

      df <- df_out()[[dataset]] |>
        left_join(df_adsl) |>
        filter(USUBJID %in% unique(df_adsl$USUBJID))

      lyt <- build_generic_occurrence_table(
        occ_df = df,
        filter_cond = filt_react$filter_cond(),
        trt_var = input$split_col,
        dataset = dataset,
        class_var = input$class,
        term_var = input$term
      )

      return(list(
        out_df = lyt$df_out,
        alt_df = df_adsl,
        lyt = lyt$lyt
      ))
    }) |>
      bindCache(list(
        adsl(),
        dataset,
        input$split_col,
        input$class,
        input$term,
        filt_react$filter_cond()
      )) |>
      bindEvent(list(adsl(), filt_react$trig_report(), input$run))

    output$table_title <- renderUI({
      req(xx_bodsys())
      req(pop_fil())
      if (dataset == "adae") {
        text <- "Table 2.2 Summary of Adverse Events by Body System or Organ Class and
        Dictionary-Derived Term; "
      } else if (dataset == "admh") {
        text <- "Table 3.1 Summary of Medical History By Body System or Organ Class and
        Dictionary-Derived Term; "
      } else {
        text <- "Table 4.1 Summary of Concomitant Medications by Medication Class and
        Standardized Medication Name; "
      }
      tags$strong(
        paste0(
          text,
          str_replace_all(str_to_title(attr(adsl()[[pop_fil()]], "label")), " Flag", "")
        )
      )
    })

    mod_dt_table_server("dt_table_bodsys",
      display_df = xx_bodsys
    )
  })
}
