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
        icon = icon("filter"),
        mod_filter_reactivity_ui(ns("filter_reactivity_1")),
        div(
          accordion(
            id = ns("summ_accord"),
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
              div(
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
                style = "overflow-x:scroll;"
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
      width = 12,
      height = "80vh",
      div(shinycssloaders::withSpinner(mod_dt_table_ui(ns("dt_table_ae_summ")), color = "#3BACB6"),
        style = "overflow-x: scroll; height: 70vh;"
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

    rv <- reactiveValues(occ_cached = NULL)

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
        select(USUBJID, setdiff(
          starts_with(c("ACT", "ARM", "TRT", "TR0", "TR1", "TR2")),
          ends_with(c("DTM", "DUR", "PN", "AN", "DT", "FL"))
        )) |>
        unique()

      logger::log_info("mod_adae_summary_server: alt_data has
                         {nrow(df_adsl)} rows")

      df <- df_out()[[dataset]] |>
        left_join(df_adsl) |>
        filter(USUBJID %in% unique(df_adsl$USUBJID))

      logger::log_info("mod_adae_summary_server: adae has
                         {nrow(df)} rows")

      df_ <- add_adae_flags(df_out()[[dataset]])

      aesi_vars <- setdiff(names(df_), names(df_out()[[dataset]]))
      aesi_vars <- aesi_vars[!aesi_vars %in% "SER"]
      df <- suppressMessages(inner_join(df, df_))
      labels <- var_labels(df[, aesi_vars])

      return(list(
        out_df = df,
        alt_df = df_adsl,
        labs = labels,
        aesi_vars = aesi_vars
      ))
    }) |>
      bindCache(list(adsl(), df_out()[[dataset]])) |>
      bindEvent(list(adsl(), df_out()[[dataset]]))

    observe({
      req(ae_summ_init())
      req(!identical(df_out()[[dataset]], rv$occ_cached))
      logger::log_info("mod_adae_summary_server: updating table options for {dataset}")

      df <- ae_summ_init()$out_df
      choices <- names(select(df, all_of(ae_summ_init()$aesi_vars)))
      selected <- choices
      labs <- as.character(ae_summ_init()$labs)
      trt_choices <-
        names(select(
          adsl(),
          setdiff(
            starts_with(c("ACT", "ARM", "TRT", "TR0", "TR1", "TR2")),
            ends_with(c("DTM", "DUR", "PN", "AN", "DT", "FL"))
          )
        ))

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

      rv$occ_cached <- df_out()[[dataset]]
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

      disp_eve <- ae_summ_init()$aesi_vars[ae_summ_init()$aesi_vars %in% input$events]

      logger::log_info("mod_adae_summary_server: creating {dataset} summary")

      lyt <- build_adae_summary(
        adae = ae_summ_init()$out_df,
        filter_cond = filt_react$filter_cond(),
        event_vars = disp_eve,
        trt_var = input$split_col
      )

      return(list(
        out_df = lyt$df_out,
        alt_df = ae_summ_init()$alt_df,
        lyt = lyt$lyt
      ))
    }) |>
      bindCache(
        list(
          adsl(),
          ae_summ_init(),
          dataset,
          input$split_col,
          input$events,
          filt_react$filter_cond()
        )
      ) |>
      bindEvent(list(adsl(), input$run, filt_react$trig_report()))

    mod_dt_table_server("dt_table_ae_summ",
      display_df = ae_summ
    )
  })
}
