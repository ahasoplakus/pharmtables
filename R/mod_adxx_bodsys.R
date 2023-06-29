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
           title = "Summary of Treatment-Emergent Adverse Events (TEAES) By Body System Class",
           domain = "ADAE") {
    ns <- NS(id)
    tagList(
      box(
        id = ns("box_adxx_bodsys"),
        title = tags$strong(title),
        sidebar = boxSidebar(
          id = ns("adxx_side_bodsys"),
          background = "#EFF5F5",
          width = 35,
          mod_filter_reactivity_ui(ns("filter_reactivity_1"), domain = domain),
          h2(tags$strong("Table Options")),
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
          tagAppendAttributes(actionButton(ns("run"), "Update"),
            class = "side_apply"
          )
        ),
        maximizable = TRUE,
        width = 12,
        height = "800px",
        div(
          withSpinner(
            mod_dt_table_ui(ns(
              "dt_table_bodsys"
            )),
            type = 6, color = "#3BACB6"
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

    observe({
      req(adsl())
      req(df_out()[[dataset]])
      logger::log_info("mod_adxx_bodsys_server: updating table options for {dataset}")

      df <- df_out()[[dataset]]

      trt_choices <-
        names(select(adsl(), setdiff(starts_with(
          c("ARM", "TRT0")
        ), ends_with("DTM"))))
      class_choices <-
        sort(names(select(
          df, union(ends_with(c(
            "SOC", "BODSYS"
          )), starts_with("ATC"))
        )))
      term_choices <-
        names(select(df, ends_with(c(
          "TERM", "DECOD"
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
        select(USUBJID, ends_with("ARM"), starts_with("TRT")) |>
        unique()

      logger::log_info("mod_adxx_bodsys_server: alt_data has {nrow(df_adsl)} rows")

      df <- df_out()[[dataset]] |>
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
        input$split_col,
        input$class,
        input$term,
        filt_react$filter_cond()
      )) |>
      bindEvent(list(adsl(), filt_react$trig_report(), input$run))

    mod_dt_table_server("dt_table_bodsys",
      display_df = xx_bodsys
    )
  })
}
