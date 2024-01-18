#' disposition UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_disposition_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      id = ns("box_disposition"),
      title = uiOutput(ns("table_title")),
      sidebar = boxSidebar(
        id = ns("disp_side"),
        background = "#EFF5F5",
        icon = icon("filter"),
        width = 35,
        div(
          accordion(
            id = ns("adsl_disp_accord"),
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
                ns("eos"),
                "End of Study Status",
                choices = NULL,
                selected = NULL,
                width = "100vw"
              ),
              selectInput(
                ns("eot"),
                "End of Treatment Status",
                choices = NULL,
                selected = NULL,
                width = "100vw"
              ),
              selectInput(
                ns("dcs_reas"),
                "Study Discontinuation Reason",
                choices = NULL,
                selected = NULL,
                width = "100vw"
              ),
              selectInput(
                ns("dct_reas"),
                "Treatment Discontinuation Reason",
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
        HTML("Abbreviations:<br>ITT: intention-to-treat<br>mITT: modified intention-to-treat
             <br>N: number of patients in treatment arm<br>n: number of patients in specified
             population or group"),
      div(shinycssloaders::withSpinner(mod_dt_table_ui(ns("dt_table_1")), color = "#3BACB6"),
        style = "overflow-x: scroll; height: 100vh;"
      )
    )
  )
}

#' disposition Server Functions
#'
#' @noRd
mod_disposition_server <- function(id, adsl, pop_fil) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(trig_report = FALSE)

    observe({
      req(adsl())
      logger::log_info("mod_dispositon_server: updating table options")

      trt_choices <-
        names(select(adsl(), setdiff(
          starts_with(c("ACT", "ARM", "TRT", "TR0", "TR1", "TR2")),
          ends_with(c("DTM", "DUR", "PN", "AN", "DT", "FL"))
        )))

      eos_vars <- names(select(adsl(), starts_with("EOSS")))
      eot_vars <- names(select(adsl(), starts_with("EOTS")))

      dcs_vars <- names(select(adsl(), contains(c("DCSREAS", "DCDECOD"))))
      dct_vars <- names(select(adsl(), contains(c("DCTREAS", "DCTDECOD"))))

      updateSelectInput(session,
        "split_col",
        choices = trt_choices,
        selected = trt_choices[1]
      )

      updateSelectInput(session,
        "eos",
        choices = eos_vars,
        selected = eos_vars[1]
      )

      updateSelectInput(session,
        "eot",
        choices = eot_vars,
        selected = eot_vars[1]
      )

      updateSelectInput(session,
        "dcs_reas",
        choices = dcs_vars,
        selected = dcs_vars[1]
      )

      updateSelectInput(session,
        "dct_reas",
        choices = dct_vars,
        selected = dct_vars[1]
      )
    }) |>
      bindEvent(adsl())

    observe({
      req(input$split_col != "")
      rv$trig_report <- TRUE
    })

    disp_df <- reactive({
      req(adsl())
      req(input$split_col != "")
      logger::log_info("mod_disposition_server: processed adsl has {nrow(adsl())} rows")

      lyt <- build_disp_table(
        adsl = adsl(),
        trt_var = input$split_col,
        eos_var = input$eos,
        eot_var = input$eot,
        dcs_reas = input$dcs_reas,
        dct_reas = input$dct_reas
      )

      logger::log_info("mod_disposition_server: disposition layout for display")

      return(list(
        out_df = lyt$df,
        alt_df = NULL,
        lyt = list(lyt$lyt[[1]], lyt$lyt[[2]])
      ))
    }) |>
      bindCache(list(
        adsl(),
        input$split_col,
        input$eos,
        input$eot,
        input$dcs_reas,
        input$dct_reas
      )) |>
      bindEvent(list(adsl(), rv$trig_report, input$run))

    output$table_title <- renderUI({
      req(disp_df())
      req(pop_fil())
      tags$strong(
        paste0(
          "Table 1.2 Patient Disposition; ",
          str_replace_all(str_to_title(attr(adsl()[[pop_fil()]], "label")), " Flag", "")
        )
      )
    })

    mod_dt_table_server("dt_table_1",
      display_df = disp_df
    )
  })
}
