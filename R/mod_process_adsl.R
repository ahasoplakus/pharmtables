#' process_adsl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_process_adsl_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' process_adsl Server Functions
#'
#' @noRd
mod_process_adsl_server <- function(id,
                                    dataset,
                                    df_out,
                                    global_filters = NULL,
                                    apply) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      cached_df = NULL,
      cached_filters = NULL
    )

    adsl <- reactive({
      req(df_out()[[dataset]])
      req(!every(global_filters(), is.null))
      run_cond <-
        !identical(df_out()[[dataset]], rv$cached_df) || !identical(global_filters(), rv$cached_filters)
      req(run_cond)
      logger::log_info("mod_process_adsl_server: loaded adsl has
                         {nrow(df_out()[[dataset]])} rows")

      study_filters <- map(names(global_filters()), \(x) {
        if (!is.numeric(global_filters()[[x]])) {
          if (x != "pop") {
            vals <- paste0(global_filters()[[x]], collapse = "','")
            vals <- str_glue("{toupper(x)} %in% c('{vals}')")
          } else {
            vals <- global_filters()[[x]]
            vals <- str_glue("{vals} == 'Y'")
          }
        } else {
          vals <- global_filters()[[x]]
          vals <- str_glue("{toupper(x)} <= {vals}")
        }
      })

      filter_cond <- reduce(study_filters, paste, sep = " & ")
      rv$cached_df <- df_out()[[dataset]]
      rv$cached_filters <- global_filters()

      df <- df_out()[[dataset]] |>
        filter(!!!parse_exprs(filter_cond))
    }) |>
      bindEvent(list(apply(), df_out()))
  })
}
