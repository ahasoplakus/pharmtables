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
      cached_filters = NULL,
      adsl_filtered = NULL
    )

    observe(
      {
        req(df_out()[[dataset]])
        req(global_filters())
        req(none(global_filters(), is.null))
        run_cond <-
          !identical(df_out()[[dataset]], rv$cached_df) || !identical(
            global_filters(),
            rv$cached_filters
          )
        req(run_cond)
        logger::log_info("mod_process_adsl_server: loaded adsl has
                         {nrow(df_out()[[dataset]])} rows")
        filter_cond <- filters_to_cond(global_filters())
        rv$cached_df <- df_out()[[dataset]]
        rv$cached_filters <- global_filters()

        rv$adsl_filtered <- df_out()[[dataset]] |>
          filter(!!!parse_exprs(filter_cond))
        if (identical(rv$cached_filtered, rv$adsl_filtered)) {
          show_toast(
            "No data to update",
            text = "No change in data between current and previously applied filters",
            type = "warning",
            width = "600px"
          )
        }
        rv$cached_filtered <- rv$adsl_filtered
      },
      priority = 945
    ) |>
      bindEvent(list(apply(), df_out()))

    return(reactive(rv$adsl_filtered))
  })
}
