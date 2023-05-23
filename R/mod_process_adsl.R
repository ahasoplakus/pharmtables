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

    adsl <- reactive({
      req(df_out()[[dataset]])
      req(!every(global_filters(), is.null))
      logger::log_info("mod_process_adsl_server: loaded adsl has
                         {nrow(df_out()[[dataset]])} rows")

      df <- filter_adsl(
        df_out()[[dataset]],
        global_filters()$pop,
        global_filters()$sex,
        global_filters()$race,
        global_filters()$ethnic,
        global_filters()$country,
        global_filters()$age,
        global_filters()$siteid,
        global_filters()$usubjid
      )
      return(df)
    }) |>
      bindEvent(list(apply(), df_out()))
  })
}
