#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  Sys.sleep(2)
  load_data <- mod_data_read_server("data_read_1")

  observe({
    req(load_data$df_read())
    domain <- c("adsl", "adae", "admh", "adcm", "advs", "adlb", "adeg")
    walk(seq_along(domain) + 2, function(x) {
      toggleState(
        selector = str_glue("#tab-Tab{x}"),
        condition = all(c(domain[x - 2], domain[1]) %in% names(load_data$df_read()))
      )
    })
    updateNavbarTabs(session, inputId = "navmenu", "Tab3")
  }) |>
    bindEvent(load_data$df_read())

  mod_data_preview_server(
    "data_preview_1",
    load_data$prev_data
  )

  adsl_filters <-
    mod_adsl_filters_server("adsl_filters_1",
      dataset = "adsl",
      load_data = load_data$df_read,
      filter_list = load_data$study_filters
    )

  filtered_adsl <-
    mod_process_adsl_server(
      "process_adsl_1",
      dataset = "adsl",
      df_out = load_data$df_read,
      global_filters = adsl_filters$filters,
      apply = adsl_filters$apply
    )

  mod_adsl_display_server("adsl_display_1",
    adsl = filtered_adsl
  )

  mod_adae_global_server(
    "adae_global_1",
    dataset = "adae",
    df_out = load_data$df_read,
    adsl = filtered_adsl,
    filters = reactive(load_data$adae_filters)
  )

  mod_adxx_bodsys_server(
    "admh_bodsys_1",
    dataset = "admh",
    df_out = load_data$df_read,
    adsl = filtered_adsl,
    filters = load_data$admh_filters
  )

  mod_adxx_bodsys_server(
    "adcm_bodsys_1",
    dataset = "adcm",
    df_out = load_data$df_read,
    adsl = filtered_adsl,
    filters = load_data$adcm_filters
  )

  mod_bds_analysis_server(
    "vitals_analysis_1",
    dataset = "advs",
    df_out = load_data$df_read,
    adsl = filtered_adsl,
    filters = reactive(load_data$advs_filters)
  )

  mod_bds_analysis_server(
    "lab_analysis_1",
    dataset = "adlb",
    df_out = load_data$df_read,
    adsl = filtered_adsl,
    filters = reactive(load_data$adlb_filters)
  )

  mod_bds_analysis_server(
    "ecg_analysis_1",
    dataset = "adeg",
    df_out = load_data$df_read,
    adsl = filtered_adsl,
    filters = reactive(load_data$adeg_filters)
  )
}
