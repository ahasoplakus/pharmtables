#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  Sys.sleep(2)
  options(shiny.maxRequestSize = 4096 * 1024^2)

  load_data <- mod_data_read_server("data_read_1")

  observe({
    if (length(load_data$df_read()) < 1) {
      hide(selector = "#acc_st_filt")
    } else {
      show(selector = "#acc_st_filt")
    }
  })

  observe({
    req(load_data$df_read())
    domain <- c("adsl", "adae", "admh", "adcm", "advs", "adlb", "adeg")
    walk(seq_along(domain) + 1, function(x) {
      toggleState(
        selector = str_glue("#tab-Tab{x}"),
        condition = all(c(domain[x - 1], domain[1]) %in% names(load_data$df_read()))
      )
    })
    updateNavbarTabs(session, inputId = "navmenu", "Tab2")
  }) |>
    bindEvent(load_data$df_read())

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

  mod_adsl_server("adsl_1",
    adsl = filtered_adsl,
    pop_fil = eventReactive(filtered_adsl(), adsl_filters$filters()$pop)
  )

  mod_adae_global_server(
    "adae_global_1",
    dataset = "adae",
    df_out = load_data$df_read,
    adsl = filtered_adsl,
    filters = reactive(load_data$adae_filters),
    pop_fil = eventReactive(filtered_adsl(), adsl_filters$filters()$pop)
  )

  mod_occ_summary_server(
    "admh_bodsys_1",
    dataset = "admh",
    df_out = load_data$df_read,
    adsl = filtered_adsl,
    filters = load_data$admh_filters,
    pop_fil = eventReactive(filtered_adsl(), adsl_filters$filters()$pop)
  )

  mod_occ_summary_server(
    "adcm_bodsys_1",
    dataset = "adcm",
    df_out = load_data$df_read,
    adsl = filtered_adsl,
    filters = load_data$adcm_filters,
    pop_fil = eventReactive(filtered_adsl(), adsl_filters$filters()$pop)
  )

  mod_bds_analysis_server(
    "vitals_analysis_1",
    dataset = "advs",
    df_out = load_data$df_read,
    adsl = filtered_adsl,
    filters = reactive(load_data$advs_filters),
    pop_fil = eventReactive(filtered_adsl(), adsl_filters$filters()$pop)
  )

  mod_bds_analysis_server(
    "lab_analysis_1",
    dataset = "adlb",
    df_out = load_data$df_read,
    adsl = filtered_adsl,
    filters = reactive(load_data$adlb_filters),
    pop_fil = eventReactive(filtered_adsl(), adsl_filters$filters()$pop)
  )

  mod_bds_analysis_server(
    "ecg_analysis_1",
    dataset = "adeg",
    df_out = load_data$df_read,
    adsl = filtered_adsl,
    filters = reactive(load_data$adeg_filters),
    pop_fil = eventReactive(filtered_adsl(), adsl_filters$filters()$pop)
  )
}
