#' setup_filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_setup_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      div(HTML("Domain Specific Filters"), style = "display: inline-block; font-family: math; color: #000; font-size: 1.25rem; font-weight: bold;"),
      style = "justify-content: center; width: 360px;"
    ),
    br(),
    fluidRow(
      accordion(
        id = ns("acc_study_setup"),
        width = 6,
        tagAppendAttributes(accordionItem(
          title = "ADSL (Subject-Level)",
          selectizeInput(
            ns("adsl_var"),
            "",
            choices = NULL,
            selected = NULL,
            options = list(maxItems = 8)
          )
        ), class = "setup_accord"),
        tagAppendAttributes(accordionItem(
          title = "ADAE (Adverse Events)",
          selectizeInput(
            ns("adae_var"),
            "",
            choices = NULL,
            selected = NULL,
            options = list(maxItems = 8)
          )
        ), class = "setup_accord"),
        tagAppendAttributes(accordionItem(
          title = "ADMH (Medical History)",
          selectizeInput(
            ns("admh_var"),
            "",
            choices = NULL,
            selected = NULL,
            options = list(maxItems = 8)
          )
        ), class = "setup_accord"),
        tagAppendAttributes(accordionItem(
          title = "ADCM (Concomitant Medications)",
          selectizeInput(
            ns("adcm_var"),
            "",
            choices = NULL,
            selected = NULL,
            options = list(maxItems = 8)
          )
        ), class = "setup_accord"),
        tagAppendAttributes(accordionItem(
          title = "ADVS (Vital Signs)",
          selectizeInput(
            ns("advs_var"),
            "",
            choices = NULL,
            selected = NULL,
            options = list(maxItems = 8)
          )
        ), class = "setup_accord"),
        tagAppendAttributes(accordionItem(
          title = "ADLB (Laboratory Tests)",
          selectizeInput(
            ns("adlb_var"),
            "",
            choices = NULL,
            selected = NULL,
            options = list(maxItems = 8)
          )
        ), class = "setup_accord"),
        tagAppendAttributes(accordionItem(
          title = "ADEG (ECG Tests)",
          selectizeInput(
            ns("adeg_var"),
            "",
            choices = NULL,
            selected = NULL,
            options = list(maxItems = 8)
          )
        ), class = "setup_accord")
      )
    )
  )
}

#' setup_filters Server Functions
#'
#' @noRd
mod_setup_filters_server <- function(id, load_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(load_data()[["adsl"]])

      logger::log_info(
        "mod_setup_filters_server: setup adsl filters"
      )

      choices <-
        names(select(load_data()[["adsl"]], !ends_with("FL")))
      choice_list <-
        named_choice_list(choices, load_data()[["adsl"]])

      selected <-
        intersect(
          c("SEX", "RACE", "ETHNIC", "COUNTRY", "AGE", "SITEID", "USUBJID"),
          choices
        )

      updateSelectizeInput(
        session,
        "adsl_var",
        "",
        choices = choice_list,
        selected = selected,
        options = list(maxItems = 8)
      )
    })

    observe({
      req(load_data()[["adsl"]])
      req(load_data()[["adae"]])

      logger::log_info(
        "mod_setup_filters_server: setup adae filters"
      )

      choices <- setdiff(
        names(load_data()[["adae"]]),
        names(load_data()[["adsl"]])
      )
      choice_list <-
        named_choice_list(choices, load_data()[["adae"]])
      selected <- NULL

      updateSelectizeInput(
        session,
        "adae_var",
        "",
        choices = choice_list,
        selected = selected,
        options = list(maxItems = 8)
      )
    })

    observe({
      req(load_data()[["adsl"]])
      req(load_data()[["admh"]])

      logger::log_info(
        "mod_setup_filters_server: setup admh filters"
      )

      choices <- setdiff(
        names(load_data()[["admh"]]),
        names(load_data()[["adsl"]])
      )
      choice_list <-
        named_choice_list(choices, load_data()[["admh"]])
      selected <- NULL

      updateSelectizeInput(
        session,
        "admh_var",
        "",
        choices = choice_list,
        selected = selected,
        options = list(maxItems = 8)
      )
    })

    observe({
      req(load_data()[["adsl"]])
      req(load_data()[["adcm"]])

      logger::log_info(
        "mod_setup_filters_server: setup adcm filters"
      )

      choices <- setdiff(
        names(load_data()[["adcm"]]),
        names(load_data()[["adsl"]])
      )
      choice_list <-
        named_choice_list(choices, load_data()[["adcm"]])
      selected <- NULL

      updateSelectizeInput(
        session,
        "adcm_var",
        "",
        choices = choice_list,
        selected = selected,
        options = list(maxItems = 8)
      )
    })

    observe({
      req(load_data()[["adsl"]])
      req(load_data()[["advs"]])

      logger::log_info(
        "mod_setup_filters_server: setup advs filters"
      )

      choices <- setdiff(
        names(select(load_data()[["advs"]], !ends_with(c("AVAL", "AVALU", "SEQ")))),
        names(load_data()[["adsl"]])
      )
      exclude_vars <- names(select(load_data()[["advs"]], !contains("CHG")))
      choices <- intersect(exclude_vars, choices)
      choice_list <-
        named_choice_list(choices, load_data()[["advs"]])
      selected <- NULL

      updateSelectizeInput(
        session,
        "advs_var",
        "",
        choices = choice_list,
        selected = selected,
        options = list(maxItems = 8)
      )
    })

    observe({
      req(load_data()[["adsl"]])
      req(load_data()[["adlb"]])

      logger::log_info(
        "mod_setup_filters_server: setup adlb filters"
      )

      choices <- setdiff(
        names(select(load_data()[["adlb"]], !ends_with(c("AVAL", "AVALU", "SEQ")))),
        names(load_data()[["adsl"]])
      )
      exclude_vars <- names(select(load_data()[["adlb"]], !contains("CHG")))
      choices <- intersect(choices, exclude_vars)
      choice_list <-
        named_choice_list(choices, load_data()[["adlb"]])
      selected <- NULL

      updateSelectizeInput(
        session,
        "adlb_var",
        "",
        choices = choice_list,
        selected = selected,
        options = list(maxItems = 8)
      )
    })

    observe({
      req(load_data()[["adsl"]])
      req(load_data()[["adeg"]])

      logger::log_info(
        "mod_setup_filters_server: setup adeg filters"
      )

      choices <- setdiff(
        names(select(load_data()[["adeg"]], !ends_with(c("AVAL", "AVALU", "SEQ")))),
        names(load_data()[["adsl"]])
      )
      exclude_vars <- names(select(load_data()[["adeg"]], !contains("CHG")))
      choices <- intersect(choices, exclude_vars)
      choice_list <-
        named_choice_list(choices, load_data()[["adeg"]])
      selected <- NULL

      updateSelectizeInput(
        session,
        "adeg_var",
        "",
        choices = choice_list,
        selected = selected,
        options = list(maxItems = 8)
      )
    })

    return(list(
      adsl_filt = reactive(input$adsl_var),
      adae_filt = reactive(input$adae_var),
      admh_filt = reactive(input$admh_var),
      adcm_filt = reactive(input$adcm_var),
      advs_filt = reactive(input$advs_var),
      adlb_filt = reactive(input$adlb_var),
      adeg_filt = reactive(input$adeg_var)
    ))
  })
}
