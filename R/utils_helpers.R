#' Create Multiple Ui
#'
#' @param filter_list Names of variables for which ui inputs need to be created
#' @param df data frame to be passed
#' @param dataset dataset name
#' @param namespace namespace object
#'
#' @description A utils function to create ui for multiple inputs dynamically
#'
#' @return List of ui inputs
#'
#' @noRd
create_widget <- function(filter_list, df, dataset, namespace) {
  filter_values <-
    filter_list |>
    set_names() |>
    map(\(x) if (is.factor(df[[dataset]][[x]])) {
      levels(unique(pull(df[[dataset]], x)))
    } else {
      unique(pull(df[[dataset]], x))
    })

  filters <- filter_list |>
    map(\(x) {
      labs <- attr(df[[dataset]][[x]], "label")
      if (isTRUE(is.numeric(filter_values[[x]]))) {
        sliderInput(
          namespace(tolower(x)),
          label = labs,
          min = min(filter_values[[x]], na.rm = TRUE),
          max = max(filter_values[[x]], na.rm = TRUE),
          value = max(filter_values[[x]], na.rm = TRUE)
        )
      } else if (length(filter_values[[x]]) > 5) {
        pickerInput(
          namespace(tolower(x)),
          label = labs,
          choices = filter_values[[x]],
          selected = filter_values[[x]],
          multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10),
          choicesOpt =
            list(content = str_trunc(filter_values[[x]],
              width = 20
            ))
        )
      } else {
        prettyCheckboxGroup(
          namespace(tolower(x)),
          label = labs,
          choices = filter_values[[x]],
          selected = filter_values[[x]],
          outline = TRUE,
          animation = "pulse",
          status = "info",
          shape = "curve"
        )
      }
    })
  do.call(tagList, filters)
}

#' Population Flag widget
#'
#' @param flags Population Flags
#' @param namespace namespace
#'
#' @return radio button widget for analysis population
#'
#' @noRd
create_flag_widget <- function(flags, namespace) {
  prettyRadioButtons(
    namespace("pop"),
    label = "Population",
    choices = flags,
    selected = flags[1],
    animation = "pulse",
    status = "info",
    shape = "curve"
  )
}

#' Create filtering condition based on filters
#'
#' @param filter_list Named list of filter values
#'
#' @return The Filtering condition
#'
filters_to_cond <- function(filter_list) {
  study_filters <- map(names(filter_list), \(x) {
    if (!is.numeric(filter_list[[x]])) {
      if (x != "pop") {
        vals <- paste0(filter_list[[x]], collapse = "','")
        if (str_sub(x, start = -3) == "dtm") {
          vals <- str_glue("as.character({toupper(x)}) %in% c('{vals}')")
        } else {
          vals <- str_glue("{toupper(x)} %in% c('{vals}')")
        }
      } else {
        vals <- filter_list[[x]]
        vals <- str_glue("{vals} == 'Y'")
      }
    } else {
      vals <- filter_list[[x]]
      vals <- str_glue("{toupper(x)} <= {vals}")
    }
  })

  filter_cond <- reduce(study_filters, paste, sep = " & ")
}

#' Add Flags to ADAE
#'
#' @param df `ADAE` dataset
#'
#' @return `ADAE` dataset with added flags
#'
add_adae_flags <- function(df) {
  df <- df |>
    mutate(
      FATAL = AESDTH == "Y",
      SER = AESER == "Y",
      SERWD = AESER == "Y" & AEACN == "DRUG WITHDRAWN",
      SERDSM = AESER == "Y" & AEACN %in% c(
        "DRUG INTERRUPTED",
        "DOSE INCREASED", "DOSE REDUCED"
      ),
      RELSER = AESER == "Y" & AEREL == "Y",
      WD = AEACN == "DRUG WITHDRAWN",
      DSM = AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
      REL = AEREL == "Y",
      RELWD = AEREL == "Y" & AEACN == "DRUG WITHDRAWN",
      RELDSM = AEREL == "Y" & AEACN %in% c(
        "DRUG INTERRUPTED",
        "DOSE INCREASED", "DOSE REDUCED"
      ),
      CTC35 = AETOXGR %in% c("3", "4", "5"),
      CTC45 = AETOXGR %in% c("4", "5")
    ) |>
    var_relabel(
      FATAL = "AE with fatal outcome",
      SER = "Serious AE",
      SERWD = "Serious AE leading to withdrawal from treatment",
      SERDSM = "Serious AE leading to dose modification/interruption",
      RELSER = "Related Serious AE",
      WD = "AE leading to withdrawal from treatment",
      DSM = "AE leading to dose modification/interruption",
      REL = "Related AE",
      RELWD = "Related AE leading to withdrawal from treatment",
      RELDSM = "Related AE leading to dose modification/interruption",
      CTC35 = "Grade 3-5 AE",
      CTC45 = "Grade 4/5 AE"
    )
}
