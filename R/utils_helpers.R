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
        shinyWidgets::prettyCheckboxGroup(
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
  shinyWidgets::prettyRadioButtons(
    namespace("pop"),
    label = "Population",
    choices = flags,
    selected = flags[1],
    animation = "pulse",
    status = "info",
    shape = "curve"
  )
}
