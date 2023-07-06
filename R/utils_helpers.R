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
#' @param label Label of Flags
#'
#' @return radio button widget for analysis population
#'
#' @noRd
create_flag_widget <- function(flags, namespace, label = "Population Flags") {
  prettyRadioButtons(
    namespace("pop"),
    label = label,
    choices = flags,
    selected = flags[1],
    animation = "pulse",
    status = "info",
    shape = "curve"
  )
}

#' Create filtering condition based on filters
#'
#' @param filter_list (`list`)\cr Named list of filter values
#'
#' @return The Filtering condition
#'
#' @family helpers
#' @keywords helpers
#' @export
#'
#' @examples
#'
#' filter_list <- list(SEX = c("F", "M"))
#' filters_to_cond(filter_list)
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
  return(filter_cond)
}
