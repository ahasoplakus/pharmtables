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
      intersect(
        levels(unique(pull(df[[dataset]], x))),
        unique(pull(df[[dataset]], x))
      )
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
        choice_vals <- filter_values[[x]]
        choice_labs <-
          map_chr(
            choice_vals,
            \(x) if (nchar(x) > 50) {
              str_trunc(x, width = 50)
            } else {
              x
            }
          )

        checkbox <- prettyCheckboxGroup(
          namespace(tolower(x)),
          label = labs,
          choiceNames = choice_labs,
          choiceValues = choice_vals,
          selected = choice_vals,
          outline = TRUE,
          animation = "pulse",
          status = "info",
          shape = "curve"
        )

        if (any(nchar(choice_vals) > 25)) {
          checkbox <- div(checkbox, style = "overflow-x:scroll;")
        }
        checkbox
      }
    })
  do.call(tagList, filters)
}

#' Population Flag widget
#'
#' @param df data frame
#' @param flags Population Flags
#' @param namespace namespace
#' @param label Label of Flags
#'
#' @return radio button widget for analysis population
#'
#' @noRd
create_flag_widget <-
  function(df, flags, namespace, label = "Population Flags") {
    labs <- map_chr(
      map_chr(flags, \(x) {
        val <- obj_label(df[[x]])
        if (is.null(val)) {
          val <- x
        }
        val
      }),
      \(y) if (nchar(y) > 50) {
        str_trunc(y, width = 50)
      } else {
        y
      }
    )

    radio <- prettyRadioButtons(
      namespace("pop"),
      label = label,
      choiceNames = labs,
      choiceValues = flags,
      selected = ifelse("SAFFL" %in% flags, "SAFFL", flags[1]),
      animation = "pulse",
      status = "info",
      shape = "curve"
    )

    if (any(nchar(labs) > 25)) {
      radio <- div(radio, style = "overflow-x: scroll;")
    }
    radio
  }

#' Create filtering condition based on filters
#'
#' @param filter_list (`list`)\cr Named list of filter values
#'
#' @return The Filtering condition
#'
#' @noRd
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
        vals <- paste0(filter_list[[x]], collapse = '","')
        if (str_sub(x, start = -3) == "dtm") {
          vals <- str_glue('as.character({toupper(x)}) %in% c("{vals}")')
        } else {
          vals <- str_glue('{toupper(x)} %in% c("{vals}")')
        }
      } else {
        vals <- filter_list[[x]]
        vals <- str_glue('{vals} == "Y"')
      }
    } else {
      vals <- filter_list[[x]]
      vals <- str_glue("{toupper(x)} <= {vals}")
    }
  })

  filter_cond <- reduce(study_filters, paste, sep = " & ")
  return(filter_cond)
}

#' Convert selectInput choices to named list
#'
#' @param choices vector of choices
#' @param dataset data frame
#'
#' @return named list of choices
#'
#' @noRd
named_choice_list <- function(choices, dataset) {
  map(choices, \(x) x) |>
    set_names(map_chr(choices, \(x) str_glue("{x}: {obj_label({dataset}[[x]])}")))
}


#' Read data into a list
#'
#' @param data_path data path from `fileInput()`
#' @param data_name data name from `fileInput()`
#' @param data_list names of data files in `fileInput()`
#'
#' @return list of data frames
#' @noRd
#'
read_data_list <- function(data_path, data_name, data_list) {
  map(data_path, function(x) {
    if (all(tools::file_ext(data_name) == "sas7bdat")) {
      df <- haven::read_sas(x)
    } else {
      df <- readRDS(x)
    }
    df
  }) |>
    set_names(data_list)
}
