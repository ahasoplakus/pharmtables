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
    purrr::set_names() |>
    purrr::map(\(x) if (is.factor(df[[dataset]][[x]])) {
      levels(unique(dplyr::pull(df[[dataset]], x)))
    } else {
      unique(dplyr::pull(df[[dataset]], x))
    })

  filter_list |>
    purrr::set_names() |>
    purrr::map(\(x)
               if (isTRUE(is.numeric(filter_values[[x]]))) {
                 sliderInput(
                   namespace(tolower(x)),
                   label = stringr::str_to_title(x),
                   min = min(filter_values[[x]], na.rm = TRUE),
                   max = max(filter_values[[x]], na.rm = TRUE),
                   value = max(filter_values[[x]], na.rm = TRUE)
                 )
               } else if (length(filter_values[[x]]) > 5) {
                 pickerInput(
                   namespace(tolower(x)),
                   label = stringr::str_to_title(x),
                   choices = filter_values[[x]],
                   selected = filter_values[[x]],
                   multiple = TRUE,
                   options = list(`actions-box` = TRUE, size = 10),
                   choicesOpt =
                     list(content = stringr::str_trunc(filter_values[[x]],
                                                                  width = 20))
                 )
               } else {
                 shinyWidgets::prettyCheckboxGroup(
                   namespace(tolower(x)),
                   label = stringr::str_to_title(x),
                   choices = filter_values[[x]],
                   selected = filter_values[[x]],
                   outline = TRUE,
                   animation = "pulse",
                   status = "info",
                   shape = "curve"
                 )
               })
}

#' Population Flag widget
#'
#' @param flags Population Flags
#' @param namespace namespace
#'
#' @return
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


#' Filter adsl by Study Filters
#'
#' @param df adsl dataset
#' @param pop Population flag
#' @param sex Sex
#' @param race Race
#' @param ethnic Ethnicity
#' @param country Country
#' @param age Age
#' @param siteid Site
#' @param usubjid Subject Identifier
#'
#' @return Filtered adsl
#' @export
#'
filter_adsl <-
  function(df,
           pop,
           sex,
           race,
           ethnic,
           country,
           age,
           siteid,
           usubjid) {
    if (pop == "SAFFL") {
      adsl <- df |>
        dplyr::filter(SAFFL == "Y")
    } else {
      adsl <- df |>
        dplyr::filter(ITTFL == "Y")
    }

    adsl <- adsl |>
      dplyr::filter(
        SEX %in% sex,
        RACE %in% race,
        ETHNIC %in% ethnic,
        COUNTRY %in% country,
        AGE <= age,
        SITEID %in% siteid,
        USUBJID %in% usubjid
      )
  }
