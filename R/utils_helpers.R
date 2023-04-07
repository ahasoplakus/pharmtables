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
    purrr::map(
      \(x) pickerInput(
        namespace(tolower(x)),
        label = x,
        choices = filter_values[[x]],
        selected = filter_values[[x]],
        multiple = TRUE,
        options = list(`actions-box` = TRUE, size = 10),
        choicesOpt = list(content = stringr::str_trunc(filter_values[[x]],
                                                       width = 20))
      )
    )
}
