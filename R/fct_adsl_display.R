#' adsl_display
#'
#' @param title Title of the demographic table
#' @param subtitle Subtitle of the demographic table
#' @param footer Footer of the demographic table
#' @param split_by Variable to split the table
#' @param summ_vars Summary variables
#'
#' @description A fct function to build the demographic table full set
#'
#' @return A tabletree object.
#' @export
#'
build_adsl <- function(title = "x.x: Study Subject Data",
                       subtitle = c(
                         "x.x.x: Demographic Characteristics",
                         "Table x.x.x.x: Demographic Characteristics - Full Analysis Set"
                       ),
                       footer = "Source: ADSL DDMMYYYY hh:mm; Listing x.xx; SDTM package: DDMMYYYY",
                       split_by = "ARM",
                       summ_vars = c("AGE", "SEX", "COUNTRY")) {
  lyt <- basic_table(title = title,
                     subtitles = subtitle,
                     prov_footer = footer,) |>
    split_cols_by(split_by) |>
    summarize_vars(summ_vars)
}
