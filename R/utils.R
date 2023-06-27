#' Baseline Demographic and Clinical Characteristics
#'
#' @param title (`character`)\cr Title of the demographic table.
#' @param subtitle (`character`)\cr Subtitle of the demographic table.
#' @param footer (`character`)\cr Footer of the demographic table.
#' @param split_cols_by (`character`)\cr Arm variable used to split table into columns.
#' @param summ_vars (`vector of character`)\cr Variables from df to include in the table.
#'
#' @return A tabletree object.
#' @family generic
#' @keywords generic
#' @export
#'
#' @examples
#' lyt <- build_demog_characteristics_table(
#'   split_cols_by = "ARM",
#'   summ_vars = c("AGE", "RACE")
#' )
#'
#'
#' ## Use the tabletree object to within build_table
#' rtables::build_table(lyt, random.cdisc.data::cadsl)
#'
build_demog_characteristics_table <-
  function(title = "x.x: Study Subject Data",
           subtitle = c(
             "x.x.x: Demographic Characteristics",
             "Table x.x.x.x: Demographic Characteristics - Full Analysis Set"
           ),
           footer = "Source: ADSL DDMMYYYY hh:mm; Listing x.xx; SDTM package: DDMMYYYY",
           split_cols_by = "ARM",
           summ_vars = c("AGE", "SEX", "COUNTRY")) {
    lyt <- basic_table(
      title = title,
      subtitles = subtitle,
      prov_footer = footer,
      show_colcounts = TRUE
    ) |>
      split_cols_by(split_cols_by) |>
      add_overall_col("All Patients") |>
      summarize_vars(
        summ_vars,
        .stats = c(
          "n",
          "mean_sd",
          "se",
          "median",
          "range",
          "quantiles",
          "count_fraction"
        ),
        .labels = c(
          n = "n",
          mean_sd = "Mean, SD",
          se = "Standard Error",
          median = "Median",
          range = "Min-Max",
          quantiles = c("IQR")
        )
      ) |>
      append_topleft(c("", "Characteristic"))
  }


#' Create Generic Occurrence Table
#'
#' @param occ_df (`data.frame`)\cr Occurrence dataset (typically ADAE, ADMH etc)
#' required to build table.
#' @param filter_cond (`character`)\cr Filtering condition required for `occ_df`.
#' @param trt_var (`character`)\cr Arm variable used to split table into columns.
#' @param dataset (`character`)\cr Name of the dataset eg. `"cadae"`.
#' @param class_var (`character`)\cr Body system organ class variable.
#' @param term_var (`character`)\cr Preferred term variable from `occ_df` to include in the table.
#'
#' @return List containing table layout object of Generic Occurrence Table and
#'  filtered Occurrencedata
#' @export
#'
#' @family generic
#' @keywords generic
#'
#' @examples
#'
#' library(rtables)
#' library(tern)
#' library(dplyr)
#' adsl <- random.cdisc.data::cadsl
#' adae <- random.cdisc.data::cadae
#' adae <- filter(adae, SAFFL == "Y")
#'
#' lyt <- build_generic_occurrence_table(
#'   occ_df = adae,
#'   filter_cond = NULL,
#'   trt_var = "ARM",
#'   dataset = "cadae",
#'   class_var = "AESOC",
#'   term_var = "AEDECOD"
#' )
#' build_table(lyt = lyt$lyt, df = lyt$df_out, alt_counts_df = adsl)
#'
build_generic_occurrence_table <-
  function(occ_df,
           filter_cond = NULL,
           trt_var,
           dataset,
           class_var,
           term_var) {
    df <- occ_df
    if (!is.null(filter_cond)) {
      df <- occ_df |>
        filter(!!!parse_exprs(filter_cond))
    }

    lyt <- basic_table() |>
      split_cols_by(var = trt_var) |>
      add_colcounts() |>
      add_overall_col(label = "All Patients")

    if (dataset == "cadae") {
      lyt <- lyt |>
        summarize_num_patients(
          var = "USUBJID",
          .stats = c("unique", "nonunique"),
          .labels = c(
            unique = "Total number of patients with at least one event",
            nonunique = "Total number of events"
          )
        )
    }

    lyt <- lyt |>
      split_rows_by(
        class_var,
        label_pos = "topleft",
        split_label = obj_label(df[[class_var]]),
        split_fun = drop_split_levels
      ) |>
      summarize_num_patients(
        var = "USUBJID",
        .stats = "unique",
        .labels = c(unique = NULL)
      ) |>
      count_occurrences(vars = term_var) |>
      append_topleft(paste(" ", obj_label(df[[term_var]])))
    return(list(lyt = lyt, df_out = df))
  }
