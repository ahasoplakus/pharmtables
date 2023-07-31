#' Baseline Demographic and Clinical Characteristics
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param title (`character`)\cr Title of the demographic table.
#' @param subtitle (`character`)\cr Subtitle of the demographic table.
#' @param footer (`character`)\cr Footer of the demographic table.
#' @param split_cols_by (`character`)\cr Arm variable used to split table into columns.
#' @param summ_vars (`vector of character`)\cr Variables from df to include in the table.
#' @param disp_stat (`vector of character`)\cr Statistics to display.
#'
#' @return A tabletree object.
#' @family generic
#' @keywords generic
#' @export
#'
#' @examples
#'
#' library(clinTables)
#' data(adsl)
#' lyt <- build_adsl_chars_table(
#'   split_cols_by = "ARM",
#'   summ_vars = c("AGE", "RACE")
#' )
#' tbl <- rtables::build_table(lyt, adsl)
#'
#' \dontrun{
#' tt_to_flextable(tbl)
#' }
#'
build_adsl_chars_table <-
  function(title = "x.x: Study Subject Data",
           subtitle = c(
             "x.x.x: Demographic Characteristics",
             "Table x.x.x.x: Demographic Characteristics - Full Analysis Set"
           ),
           footer = "Source: ADSL DDMMYYYY hh:mm; Listing x.xx; SDTM package: DDMMYYYY",
           split_cols_by = "ARM",
           summ_vars = c("AGE", "SEX", "COUNTRY"),
           disp_stat = c("n", "mean_sd", "se", "median", "range", "quantiles", "count_fraction")) {
    lyt <- basic_table(
      title = title,
      subtitles = subtitle,
      prov_footer = footer,
      show_colcounts = TRUE
    ) |>
      split_cols_by(split_cols_by, split_fun = drop_split_levels) |>
      add_overall_col("All Patients") |>
      summarize_vars(
        summ_vars,
        .stats = disp_stat,
        .labels = c(
          n = "n",
          mean_sd = "Mean, SD",
          se = "Standard Error",
          median = "Median",
          range = "Min-Max",
          quantiles = c("IQR")
        )
      ) |>
      append_topleft(c("", "Baseline Characteristic"))
  }


#' Create Generic Occurrence Summary Table
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param occ_df (`data.frame`)\cr Occurrence dataset (typically ADAE, ADMH etc)
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
#' library(clinTables)
#' library(rtables)
#' library(tern)
#' library(dplyr)
#' data(adsl)
#' data(adae)
#' adae <- filter(adae, SAFFL == "Y")
#'
#' lyt <- build_generic_occurrence_table(
#'   occ_df = adae,
#'   filter_cond = NULL,
#'   trt_var = "ARM",
#'   dataset = "adae",
#'   class_var = "AESOC",
#'   term_var = "AEDECOD"
#' )
#' tbl <- build_table(lyt = lyt$lyt, df = lyt$df_out, alt_counts_df = adsl)
#'
#' \dontrun{
#' tt_to_flextable(tbl)
#' }
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

    if (dataset == "adae") {
      text <- "event"
    } else if (dataset == "admh") {
      text <- "condition"
    } else {
      text <- "treatment"
    }

    lyt <- basic_table(show_colcounts = TRUE) |>
      split_cols_by(var = trt_var, split_fun = drop_split_levels) |>
      add_overall_col(label = "All Patients") |>
      summarize_num_patients(
        var = "USUBJID",
        .stats = c("unique", "nonunique"),
        .labels = c(
          unique = str_glue("Total number of patients with at least one {text}"),
          nonunique = str_glue("Total number of {text}s")
        )
      ) |>
      split_rows_by(
        class_var,
        label_pos = "topleft",
        split_label = obj_label(occ_df[[class_var]]),
        indent_mod = 1L,
        split_fun = drop_split_levels
      ) |>
      summarize_num_patients(
        var = "USUBJID",
        .stats = "unique",
        .labels = c(unique = NULL)
      ) |>
      count_occurrences(vars = term_var) |>
      append_topleft(paste(" ", obj_label(occ_df[[term_var]])))

    return(list(lyt = lyt, df_out = df))
  }

#' Create generic BDS Summary table
#'
#' @description `r lifecycle::badge("maturing")`
#'
#' @param bds_df (`data.frame`)\cr BDS dataset (typically ADVS, ADLB etc)
#' @param filter_cond (`character`)\cr Filtering condition required for `bds_df`.
#' @param param (`character`)\cr BDS parameter value from `PARAM`
#' @param trt_var (`character`)\cr Arm variable used to split table into columns.
#' @param visit (`character`)\cr Visit variable name eg. `AVISIT`
#' @param timepoint (`character`)\cr Timepoint variable name eg. `ATPT`
#' @param disp_vars (`vector of characters`)\cr Variables to summarize
#'
#' @return List containing Generic BDS table layout and filtered BDS data
#' @export
#'
#' @family generic
#' @keywords generic
#'
#' @examples
#'
#' library(clinTables)
#' library(rtables)
#' data(adsl)
#' data(advs)
#'
#' lyt <- build_generic_bds_table(advs,
#'   param = "Diastolic Blood Pressure",
#'   trt_var = "ARM", visit = "AVISIT",
#'   disp_vars = c("AVAL", "CHG")
#' )
#' \dontrun{
#' tt_to_flextable(build_table(lyt = lyt$lyt, df = lyt$df_out, alt_counts_df = adsl))
#' }
#'
build_generic_bds_table <-
  function(bds_df,
           filter_cond = NULL,
           param,
           trt_var,
           visit = "AVISIT",
           timepoint = NULL,
           disp_vars) {
    df <- bds_df |>
      filter(PARAM %in% param)

    if (!is.null(filter_cond)) {
      df <- df |>
        filter(!!!parse_exprs(filter_cond))
    }

    vis_label <- obj_label(bds_df[[visit]])
    vis_levels <- df |>
      arrange(AVISITN) |>
      distinct(!!sym(visit)) |>
      pull(!!sym(visit)) |>
      as.character()

    df[[visit]] <- factor(df[[visit]], levels = vis_levels)
    var_labs <- map_chr(disp_vars, \(x) obj_label(bds_df[[x]]))

    lyt <- basic_table(show_colcounts = TRUE) |>
      split_cols_by(trt_var, split_fun = drop_split_levels) |>
      split_rows_by(
        visit,
        split_fun = drop_split_levels,
        indent_mod = 1L,
        label_pos = "topleft",
        split_label = vis_label
      )

    if (!is.null(timepoint) && timepoint %in% names(bds_df)) {
      lyt <- lyt |>
        split_rows_by(
          timepoint,
          split_fun = drop_split_levels,
          label_pos = "topleft",
          split_label = obj_label(bds_df[[timepoint]])
        )
    }

    lyt <- lyt |>
      split_cols_by_multivar(
        vars = disp_vars,
        varlabels = str_wrap(var_labs, 15)
      ) |>
      summarize_colvars(.labels = c(range = "Min - Max")) |>
      append_topleft(paste(" ", "Summary Statistic"))

    return(list(lyt = lyt, df_out = df))
  }
