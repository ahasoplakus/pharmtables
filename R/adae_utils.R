#' Add Flags to ADAE
#'
#' @param df (`data.frame`)\cr ADAE dataset.
#'
#' @return `ADAE` dataset with added flags
#'
#' @family adae_utils
#' @keywords adae_utils
#'
#' @export
#' @examples
#' library(clinTables)
#' library(dplyr)
#' adsl <- random.cdisc.data::cadsl
#' adae <- random.cdisc.data::cadae
#' adae_ <- add_adae_flags(adae)
#'
#' tbl <- select(adae_, c("USUBJID", setdiff(names(adae_), names(adae))))
#' slice_head(tbl, n = 5)
#'
add_adae_flags <- function(df) {
  if (!any(c("AESER", "AEREL", "AEACN", "AESDTH") %in% names(df))) {
    return(df)
  }

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
      )
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
      RELDSM = "Related AE leading to dose modification/interruption"
    )

  if ("AETOXGR" %in% names(df)) {
    df <- df |>
      mutate(
        CTC35 = AETOXGR %in% c("3", "4", "5"),
        CTC45 = AETOXGR %in% c("4", "5")
      ) |>
      var_relabel(
        CTC35 = "Grade 3-5 AE",
        CTC45 = "Grade 4/5 AE"
      )
  }

  df
}


#' Create ADAE Summary Table
#'
#' @param adae (`data.frame`)\cr ADAE dataset.
#' @param filter_cond (`character`)\cr Filtering condition required for `adae`.
#' @param event_vars (`vector of character`)\cr Variables added to source `ADAE`
#' by `add_adae_flags()`.
#' @param trt_var (`character`)\cr Arm variable used to split table into columns.
#'
#' @return List containing layout object of ADAE Summary Table and filtered ADAE data
#' @export
#'
#' @family adae_utils
#' @keywords adae_utils
#'
#' @examples
#'
#' library(clinTables)
#' library(rtables)
#' adsl <- random.cdisc.data::cadsl
#' adae <- random.cdisc.data::cadae
#' adae_ <- add_adae_flags(adae)
#' lyt <- build_adae_summary(
#'   adae = adae_,
#'   filter_cond = NULL,
#'   event_vars = setdiff(names(adae_), names(adae)),
#'   trt_var = "ARM"
#' )
#' tbl <- build_table(lyt = lyt$lyt, df = lyt$df_out, alt_counts_df = adsl)
#'
#' \dontrun{
#' tt_to_flextable(tbl)
#' }
#'
build_adae_summary <-
  function(adae, filter_cond = NULL, event_vars, trt_var) {
    df <- adae
    if (!is.null(filter_cond)) {
      df <- adae |>
        filter(!!!parse_exprs(filter_cond))
    }

    lyt <- basic_table(show_colcounts = TRUE) |>
      split_cols_by(var = trt_var, split_fun = drop_split_levels) |>
      add_overall_col(label = "All Patients") |>
      count_patients_with_event(
        vars = "USUBJID",
        filters = c("STUDYID" = as.character(unique(adae[["STUDYID"]]))),
        denom = "N_col",
        .labels = c(count_fraction = "Total number of patients with at least one adverse event")
      ) |>
      count_values(
        "STUDYID",
        values = as.character(unique(adae[["STUDYID"]])),
        .stats = "count",
        .labels = c(count = "Total AEs"),
        table_names = "total_aes"
      ) |>
      count_patients_with_flags(
        "USUBJID",
        flag_variables = var_labels(adae[, event_vars]),
        denom = "N_col",
        var_labels = "Total number of patients with at least one",
        show_labels = "visible"
      ) |>
      append_topleft(c("", "Adverse Events"))
    return(list(lyt = lyt, df_out = df))
  }

#' Adverse Events Table by Body System and Severity/Toxicity
#'
#' @param adsl (`data.frame`)\cr ADSL dataset.
#' @param df_adae (`data.frame`)\cr ADAE dataset.
#' @param colsby (`character`)\cr Variable used to split table into columns.
#' @param filter_cond (`character`)\cr Filtering condition required for `df_adae`.
#' @param grade_val (`character`)\cr AE Severity or AE Toxicity Grade i.e. `AESEV` or `AETOXGR`
#' @param class_val (`character`)\cr System Organ Class/Body System Class i.e. `AESOC` or `AEBODSYS`
#' @param term_val (`character`)\cr Dictionary Derived Term i.e. `AEDECOD` or `AETERM`
#' @param default_view (`logical`)\cr `TRUE` displays values of `grade_val` into columns.
#'
#' @return An rtable object
#' @family adae_utils
#' @keywords adae_utils
#' @export
#'
#' @examples
#'
#' library(clinTables)
#' library(rtables)
#' adsl <- random.cdisc.data::cadsl
#' adae <- random.cdisc.data::cadae
#'
#' tbl <- build_adae_by_sev_tox(
#'   adsl = adsl,
#'   df_adae = adae,
#'   colsby = "ARM",
#'   grade_val = "AESEV",
#'   class_val = "AESOC",
#'   term_val = "AEDECOD",
#'   default_view = TRUE
#' )
#'
#' \dontrun{
#' tt_to_flextable(tbl)
#' }
#'
build_adae_by_sev_tox <- function(adsl,
                                  df_adae,
                                  colsby = "ARM",
                                  filter_cond = NULL,
                                  grade_val = "AESEV",
                                  class_val = "AESOC",
                                  term_val = "AEDECOD",
                                  default_view = TRUE) {
  if (!is.null(filter_cond)) {
    df_adae <- df_adae |>
      filter(!!!parse_exprs(filter_cond))
  } else {
    df_adae <- df_adae
  }

  if (isTRUE(default_view)) {
    adsl <- adsl |>
      add_count(.data[[colsby]]) |>
      mutate(colsby_lab = paste0(.data[[colsby]], " (N = ", n, ")")) |>
      select(all_of(c("USUBJID", colsby)), colsby_lab)

    if (grade_val == "AESEV") {
      adae <- df_adae |>
        mutate(
          AESEVN =
            case_when(
              .data[[grade_val]] == "MILD" ~ 1,
              .data[[grade_val]] == "MODERATE" ~ 2,
              TRUE ~ 3
            )
        )
    } else {
      adae <- df_adae |>
        mutate(AESEVN = as.numeric(AETOXGR))
    }

    adae <- adae |>
      group_by(USUBJID, .data[[colsby]], .data[[class_val]], .data[[term_val]]) |>
      filter(AESEVN == max(AESEVN)) |>
      ungroup()

    pre_ae <- df_adae |>
      select(all_of(c(class_val, term_val))) |>
      distinct_all() |>
      arrange(.data[[class_val]])

    pre_ae1 <-
      cross_join(select(df_adae, all_of(c(
        "USUBJID", colsby
      ))), pre_ae) |>
      distinct_all()

    adae <-
      full_join(adae, pre_ae1, by = c("USUBJID", colsby, class_val, term_val)) |>
      modify_if(is.factor, as.character) |>
      mutate(!!grade_val := tidyr::replace_na(!!sym(grade_val), "Missing")) |>
      left_join(adsl, by = c("USUBJID", colsby)) |>
      modify_if(is.character, as.factor)

    dummy_sev <- data.frame(x = unique(adae[[grade_val]]))
    names(dummy_sev) <- grade_val

    l1 <- levels(adae[[colsby]]) |>
      map(~ {
        df <- adae |>
          filter(.data[[colsby]] == .x) |>
          mutate(sp_labs = "N") |>
          mutate(!!colsby := as.character(!!sym(colsby)))

        df_adsl1 <- adsl |>
          filter(.data[[colsby]] == .x) |>
          mutate(!!colsby := as.character(!!sym(colsby)))

        df_adsl <- df_adsl1 |>
          select(all_of(c("USUBJID", colsby))) |>
          distinct_all() |>
          cross_join(dummy_sev)

        lyt1 <- basic_table() |>
          split_cols_by(colsby,
            labels_var = "sp_labs",
            split_fun = remove_split_levels("Missing")
          ) |>
          split_rows_by(class_val) |>
          count_occurrences(term_val) |>
          build_table(
            mutate(
              df,
              !!colsby := ifelse(.data[[grade_val]] == "Missing", "Missing", !!sym(colsby)),
              sp_labs = ifelse(.data[[grade_val]] == "Missing", "Missing", sp_labs)
            ),
            alt_counts_df = df_adsl1
          )


        lyt <- basic_table() |>
          split_cols_by(colsby, labels_var = "colsby_lab") |>
          split_rows_by(class_val,
            indent_mod = 1L,
            label_pos = "topleft",
            split_label = obj_label(df_adae[[class_val]])
          ) |>
          split_cols_by(grade_val, split_fun = remove_split_levels("Missing")) |>
          count_occurrences(term_val) |>
          append_varlabels(df_adae, term_val, indent = 2L) |>
          build_table(df, alt_counts_df = df_adsl)
        cbind_rtables(lyt1, lyt)
      })

    tab <- reduce(l1, cbind_rtables)
  } else {
    lyt <- basic_table() |>
      split_cols_by(var = colsby, split_fun = drop_split_levels) |>
      add_colcounts() |>
      add_overall_col(label = "All Patients") |>
      add_colcounts() |>
      summarize_num_patients("USUBJID") |>
      split_rows_by(
        class_val,
        child_labels = "visible",
        nested = TRUE,
        label_pos = "topleft",
        split_label = obj_label(df_adae[[class_val]]),
        split_fun = drop_split_levels
      ) |>
      split_rows_by(
        term_val,
        child_labels = "visible",
        nested = TRUE,
        label_pos = "topleft",
        split_label = obj_label(df_adae[[term_val]]),
        split_fun = drop_split_levels
      ) |>
      summarize_occurrences_by_grade(grade_val)

    tab <-
      build_table(
        df = df_adae,
        alt_counts_df = adsl,
        lyt = lyt
      )
  }
  return(tab)
}
