#' Add Flags to ADAE
#'
#' @description `r lifecycle::badge("maturing")`
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
#' data(adsl)
#' data(adae)
#' adae_ <- add_adae_flags(adae)
#'
#' tbl <- select(adae_, c("USUBJID", setdiff(names(adae_), names(adae))))
#' slice_head(tbl, n = 5)
#'
add_adae_flags <- function(df) {
  ae_vars <- c(
    "AESER", "AESDTH", "AESLIFE", "AESHOSP",
    "AESDISAB", "AESCONG", "AESMIE", "AEACN", "AETOXGR"
  )

  new_vars <- setdiff(ae_vars, names(df))

  if (length(new_vars) > 0) {
    df <- map(setdiff(ae_vars, names(df)), \(x) mutate(df, !!x := "")) |>
      reduce(full_join)
  }

  df_out <- df |>
    mutate(
      SER = with_label(AESER == "Y", "Serious AE"),
      SAEFATAL = with_label(AESER == "Y" & AESDTH == "Y", "SAEs with fatal outcome"),
      SAELIFE = with_label(AESER == "Y" & AESLIFE == "Y", "Life-threatening SAEs"),
      SAEHOSP = with_label(AESER == "Y" & AESHOSP == "Y", "SAEs requiring hospitalization"),
      SAEDISAB = with_label(
        AESER == "Y" & AESDISAB == "Y",
        "SAEs resulting in substantial disruption of normal life functions"
      ),
      SAECONG = with_label(AESER == "Y" & AESCONG == "Y", "Congenital anomaly or birth defect"),
      SAEMIE = with_label(AESER == "Y" & AESMIE == "Y", "Other SAEs"),
      WD = with_label(
        AEACN == "DRUG WITHDRAWN",
        "AE leading to permanent discontinuation of study drug"
      ),
      WDSM = with_label(
        AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
        "AE leading to dose modification/interruption"
      ),
      AEINT = with_label(AEACN == "DRUG INTERRUPTED", "AE leading to interruption of study drug"),
      AERED = with_label(AEACN == "DOSE REDUCED", "AE leading to reduction of study drug"),
      AED = with_label(AEACN == "DOSE RATE REDUCED", "AE leading to dose delay of study drug"),
      AEMIE = with_label(AEACN == "DOSE INCREASED", "Other AEs"),
      CTC35 = with_label(str_to_sentence(AETOXGR) %in% c(
        "3", "4", "5",
        "Grade 3", "Grade 4", "Grade 5"
      ), "Grade 3-5 AE"),
      CTC45 = with_label(AETOXGR %in% c("4", "5", "Grade 4", "Grade 5"), "Grade 4/5 AE")
    ) |>
    select(-all_of(new_vars))

  df_out
}


#' Create ADAE Summary Table
#'
#' @description `r lifecycle::badge("maturing")`
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
#' data(adsl)
#' data(adae)
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
    df <- adae |>
      filter(toupper(TRTEMFL) == "Y")

    if (!is.null(filter_cond)) {
      df <- df |>
        filter(!!!parse_exprs(filter_cond))
    }

    req(nrow(df) > 0)

    ser_vars <-
      event_vars[which(str_sub(event_vars, 1, 3) == "SAE")]
    ae_vars <- event_vars[which(str_sub(event_vars, 1, 2) == "WD")]
    ds_vars <- event_vars[which(str_sub(event_vars, 1, 2) == "AE")]
    ctc_vars <-
      event_vars[which(str_sub(event_vars, 1, 3) == "CTC")]

    lyt <- basic_table(show_colcounts = TRUE) |>
      split_cols_by(var = trt_var, split_fun = drop_split_levels) |>
      add_overall_col(label = "All Patients") |>
      count_patients_with_flags(
        "USUBJID",
        flag_variables = var_labels(adae[, "SER"]),
        .indent_mods = 1L,
        denom = "N_col",
        table_names = "sae"
      )

    if (length(ser_vars) > 0) {
      lyt <- lyt |>
        count_patients_with_flags(
          "USUBJID",
          flag_variables = var_labels(adae[, ser_vars]),
          .indent_mods = 2L,
          denom = "N_col",
          table_names = "sae_fl"
        )
    }

    if (length(ae_vars) > 0) {
      lyt <- lyt |>
        count_patients_with_flags(
          var = "USUBJID",
          flag_variables = var_labels(adae[, ae_vars]),
          .indent_mods = 1L,
          denom = "N_col",
          table_names = "ae"
        )
    }

    if (length(ds_vars) > 0) {
      lyt <- lyt |>
        count_patients_with_flags(
          var = "USUBJID",
          flag_variables = var_labels(adae[, ds_vars]),
          .indent_mods = 2L,
          denom = "N_col",
          table_names = "ds"
        )
    }

    lyt <- lyt |>
      analyze_num_patients(
        vars = "USUBJID",
        .stats = "unique",
        .labels = c(unique = "Any AE"),
        .indent_mods = 1L,
        show_labels = "hidden"
      )

    if ("ASEV" %in% names(adae)) {
      lyt <- lyt |>
        count_occurrences_by_grade(
          var = "ASEV",
          show_labels = "hidden",
          .indent_mods = 2L
        )
    } else if ("AESEV" %in% names(adae)) {
      lyt <- lyt |>
        count_occurrences_by_grade(
          var = "AESEV",
          show_labels = "hidden",
          .indent_mods = 2L
        )
    }

    if (length(ctc_vars) > 0) {
      lyt <- lyt |>
        count_patients_with_flags(
          var = "USUBJID",
          flag_variables = var_labels(adae[, ctc_vars]),
          .indent_mods = 2L,
          denom = "N_col",
          table_names = "ctc"
        )
    }

    lyt <- lyt |>
      append_topleft(c("", "Adverse Events"))

    return(list(lyt = lyt, df_out = df))
  }

#' Adverse Events Table by Body System and Severity/Toxicity
#'
#' @description `r lifecycle::badge("maturing")`
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
#' data(adsl)
#' data(adae)
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
      filter(!!!parse_exprs(filter_cond)) |>
      filter(toupper(TRTEMFL) == "Y")
  } else {
    df_adae <- df_adae |>
      filter(toupper(TRTEMFL) == "Y")
  }

  req(nrow(df_adae) > 0)

  if (isTRUE(default_view)) {
    tab <- sev_tox_default(
      adsl = adsl,
      df_adae = df_adae,
      colsby = colsby,
      grade_val = grade_val,
      class_val = class_val,
      term_val = term_val
    )
  } else {
    lyt <- basic_table(show_colcounts = TRUE) |>
      split_cols_by(var = colsby, split_fun = drop_split_levels) |>
      add_overall_col(label = "All Patients") |>
      summarize_num_patients(
        var = "USUBJID",
        .stats = c("unique", "nonunique"),
        .labels = c(
          unique = str_glue("Total number of patients with at least one adverse event"),
          nonunique = str_glue("Total number of adverse events")
        )
      ) |>
      split_rows_by(
        class_val,
        child_labels = "visible",
        nested = TRUE,
        label_pos = "topleft",
        indent_mod = 1L,
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

#' Create ADAE Summary by Toxicity/Severity
#'
#' @inheritParams build_adae_by_sev_tox
#' @return rtables object of the ocmbined table
#' @noRd
#'
sev_tox_default <-
  function(adsl,
           df_adae,
           colsby,
           grade_val,
           class_val,
           term_val) {
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
        mutate(AESEVN = as.numeric(.data[[grade_val]]))
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
      cross_join(select(df_adae, all_of(c("USUBJID", colsby))), pre_ae) |>
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
      map(
        ~ sev_tox_by_cols(
          df_adae = df_adae,
          adae = adae,
          adsl = adsl,
          colsby = colsby,
          cols_val = .x,
          dummy_sev = dummy_sev,
          class_val = class_val,
          term_val = term_val,
          grade_val = grade_val
        )
      )

    tab_trt <- reduce(l1, cbind_rtables)
    tab_all <- sev_tox_by_all(
      df_adae = df_adae,
      adae = adae,
      adsl = adsl,
      dummy_sev = dummy_sev,
      class_val = class_val,
      term_val = term_val,
      grade_val = grade_val
    )

    tab <- cbind_rtables(tab_trt, tab_all)
  }

#' Severity/Toxicity by columns
#'
#' @inheritParams build_adae_by_sev_tox
#' @param adae Filtered `adae`
#' @param dummy_sev Dummy data containing Severity/Toxicity combinations
#' @param cols_val Unique values of columns
#'
#' @return Combined severity/toxicity table by Columns
#' @noRd
#'
sev_tox_by_cols <-
  function(df_adae,
           adae,
           adsl,
           colsby,
           cols_val,
           dummy_sev,
           class_val,
           term_val,
           grade_val) {
    df <- adae |>
      filter(.data[[colsby]] == cols_val) |>
      mutate(sp_labs = "n") |>
      mutate(!!colsby := as.character(!!sym(colsby)))

    df_adsl1 <- adsl |>
      filter(.data[[colsby]] == cols_val) |>
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
      split_cols_by(grade_val, split_fun = remove_split_levels("Missing")) |>
      split_rows_by(
        class_val,
        indent_mod = 1L,
        label_pos = "topleft",
        split_label = obj_label(df_adae[[class_val]])
      ) |>
      count_occurrences(term_val) |>
      append_varlabels(df_adae, term_val, indent = 2L) |>
      build_table(df, alt_counts_df = df_adsl)

    tab <- cbind_rtables(lyt1, lyt)
  }

#' Severity/Toxicity Count for entire population
#'
#' @inheritParams build_adae_by_sev_tox
#' @param adae Filtered `adae`
#' @param dummy_sev Dummy data containing Severity/Toxicity combinations
#'
#' @return Combined severity/toxicity table for entire population
#' @noRd
#'
sev_tox_by_all <-
  function(df_adae,
           adae,
           adsl,
           dummy_sev,
           class_val,
           term_val,
           grade_val) {
    df_all <- adae |>
      mutate(
        All = "n",
        Labs = paste0(
          "All Patients (N = ",
          length(unique(adsl[["USUBJID"]])), ")"
        )
      ) |>
      filter(.data[[grade_val]] != "Missing")

    df_adsl1_all <- adsl |>
      mutate(
        All = "n",
        Labs = paste0(
          "All Patients (N = ",
          length(unique(adsl[["USUBJID"]])), ")"
        )
      )

    df_adsl_all <- df_adsl1_all |>
      select(USUBJID, Labs) |>
      distinct_all() |>
      cross_join(dummy_sev)

    lyt1_all <- basic_table() |>
      split_cols_by(
        "All",
        split_fun = drop_split_levels,
        labels_var = "All"
      ) |>
      split_rows_by(class_val, split_fun = drop_split_levels) |>
      count_occurrences(term_val) |>
      build_table(
        df = df_all,
        alt_counts_df = df_adsl1_all
      )

    lyt_all <- basic_table() |>
      split_cols_by("Labs", split_fun = drop_split_levels) |>
      split_cols_by(
        grade_val,
        split_fun = drop_split_levels
      ) |>
      split_rows_by(
        class_val,
        indent_mod = 1L,
        label_pos = "topleft",
        split_fun = drop_split_levels,
        split_label = obj_label(df_adae[[class_val]])
      ) |>
      count_occurrences(term_val) |>
      append_varlabels(df_adae, term_val, indent = 2L) |>
      build_table(df_all, alt_counts_df = df_adsl_all)

    tab <- cbind_rtables(lyt1_all, lyt_all)
  }
