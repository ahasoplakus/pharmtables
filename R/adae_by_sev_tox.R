#' Adverse Events Table by Body System and Severity/Toxicity
#'
#' @param adsl adsl data
#' @param df_adae adae data
#' @param colsby split columns by (default: ARM)
#' @param grade_val AE Severity or AE Toxicity Grade
#' @param class_val Sytem Organ Class/Body System Class
#' @param term_val Dictionary Derived Term
#' @param default_view Logical `TRUE` or `FALSE`
#'
#' @return An rtable object
#' @family helpers
#' @keywords helpers
#' @export
#'
#' @examples
#'
#' adsl <- random.cdisc.data::cadsl
#' adae <- random.cdisc.data::cadae
#'
#' ae_by_sev_tox_table <- adae_by_sev_tox(
#'   adsl = adsl,
#'   df_adae = adae,
#'   colsby = "ARM",
#'   grade_val = "AESEV",
#'   class_val = "AESOC",
#'   term_val = "AEDECOD",
#'   default_view = TRUE
#' )
#'
#' ae_by_sev_tox_table
#'
adae_by_sev_tox <- function(adsl,
                            df_adae,
                            colsby = "ARM",
                            grade_val = "AESEV",
                            class_val = "AESOC",
                            term_val = "AEDECOD",
                            default_view = TRUE) {
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
      mutate(!!grade_val := replace_na(!!sym(grade_val), "Missing")) |>
      left_join(adsl, by = c("USUBJID", colsby)) |>
      modify_if(is.character, as.factor)

    dummy_sev <- data.frame(x = unique(adae[[grade_val]]))
    names(dummy_sev) <- grade_val

    l1 <- levels(adsl[[colsby]]) |>
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
      split_cols_by(var = colsby) |>
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
