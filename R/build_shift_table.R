#' Shift Table
#'
#' @param adsl (`data.frame`)\cr `adsl` data set.
#' @param bds_df (`data.frame`)\cr `bds` data set eg. `adlb`, `advs`.
#' @param filter_cond (`character`)\cr Filtering condition required for `bds_df`.
#' @param trt_var (`character`)\cr  Name of the treatment variable from `adsl` for treatment
#' count, eg. `ARM`.
#' @param trt_label (`named vector of character`)\cr Label of `trt_var`.
#' @param group_var (`vector of characters`)\cr Names of variables for grouping in addition to
#' parameter. Default is `NULL`.
#' @param group_label (`named vector of character`)\cr Label of `group_var`.
#'
#' @return A Flextable object of the Shift Table
#' @export
#'
#' @examples
#' library(clinTables)
#' data(adsl)
#' data(adlb)
#'
#' build_shift_table(
#'   adsl = adsl,
#'   bds_df = adlb,
#'   trt_var = "ARM",
#'   trt_label = c(ARM = "Description of Planned Arm"),
#'   group_var = "AVISIT",
#'   group_label = c(AVISIT = "Analysis Visit")
#' )
#'
build_shift_table <-
  function(adsl,
           bds_df,
           filter_cond = NULL,
           trt_var,
           trt_label = NULL,
           group_var = NULL,
           group_label = NULL) {
    adsl_bign <- select(adsl, c("USUBJID", all_of(trt_var))) |>
      add_count(.data[[trt_var]], name = "N")

    df <- bds_df |>
      filter(!is.na(AVAL), BNRIND != "<Missing>") |>
      left_join(adsl_bign) |>
      mutate(!!trt_var := paste0(!!!syms(trt_var), " (N=", N, ")"))

    if (!is.null(filter_cond)) {
      df <- df |>
        filter(!!!parse_exprs(filter_cond))
    }

    if (nrow(df) < 1) {
      return(data.frame(NULL))
    }

    group_vars <- c(trt_var, "PARAMCD", group_var)

    # create a temp variable to rank records based on ANRIND and take the worst
    # within the group
    wpb <- df |>
      mutate(temp = case_when(
        toupper(ANRIND) %in% c("H", "HIGH") ~ 3,
        toupper(ANRIND) %in% c("N", "NORMAL") ~ 2,
        toupper(ANRIND) %in% c("L", "LOW") ~ 1,
        TRUE ~ 0
      )) |>
      group_by(!!!syms(group_vars), USUBJID) |>
      arrange(temp) |>
      filter(row_number() == n()) |>
      select(-temp) |>
      ungroup()

    dummy_anr <-
      tidyr::expand_grid(
        !!trt_var := unique(wpb[[trt_var]]),
        PARAM = intersect(levels(bds_df[["PARAM"]]), unique(wpb[["PARAM"]])),
        ANRIND = levels(bds_df[["ANRIND"]])
      )

    if (!is.null(group_var)) {
      group_anr <-
        map(group_var, \(x) tibble::tibble(!!x := intersect(
          levels(bds_df[[x]]),
          unique(df[[x]])
        ))) |>
        reduce(tidyr::expand_grid)
      dummy_anr <- cross_join(dummy_anr, group_anr)
    }

    dummy_anr <- dummy_anr |>
      cross_join(tibble::tibble(BNRIND = levels(bds_df[["ANRIND"]]))) |>
      filter(ANRIND != "<Missing>", BNRIND != "<Missing>")

    wpb_anr <- wpb |>
      bind_rows(mutate(wpb, BNRIND = "Total")) |>
      group_by(!!!syms(group_vars), BNRIND, ANRIND, N) |>
      count(PARAM, name = "CNT") |>
      ungroup() |>
      select(-PARAMCD) |>
      full_join(dummy_anr,
        by = c(trt_var, "PARAM", group_var, "ANRIND", "BNRIND")
      ) |>
      mutate(PCT = round(CNT / N * 100, 2)) |>
      select(-N)

    tab <- flextable::tabulator(
      x = wpb_anr,
      rows = c("PARAM", trt_var, group_var, "BNRIND"),
      columns = c("ANRIND"),
      `n` = flextable::as_paragraph(CNT),
      `%` = flextable::as_paragraph(PCT)
    )

    tab$data <-
      modify_if(tab$data, is.numeric, function(x) {
        tidyr::replace_na(x, 0)
      })

    ft <- flextable::as_flextable(
      x = tab,
      separate_with = "PARAM",
      label_rows = c(
        PARAM = "Parameter",
        c(trt_label, group_label),
        BNRIND = "Baseline Reference Range Indicator"
      )
    )

    ft
  }
