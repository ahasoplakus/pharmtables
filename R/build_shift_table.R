#' @title Create Shift Table
#'
#' @description `r lifecycle::badge("stable")`
#' Shift Tables show the progression of change from the baseline, with the progression often
#' being along time; the number of subjects is displayed in different range
#' (e.g. low, normal, or high) at baseline and at selected time points or intervals.
#'
#' @param adsl (`data.frame`)\cr `adsl` data frame.
#' @param bds_df (`data.frame`)\cr `bds` data frame eg. `adlb`, `advs`.
#' @param filter_cond (`character`)\cr Filtering condition required for `bds_df`.
#' @param trt_var (`character`)\cr  Name of the treatment variable from `adsl` for treatment
#' count, eg. `ARM`. Must be a vector of length `1`.
#' @param trt_label (`named vector of character`)\cr Label of `trt_var`. Must be a **named** vector
#' of length `1`.
#' @param group_var (`vector of characters`)\cr Names of variables for grouping in addition to
#' parameter. Default is `NULL`.
#' @param group_label (`named vector of characters`)\cr Label of `group_var`.
#' @param default_view (`logical`)\cr If `TRUE` (**default**), values of `trt_var` are shown in
#' rows, else in columns.
#'
#' @return A Flextable object of the Shift Table
#' @export
#'
#' @family generic
#' @keywords generic
#'
#' @examples
#' adsl <- pharmaverseadam::adsl |> drop_missing_cols()
#' adlb <- pharmaverseadam::adlb |> drop_missing_cols()
#'
#' \dontrun{
#' build_shift_table(
#'   adsl = adsl,
#'   bds_df = adlb,
#'   filter_cond = NULL,
#'   trt_var = "ARM",
#'   trt_label = c(ARM = "Description of Planned Arm"),
#'   group_var = NULL, ,
#'   default_view = FALSE
#' )
#' }
#'
build_shift_table <-
  function(adsl,
           bds_df,
           filter_cond = NULL,
           trt_var,
           trt_label = NULL,
           group_var = NULL,
           group_label = NULL,
           default_view = TRUE) {
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

    # create dummy dataset {dummy_anr} with all possible combinations of Treatment, Parameter,
    # ANRIND and additional grouping variables eg. AVISIT
    dummy_anr <-
      tidyr::expand_grid(
        !!trt_var := unique(wpb[[trt_var]]),
        PARAM = intersect(levels(as.factor(bds_df[["PARAM"]])), unique(wpb[["PARAM"]])),
        ANRIND = levels(as.factor(bds_df[["ANRIND"]]))
      )

    if (!is.null(group_var)) {
      group_anr <-
        map(group_var, \(x) {
          tibble::tibble(!!x := intersect(
            levels(as.factor(bds_df[[x]])),
            unique(df[[x]])
          ))
        }) |>
        reduce(tidyr::expand_grid)
      dummy_anr <- cross_join(dummy_anr, group_anr)
    }

    dummy_anr <- dummy_anr |>
      cross_join(tibble::tibble(BNRIND = c(levels(as.factor(bds_df[["ANRIND"]])), "Total"))) |>
      filter(!ANRIND %in% c("<Missing>", ""), !BNRIND %in% c("<Missing>", ""))

    # get the parameter counts and merge with {dummy_anr} to get all combinations
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

    wpb_anr <- wpb_anr |>
      modify_at(
        c("BNRIND", "ANRIND"),
        \(x) {
          case_match(x,
            "H" ~ "HIGH",
            "L" ~ "LOW",
            "N" ~ "NORMAL",
            .default = x
          )
        }
      )

    if (isTRUE(default_view)) {
      ft_rows <- c("PARAM", trt_var, group_var, "BNRIND")
      ft_cols <- c("ANRIND")
      ft_labs <- c(trt_label, group_label)
    } else {
      ft_rows <- c("PARAM", group_var, "BNRIND")
      ft_cols <- c(trt_var, "ANRIND")
      ft_labs <- group_label
    }

    tab <- flextable::tabulator(
      x = wpb_anr,
      rows = ft_rows,
      columns = ft_cols,
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
        ft_labs,
        BNRIND = "Baseline Reference Range Indicator"
      )
    )

    ft
  }
