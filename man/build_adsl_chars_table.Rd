% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_summary_utils.R
\name{build_adsl_chars_table}
\alias{build_adsl_chars_table}
\title{Baseline Demographic and Clinical Characteristics}
\usage{
build_adsl_chars_table(
  title = "",
  subtitle = character(),
  footer = character(),
  split_cols_by = "ARM",
  group_by = NULL,
  summ_vars = c("AGE", "SEX", "COUNTRY"),
  disp_stat = c("n", "mean_sd", "se", "median", "range", "quantiles", "count_fraction")
)
}
\arguments{
\item{title}{(\code{character})\cr Title of the demographic table.}

\item{subtitle}{(\code{character})\cr Subtitle of the demographic table.}

\item{footer}{(\code{character})\cr Footer of the demographic table.}

\item{split_cols_by}{(\code{character})\cr Arm variable used to split table into columns.}

\item{group_by}{(\code{character})\cr Additional Grouping Variables (max \code{2})}

\item{summ_vars}{(\verb{vector of character})\cr Variables from df to include in the table.}

\item{disp_stat}{(\verb{vector of character})\cr Statistics to display.}
}
\value{
A tabletree object.
}
\description{
Baseline Demographic and Clinical Characteristics
}
\examples{
adsl <- pharmaverseadam::adsl |> drop_missing_cols()

lyt <- build_adsl_chars_table(
  split_cols_by = "ARM",
  summ_vars = c("AGE", "RACE")
)
tbl <- rtables::build_table(lyt, adsl)

\dontrun{
tt_to_flextable(tbl)
}

}
\seealso{
Other generic: 
\code{\link{build_disp_table}()},
\code{\link{build_generic_bds_table}()},
\code{\link{build_generic_occurrence_table}()},
\code{\link{build_shift_table}()}
}
\concept{generic}
\keyword{generic}
