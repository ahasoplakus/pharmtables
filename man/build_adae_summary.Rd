% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adae_utils.R
\name{build_adae_summary}
\alias{build_adae_summary}
\title{Create ADAE Summary Table}
\usage{
build_adae_summary(adae, filter_cond = NULL, event_vars, trt_var)
}
\arguments{
\item{adae}{(\code{data.frame})\cr ADAE dataset.}

\item{filter_cond}{(\code{character})\cr Filtering condition required for \code{adae}.}

\item{event_vars}{(\verb{vector of character})\cr Variables added to source \code{ADAE}
by \code{add_adae_flags()}.}

\item{trt_var}{(\code{character})\cr Arm variable used to split table into columns.}
}
\value{
List containing layout object of ADAE Summary Table and filtered ADAE data
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#maturing}{\figure{lifecycle-maturing.svg}{options: alt='[Maturing]'}}}{\strong{[Maturing]}}
}
\examples{
suppressMessages(library(rtables))

adsl <- pharmaverseadam::adsl |> drop_missing_cols()
adae <- pharmaverseadam::adae |> drop_missing_cols()

adae_ <- suppressMessages(add_adae_flags(adae))
lyt <- build_adae_summary(
  adae = adae_,
  filter_cond = NULL,
  event_vars = setdiff(names(adae_), names(adae)),
  trt_var = "ARM"
)
tbl <- build_table(lyt = lyt$lyt, df = lyt$df_out, alt_counts_df = adsl)

\dontrun{
tt_to_flextable(tbl)
}

}
\seealso{
Other adae_utils: 
\code{\link{add_adae_flags}()},
\code{\link{build_adae_by_sev_tox}()}
}
\concept{adae_utils}
\keyword{adae_utils}
