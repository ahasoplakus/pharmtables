% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_summary_utils.R
\name{build_disp_table}
\alias{build_disp_table}
\title{Disposition Summary Table}
\usage{
build_disp_table(
  adsl,
  trt_var,
  eos_var = "EOSSTT",
  eot_var = "EOTSTT",
  dcs_reas = "DCSREAS",
  dct_reas = "DCTREAS"
)
}
\arguments{
\item{adsl}{(\code{data.frame})\cr adsl data set.}

\item{trt_var}{(\code{character})\cr Arm variable used to split table into columns.}

\item{eos_var}{(\code{character})\cr Name of the \verb{end of study} variable, default is \code{"EOSSTT"}.}

\item{eot_var}{(\code{character})\cr Name of the \verb{end of treatment} variable, default is \code{"EOTSTT"}.}

\item{dcs_reas}{(\code{character})\cr Name of the \verb{study discontinuation reason} variable,
default is \code{"DCSREAS"}.}

\item{dct_reas}{(\code{character})\cr Name of the \verb{treatment discontinuation reason} variable,
default is \code{"DCTREAS"}.}
}
\value{
An \code{rtable} object of the \strong{Disposition Summary Table}
}
\description{
Disposition Summary Table
}
\examples{
library(rtables)

adsl <- pharmaverseadam::adsl |> drop_missing_cols()

tbl <- build_disp_table(
  adsl = adsl,
  trt_var = "ARM",
  eos_var = "EOSSTT",
  eot_var = "EOTSTT",
  dcs_reas = "DCSREAS",
  dct_reas = "DCTREAS"
)

tbl1 <- build_table(lyt = tbl$lyt[[1]], df = tbl$df)
tbl2 <- build_table(lyt = tbl$lyt[[2]], df = tbl$df)
col_info(tbl1) <- col_info(tbl2)
tt <- rbind(tbl1, tbl2)

\dontrun{
tt_to_flextable(tt)
}

}
\seealso{
Other generic: 
\code{\link{build_adsl_chars_table}()},
\code{\link{build_generic_bds_table}()},
\code{\link{build_generic_occurrence_table}()},
\code{\link{build_shift_table}()}
}
\concept{generic}
\keyword{generic}
