% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_total_catch.R
\name{get_total_catch}
\alias{get_total_catch}
\title{Get Total Catch}
\usage{
get_total_catch(
  pot_data,
  crab_data,
  ft_data,
  stock,
  by = NULL,
  lump = T,
  units = "t"
)
}
\arguments{
\item{pot_data}{NULL. Observer count pot data in the same format as the output of \code{load_pot_dump()}}

\item{crab_data}{NULL. Observer measure pot data in the same format as the output of \code{load_crab_dump()}}

\item{ft_data}{NULL. Fish ticket data with fields 'crab_year', 'fishery', 'effort' and whatever variables are specified in by.}

\item{stock}{NULL. Character string stock abbreviation: BSSC, WBT, EBT, BBRKC, EAG, WAG, PIGKC, SMBKC, PIBKC, PIRKC, WAIRKC.}

\item{by}{NULL. Vector of column names other than crab year, fishery, and group to use as a grouping variable.}

\item{lump}{T/F. Lump shell condition into new and old (TRUE) or not (FALSE). Default = TRUE.}

\item{units}{NULL. Unit of measurement, "t" or "lb". Default = "t".}
}
\value{
Data frame including crab year, fishery, grouping variables, effort, average weight, and total catch. Average weights used for expansion to weight are only based on crab year, fishery, and sex. No other 'by' variables are included here, so that the sum of total catch by variables (i.e., sex and shell condition) will equal the total catch by sex.
}
\description{
Estimate total catch from observer CPUE data
}
\examples{
get_avg_wt(pot_data, crab_data, ft_data, stock = "BBRKC")

}
