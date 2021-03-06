% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recover_na.R
\name{recover_na}
\alias{recover_na}
\title{Recovers NA-values in data}
\usage{
recover_na(
  alaluokkadata,
  ylaluokkadata,
  alaluokka,
  ylaluokka,
  muuttuja,
  onlyAccurate = FALSE,
  randomizeRest = FALSE
)
}
\arguments{
\item{alaluokkadata}{data.frame, contains the data for which NAs are to be recovered.}

\item{ylaluokkadata}{data.frame, contains the date on one aggregation level above. This data
contains the additional information that is used to recovers NAs.}

\item{alaluokka}{character, is the name of the aggregation class in alaluokkadata and ylaluokkadata.}

\item{ylaluokka}{character, is the name of the aggregation class in ylaluokkadata and alaluokkadata.}

\item{muuttuja}{character, is the name of the variable for which NAs are to be recovered.}

\item{onlyAccurate}{logical, if TRUE, only replaces NA-values that can be recovered accurately.
Otherwise see \code{randomizeRest}. Defaults to FALSE.}

\item{randomizeRest}{logical, if TRUE, randomizes a value from 1:4 for those NA-values that cannot
be accurately recovered. If FALSE, evenly allocates the missing values as inferred from \code{ylaluokkadata}
to replace NA-values.}
}
\value{
data.frame Output is a data.frame alaluokkadata with NAs recovered.
}
\description{
Using information disaggregated at different levels recovers protected information: NAs
in cells that would otherwise have values 1-4. Options to impute NA-values that cannot
be perfectly recovered.
}
