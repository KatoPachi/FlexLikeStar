% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reginfo.R
\name{lm.reginfo}
\alias{lm.reginfo}
\title{Get Statistics of Linear Regression}
\usage{
lm.reginfo(reg, keep.stat = NULL, df = NULL, digits = NULL)
}
\arguments{
\item{reg}{regression with class "lm"}

\item{keep.stat}{statistics you want to obtain}

\item{df}{logical value whether to show degree-of-freedom}

\item{digits}{maximum digits}
}
\value{
list: dataframe that only contains statistics of regressions, and names of extracting statistics
}
\description{
Get Statistics of Linear Regression
}
