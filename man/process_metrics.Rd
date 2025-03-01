% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_metrics.R
\name{process_metrics}
\alias{process_metrics}
\title{Process metrics from bootstrap output}
\usage{
process_metrics(boot_stats, boot_method = "standard")
}
\arguments{
\item{boot_stats}{Output from the bootstrapping.}

\item{boot_method}{standard/blb.}
}
\value{
A data.table containing the processed metrics for each PGS.
}
\description{
Process metrics from bootstrap output
}
\keyword{internal}
