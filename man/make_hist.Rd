% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_hist.R
\name{make_hist}
\alias{make_hist}
\title{Convert a tibble of counts per bin into a histogram list object and create 0
counts for missing bins.}
\usage{
make_hist(counts_per_bin, bin_width = 1, eps = 1e-07)
}
\arguments{
\item{counts_per_bin}{tibble with columns \code{binCount}, \code{binMid}, \code{binMin},
\code{binMax}. Had thought about simplifying down to just \code{binMid}, but need to
specify the bin endpoints (as empty bins don't usually get included here).}

\item{bin_width}{bin width for the data set. Might not be
\code{min(diff(counts_per_bin$binMid))} as that will missing \code{binMid} for empty
bins, so need to create those here. Function does check that the data are
consistent with \code{bin_width}, TODO i.e. TODO}
}
\value{
a histogram list object with components (see \code{?hist}):
\itemize{
\item \code{breaks}
\item \code{mids}
\item \code{counts}
\item \code{xname}  TODO make this the default \code{"Body length (x), mm"},
\item \code{equidist} TRUE since have equal bin widths
}
}
\description{
Can then use \code{plot()} which calls \code{plot.histogram()}. Without the 0 counts for missing bins
\code{plot.histogram()} does not plot counts because bins appear to have unequal widths.
}
\examples{
\dontrun{
counts_per_bin_example
make_hist(counts_per_bin_example)
}
}
\author{
Andrew Edwards
}
