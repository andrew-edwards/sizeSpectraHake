##' Example tibble in the right format to use in testing and examples,
##'
##' Has a gap (empty bins that aren't explicitly mentioned) before the final bin
##' for testing `make_hist()`, is loosely based on 2004 data.
##'
##' @format Tibble with self-explanatory columns:
##' \describe{
##' \item{binCount}
##' \item{binMid}
##' \item{binMin}
##' \item{binMax}
##' }
##' @examples
##' \dontrun{
##' counts_per_bin_example
##' make_hist(counts_per_bin_example)
##' }
##'
##' @author Andrew Edwards
##' @source Generated from running `data-raw/counts_per_bin_example.R`.
"counts_per_bin_example"
