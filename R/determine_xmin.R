##' Given counts in 1-mm bins, determine the mode and return min of that bin as xmin
##'
##' @param counts_per_bin_1_mm tibble containing columns `wmin`, `wmax, `number, `resolution`
##' @return xmin to use to fit ISD. Can recalculate the bins `>= xmin` later
##'   when needed, no need to return here
##' @export
##' @author Andrew Edwards
##' @examples
##' \dntrun{
##'
##' }
determine_xmin <- function(counts_per_bin_1_mm){
  stopifnot("Need only 1-mm resolution in determine_xmin()" =
              unique(counts_per_bin_1_mm$resolution) == 1)

  max_ind <- which.max(counts_per_bin_1_mm$number)

  if(max_ind == 1){
    counts_per_bin_desc_1_mm <- counts_per_bin_1_mm} else {
    counts_per_bin_desc_1_mm <- counts_per_bin_1_mm[-(1:(max_ind-1)), ]
  }
  xmin <- min(counts_per_bin_desc_1_mm$wmin)   # xmin for size spectrum analysis
  xmin
}
