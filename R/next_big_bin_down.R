##' Find the next bin break down for a given value and bin width
##'
##' Quick attempt, doesn't work properly, just do manually.
##'
##' @param a the value
##' @param b the bin width
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' @ontrun{
##' @
##' @}
next_big_bin_down <- function(a, b){
  half_b <- b/2
  ((a %/% half_b) - ((a %% half_b) %% 2) == 0) * half_b
}
