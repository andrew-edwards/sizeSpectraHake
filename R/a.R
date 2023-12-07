#' Shorthand for `as.data.frame()` for printing all of a tibble.
#'
#' @details Often want to print all of a (small) tibble, but `as.data.frame()`
#'   is cumbersome.
#'
#' @param tib `tibble` to print
#'
#' @return print `a` as a data.frame, so show all the rows
#' @author Andrew Edwards
#' @export
a <- function(tib){
  as.data.frame(tib)
}
