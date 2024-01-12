##' Convert a tibble of counts per bin into a histogram list object and create 0
##' counts for missing bins.
##'
##' Can then use `plot()` which calls `plot.histogram()`. Without the 0 counts for missing bins
##' `plot.histogram()` does not plot counts because bins appear to have unequal widths.
##'
##' @param counts_per_bin tibble with columns `binCount`, `binMid`, `binMin`,
##'   `binMax`. Had thought about simplifying down to just `binMid`, but need to
##'   specify the bin endpoints (as empty bins don't usually get included
##'   here). Columns can also be just `wmin`, `wmax`, `number` and `resolution`
##'   (of the bin width) which is closer to the format needed for MLEbins. `make_hist()` just
##'   changes them internally to `binCount` etc., and calculates `binMid`.
##' @param bin_width bin width for the data set. Might not be
##'   `min(diff(counts_per_bin$binMid))` as that will missing `binMid` for empty
##'   bins, so need to create those here. Function does check that the data are
##'   consistent with `bin_width`, TODO i.e. TODO
##' @return a histogram list object with components (see `?hist`):
##'  - `breaks`
##'  - `mids`
##'  - `counts`
##'  - `xname`  TODO make this the default `"Body length (x), mm"`,
##'  - `equidist` TRUE since have equal bin widths
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' counts_per_bin_example
##' make_hist(counts_per_bin_example)
##' }
make_hist <- function(counts_per_bin,
                      bin_width = 1,
                      eps = 0.0000001){

  # Change the names here if they're the MLEbins format.
  if("wmax" %in% names(counts_per_bin)){
    counts_per_bin <- rename(counts_per_bin,
                             binCount = number,
                             binMin = wmin,
                             binMax = wmax) %>%
      mutate(binMid = (binMin + binMax)/2) # mean(c(binMin, binMax))) didn't
                                           # work
  }


  # Check the bin widths are compatible with (i.e. multiples of) bin_width
  # Problem is due to floating point resolution, the remainder might be
  # 0.9999*bin_width. So can't just look at the absolute remainder.

  bin_diffs_remainder <- diff(counts_per_bin$binMid) %% bin_width

  # That gives some just above 0, and some just below bin_width
  # (if bin_width = 1 don't think this happens).

  bin_diffs_remainder_not_zero <- which(bin_diffs_remainder > eps &
                                        bin_diffs_remainder < bin_width - eps)

  if(length(bin_diffs_remainder_not_zero) != 0){
    stop("Need counts_per_bin$binMid to all be multiples of bin_width")
  }

  all_bins <- tibble::tibble(binMid = seq(min(counts_per_bin$binMid),
                                          max(counts_per_bin$binMid),
                                          bin_width))

  counts_all <- dplyr::right_join(counts_per_bin,
                                  all_bins,
                                  by = c("binMid")) %>%
    dplyr::mutate(binMin = binMid - bin_width/2,
                  binMax = binMid + bin_width/2) %>%
    tidyr::replace_na(list(binCount = 0)) %>%
    dplyr::arrange(binMin)

  counts_list <- list(breaks = c(counts_all$binMin,
                                 max(counts_all$binMax)),
                      mids = counts_all$binMid,
                      counts = counts_all$binCount,
                      xname = "Body length (x), mm",
                      equidist = TRUE)

  class(counts_list) <- c("histogram", class(counts_list))

  counts_list
}
