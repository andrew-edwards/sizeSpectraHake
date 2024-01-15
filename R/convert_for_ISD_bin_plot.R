##' Convert counts in bins to format needed for `ISD_bin_plot()`
##'
##' Need the `>=` sums etc. Taking some from `sizeSpectra` `MEPS_IBTS_recommend`
##' vignette. See `analysis-nov23-C.Rmd` for example.
##'
##' @param counts tibble that was used as an input to `MLEbins_one_year <-
##'   calcLike(negLL.fn = negLL.PLB.bins.species,, ...), with columns `wmin`,
##'   `wmax`, `Number`, `specCode`.
##' @return tibble of results needed for `ISD_bin_plot()`.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # See .Rmd. counts_per_bin_one_year_desc_rename
##' }
convert_for_ISD_bin_plot <- function(counts){

  counts_rename <- dplyr::arrange(counts,
                                  desc(wmin))
  sum_number <- sum(counts_rename$Number)

  # Have to do not with dplyr:
  wmin_vec <- counts_rename$wmin
  wmax_vec <- counts_rename$wmax
  num_vec <- counts_rename$Number

  count_gte_wmin <- rep(NA,
                        length(num_vec)) # to do a manual count
  low_count = count_gte_wmin
  high_count = count_gte_wmin

  for(iii in 1:length(count_gte_wmin))
  {
    count_gte_wmin[iii] <- sum( (wmin_vec >= wmin_vec[iii]) * num_vec)
    low_count[iii] <- sum( (wmin_vec >= wmax_vec[iii]) * num_vec)
    high_count[iii] <- sum( (wmax_vec >  wmin_vec[iii]) * num_vec)
  }

  counts_rename <- bind_cols(counts_rename,
                             "countGTEwmin" = count_gte_wmin,
                             "lowCount" = low_count,
                             "highCount" = high_count)

  return(counts_rename)
}
