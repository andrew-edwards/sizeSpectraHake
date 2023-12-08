##' Plot hake spectra results
##'
##' @param res list of class `hake_spectra_results` as output from `fit_all_years()`
##' @param years years to be plotted (if `NULL` then do all years)
##' @param ... arguments to passed onto `sizeSpectra::ISD_bin_plot_nonoverlapping()`
##' @return figure in current device, see .Rmd for multiple pages
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' See .Rmd
##' }
plot.hake_spectra_results <- function(res,
                                      years = NULL,
                                      ...){

  all_years <- unlist(lapply(res, '[[', "year"))

  if(is.null(years)){
    years <- all_years
  }

  # Get indices of all_years for years to be plotted
  years_indices <- which(all_years %in% years)

  # Global xlim
  xlim_global <- c(min(unlist(lapply(res, '[[',
                                     "xmin"))[years_indices]),
                   max(unlist(lapply(res, '[[',
                                   "xmax"))[years_indices]))

  mai_orig <- par("mai")

  for(i in years_indices){
    par(mai = mai_orig)     # Since gets reset by ISD_bin_plot(). Should clean
                            # up that function in sizeSpectra.

    border_col = "black"

    # Have to make the full histogram (with 0 counts here) to get the colours
    #  right, but ISD plot (I think) requires no 0 counts (maybe they get ignored).
    make_hist_full <- make_hist(res[[i]]$counts_per_bin,
                                bin_width = res[[i]]$bin_width)

    col_hist <- ifelse(make_hist_full$mids < min(res[[i]]$counts_per_bin_desc$binMid),
                       "grey",
                       "red")

    plot(make_hist_full,
         main = all_years[i],
         xlim = xlim_global,
         col = col_hist,
         border = col_hist)

    # Can delete TODO
#    plot(make_hist(res[[i]]$counts_per_bin_desc,
#                   bin_width = res[[i]]$bin_width),
#         add = TRUE,
#         col = "red",
#         border = border_col)

    ISD_bin_plot_nonoverlapping(binValsTibble = res[[i]]$counts_per_bin_desc,
                                b.MLE = res[[i]]$b_l,
                                b.confMin = res[[i]]$b_l_confMin,
                                b.confMax = res[[i]]$b_l_confMax,
                                yBig.inc = 10000,
                                xLab = "Body length (x), mm",
                                year = all_years[i],
                                xlim = xlim_global,
                                ...)
    par(mfrow = c(1,1))
  }
}
