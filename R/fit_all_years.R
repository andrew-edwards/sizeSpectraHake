##' Fit all the years of data using MLEbin method
##'
##'
##' @param raw_simp_prop input tibble of correct format (see .Rmd)
##' @param strata_to_analyse which strata to use, default is the full coast (excluding ones
##'   that should not be used)
##' @param min_year_to_analyse minimum year to analyse, if `NULL` (the default)
##'   then does the first year of data
##' @param max_year_to_analyse maximum year to analyse, if `NULL` (the default)

##' @param bin_width_each_year tibble of bin widths in each year, calculated
##'   earlier TODO could include as a data object in the package
##' @return list of class `hake_spectra_results` with one element for each year,
##'   each one containing:
##' \describe{
##' \item{year}
##' \item{bin_width}
##' \item{xmin}
##' \item{xmax}
##' \item{n}
##' \item{counts_per_bin}
##' \item{counts_per_bin_desc}{counts in the descending limb, including the
##'   peak}
##' \item{b_l}
##' \item{b_l_confMin}
##' \item{b_l_confMax}
##' }
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # see .Rmd file
##' res_all_strata <- fit_all_years(raw_simp_prop,
##'                     bin_width_each_year = bin_width_each_year)
##' }
fit_all_years <- function(raw_simp_prop,
                          strata_to_analyse = c("C", "NC", "S", "SC", "N"),
                          min_year_to_analyse = NULL,
                          max_year_to_analyse = NULL,
                          bin_width_each_year
                          ){
  full_years <- sort(unique(raw_simp_prop$year))

  if(is.null(min_year_to_analyse)){
    min_year_to_analyse <- min(full_years)
  }

  if(is.null(max_year_to_analyse)){
    max_year_to_analyse <- max(full_years)
  }
# browser()
  years_to_analyse <- min_year_to_analyse:max_year_to_analyse

  results <- list()        # All the results, each element of list corresponds
                           # to a list of results for that year


  for(i in 1:length(years_to_analyse)){
    bin_width <- filter(bin_width_each_year,
                        year == years_to_analyse[i]) %>%
      dplyr::pull(bin_width)

print(years_to_analyse[i])
    data_this_year <- filter(raw_simp_prop,
                             year == years_to_analyse[i],
                             strata %in% strata_to_analyse)
# Changed that %in% from ==, may have been while some earlier results were maybe
# wrong. Doesn't seem to have fixed it though. TODO remove once resolved
# browser()
    # Bins and the counts in each bin
    counts_per_bin <- summarise(group_by(data_this_year,
                                         x),
                                binCount = sum(scaled_counts)) %>%
      mutate(binMid = x,         # Assume these are midpoints, but TODO check
             binMin = binMid - bin_width/2,
             binMax = binMid + bin_width/2) %>%
      arrange(binMid) %>%
      select(-"x")

    # Check bin widths are what we prescribed in bin_width_each_year
    if(!expect_equal(min(diff(counts_per_bin$binMid)), bin_width)){
      stop(paste0("Double check the value in bin_width_each_year; may have to relax this condition; this failed for year ", years_to_analyse[i]))

      # Having no adjacent bins with values will cause this to fail, which seems
      # unlikely), but would want to manually look into and then tweak
      # condition. Does not fail for 2021 even with 0.01 mm bins.
    }

    max_ind <- which.max(counts_per_bin$binCount)

    ifelse(max_ind == 1,
           counts_per_bin_desc <- counts_per_bin,
           counts_per_bin_desc <- counts_per_bin[-(1:(max_ind-1)), ])

    vecDiff_use <- ifelse(years_to_analyse[i] == 2006 &
                          strata_to_analyse == "C",
                          200,
                          15)   # TODO add dataset == "nov23"  also, need to
                                # input into function

    MLEbin_res <-  sizeSpectra::calcLike(negLL.fn = sizeSpectra::negLL.PLB.binned,
                                         p = -1.5,
                                         w = c(dplyr::pull(counts_per_bin_desc,
                                                           binMin),
                                               dplyr::pull(counts_per_bin_desc,
                                                           binMax)[nrow(counts_per_bin_desc)]),
                                         # all minima plus max of final bin
                                         d = dplyr::pull(counts_per_bin_desc,
                                                         binCount),
                                         J = nrow(counts_per_bin_desc),   # = num.bins
                                         # suppress.warnings = TRUE,
                                         vecDiff = vecDiff_use)             # increase this if hit a bound

    results[[i]] <- list(
      year = years_to_analyse[i],
      bin_width = bin_width,
      xmin = min(counts_per_bin_desc$binMin),
      xmax = max(counts_per_bin_desc$binMax),
      n = sum(counts_per_bin_desc$binCount),
      counts_per_bin = counts_per_bin,
      counts_per_bin_desc = counts_per_bin_desc,
      b_l = MLEbin_res$MLE,
      b_l_confMin = MLEbin_res$conf[1],
      b_l_confMax = MLEbin_res$conf[2])
  }

  class(results) <- c("hake_spectra_results", class(results))
  return(results)
}
