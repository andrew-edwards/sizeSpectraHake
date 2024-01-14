##' Proportionally split a 10-mm count in which `xmin` falls
##'
##' Having already determined `xmin` based on just 1-mm counts, look for the
##' 10-mm bin that includes `xmin` and proportionally split its counts based on
##' the 1-mm bin counts. Code is pretty tailored to RREAS age-0 hake data, and
##' assumes 1-mm bins are, for example, 30.5 to 31.5, while 10-mm bins are 25 to
##' 35 (so assumes that the bin breaks for both resolutions do not coincide).
##' If not then need to adapt it.
##'
##' @param counts_per_bin_1_mm tibble of counts in the 1-mm bins; use the counts
##'   from the same haul as the 10-mm bins, if they exists (I if we have a set
##'   `B`), else use the counts for the full year, since have nothing else to go
##'   on. TODO write a function to come before this to determine the input; and
##'   also prob test sensitivity at some point.
##' @param counts_per_bin_10_mm tibble of counts in the 10-mm bins
##' @param xmin `xmin` to be used to fit the size spectrum, as already
##'   calculated in a previous function TODO
##' @return tibble that is `counts_per_bin_10_mm` with the bin that contains
##'   `xmin` appropriately_apportioned.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # see .Rmd.
##' # For a test that if xmin outside then returns the original 10-mm tibble,
##'  do something like:
##' test <- apportion_10_mm_bin(counts_per_bin_this_haul %>% filter(resolution == 1),
##'                         counts_per_bin_this_haul %>% filter(resolution == 10),
##'                         xmin = 1500)  # xmin way outside the bins
##' expect_equal(counts_per_bin_this_haul %>% filter(resolution == 10),
##'              test)
##' }
apportion_10_mm_bin <- function(counts_per_bin_1_mm,
                             counts_per_bin_10_mm,
                             xmin
                             ){

  stopifnot("Need only 1-mm resolution in counts_per_bin_1_mm input to apportion_10_mm_bin()" =
              unique(counts_per_bin_1_mm$resolution) == 1)
  stopifnot("Need only 10-mm resolution in counts_per_bin_10_mm input to apportion_10_mm_bin()" =
              unique(counts_per_bin_10_mm$resolution) == 10)

  # Which 10-mm bin contains xmin
  bin_10_mm_contains_xmin <- filter(counts_per_bin_10_mm,
                                    wmin < xmin,
                                    wmax > xmin)

  # Step 3, get the 1-mm bins fully contained in that 10-mm bin. Three possibilities:
  if(nrow(bin_10_mm_contains_xmin) > 1){
    stop("Should only be one 10_mm bin containing xmin")
  }

  if(nrow(bin_10_mm_contains_xmin) == 0){
    # Don't have to deal with the 10-mm bin (all the 10-mm bins might be higher,
    # for example)
    counts_per_bin_10_mm_this_haul_apportioned <- counts_per_bin_10_mm
  }

  if(nrow(bin_10_mm_contains_xmin) == 1){
    bin_1_mm_in_bin_10_mm_contains_xmin <- filter(counts_per_bin_1_mm,
                                                  wmin > bin_10_mm_contains_xmin$wmin,
                                                  wmax < bin_10_mm_contains_xmin$wmax)
    # Has to be at least one because xmin is a 1-mm wmin by defintion

    # Step 4, ratio of counts >/< xmin
    number_gt_xmin <- sum(filter(bin_1_mm_in_bin_10_mm_contains_xmin,
                                 wmin >= xmin)$number)
    number_lt_xmin <- sum(filter(bin_1_mm_in_bin_10_mm_contains_xmin,
                                 wmax <= xmin)$number)

    expect_equal(number_gt_xmin + number_lt_xmin,
                 sum(bin_1_mm_in_bin_10_mm_contains_xmin$number))

    prop_gt_xmin <- number_gt_xmin / (number_lt_xmin + number_gt_xmin)

    # Step 5
    # wmin to xmin bin (won't be needed for analysis but used for double
    # checking and plotting), this is a single row:
    new_bins_for_this_haul_1 <- bin_10_mm_contains_xmin
    new_bins_for_this_haul_1$wmax <- xmin
    new_bins_for_this_haul_1$number <- new_bins_for_this_haul_1$number *
      (1 - prop_gt_xmin)

    # xmin to wmax bin, that will get used in the fitting
    new_bins_for_this_haul_2 <- bin_10_mm_contains_xmin
    new_bins_for_this_haul_2$wmin <- xmin
    new_bins_for_this_haul_2$number <- new_bins_for_this_haul_2$number *
      prop_gt_xmin

    # Check the sum of new bins equal the old bin that's about to be replaced
    stopifnot(new_bins_for_this_haul_1$number +
              new_bins_for_this_haul_2$number ==
              filter(counts_per_bin_10_mm,
                     wmin == bin_10_mm_contains_xmin$wmin)$number)

    # Remove the old bin that is getting apportioned with the new two bins
    counts_per_bin_10_mm_this_haul_apportioned <- counts_per_bin_10_mm %>%
      filter(wmin != bin_10_mm_contains_xmin$wmin) %>%
      bind_rows(new_bins_for_this_haul_1,
                new_bins_for_this_haul_2) %>%
      arrange(wmin)
  }  # Ends if(nrow(bin_10_mm_contains_xmin) == 1)

  return(counts_per_bin_10_mm_this_haul_apportioned)
}




##'
##' This is what to functionalise, and then apply it to
##' `counts_per_bin_one_year_repeated_hauls` [providing they have 1 mm and 10 mm,
##' since 2022 and 2023 don't], and then to, I'm thinking, the other
##' hauls that have 30. Need to base those proportions on either the repeated hauls
##' ones or on the overall data for that year. May want to test sensitivity to these
##' choices.
##' Plan:
##' 1. pick xmin based on all-hauls 1-mm bins as before, will end in 0.5 by definition, e.g. 30.5
##' Do these for the repeated hauls only:
##' 2. determine which 10-mm bin contains that `xmin` call it `wmin^*` to `wmax^*` i.e. 25-35
##' 3. look at the 1-mm bins that are fully within that 10-mm bin, i.e. 25.5-26.5,
##' 26.5-27.5, ..., 33.5-34.5; there will be 9 of them.
##' 4. sum the counts in those bins that are `>xmin` and `<xmin`, work out the ratio of
##' `counts_>xmin / counts_<xmin`. Doing at haul level remember.
##' 5. Multiply that ratio by the total counts in the 10-mm bin to give a count for
##' the new bin, `xmin` to `wmax^*`,plus a count for the `wmin^*` to `xmin` bin.
##' 6. New bin's bin breaks are `xmin`, `wmax^*`.
##' 7. Remove 1-mm bins and 10-mm `<xmin`, then add the new bin.
