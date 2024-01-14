##' Proportionally split a 10-mm count in which `xmin` falls
##'
##' Having already determined `xmin` based on just 1-mm counts, look for the
##' 10-mm bin that includes `xmin` and proportionally split its counts based on
##' the 1-mm bin counts.
##'
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
##' }
apportion_10_mm_bin <- function(counts_per_bin_1_mm,
                             counts_per_bin_10_mm,
                             xmin
                             ){
#hello
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
