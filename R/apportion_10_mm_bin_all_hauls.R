##' Do the apportioning for all hauls with 10-mm bin data
##'
##' Determine and loop through all hauls with 10-mm bin data, calling
##'  `apportion_10_mm_bin()`
##'  for each haul. The input for the 1-mm bins depends on whether that haul
##'  also has 1-mm data or not. If not then use the full data (assumed to be at the year level).
##'
##' @param counts_per_bin_per_set_id_one_year tibble for just one year
##' @param xmin min `x` that we will fit the ISD over; needed here for partitioning
##'
##' @return tibble of `counts_per_bin_per_set_id_one_year` with the apportioned
##'   10-mm bins replacing the appropriate pre-apportioned ones. Might not be in
##'   exact same order as before.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
apportion_10_mm_bin_all_hauls <- function(counts_per_bin_per_set_id_one_year,
                                          xmin){
  id_with_10_mm <- counts_per_bin_per_set_id_one_year %>%
    filter(resolution == 10) %>%
    select(id) %>%
    unique() %>%
    pull

  counts_per_bin_in_sets_with_10_mm <- filter(counts_per_bin_per_set_id_one_year,
                                              id %in% id_with_10_mm)

  id_with_1_mm_and_10_mm <- filter(counts_per_bin_in_sets_with_10_mm,
                                   resolution == 1)$id %>%
                                                  unique()

  id_with_10_mm_but_no_1_mm <- setdiff(id_with_10_mm,
                                       id_with_1_mm_and_10_mm)

  # See commit ca22b9f for what I had using grep and sub on "_B", now have the
  # simpler code above as now kept resolution column

  # This will get filled in for each haul
  counts_per_10_mm_bin_all_apportioned <- tibble()

  # Do ids with 1 and 10 mm first:
  for(i in 1:length(id_with_1_mm_and_10_mm)){
    counts_per_bin_this_haul <- filter(counts_per_bin_in_sets_with_10_mm,
                                       id == id_with_1_mm_and_10_mm[i])

    # Get the 10-mm apportioned calculation for this haul
    counts_per_10_mm_bin_this_haul_apportioned <-
      apportion_10_mm_bin(counts_per_bin_this_haul %>% filter(resolution == 1),
                          counts_per_bin_this_haul %>% filter(resolution == 10),
                          xmin = xmin)

    counts_per_10_mm_bin_all_apportioned <- bind_rows(
      counts_per_10_mm_bin_all_apportioned,
      counts_per_10_mm_bin_this_haul_apportioned)
  }

  # Now do ids 10 mm but no 1 mm, need a different input proportion. Had thought
  # that since it's the same for all these could calculate first and add as a new
  # argument, but apportion_10_mm_bin() already deals with when xmin not in a 10-mm
  # bin, so just leave the repeated calculation (not a time-consuming calculation)

  for(i in 1:length(id_with_10_mm_but_no_1_mm)){
    counts_per_bin_this_haul <- filter(counts_per_bin_in_sets_with_10_mm,
                                       id == id_with_10_mm_but_no_1_mm[i])

    # Get the 10-mm apportioned calculation for this haul
    counts_per_10_mm_bin_this_haul_apportioned <-
      apportion_10_mm_bin(counts_per_bin_1_mm_one_year,    # Use the full years
                          counts_per_bin_this_haul %>% filter(resolution == 10),
                          xmin = xmin)

    counts_per_10_mm_bin_all_apportioned <- bind_rows(
      counts_per_10_mm_bin_all_apportioned,
      counts_per_10_mm_bin_this_haul_apportioned)
  }

  # Easiest just to replace all the 10-mm ones with apportioned ones (any that
  # didn't change still won't have changed).

  counts_per_bin_per_set_id_one_year_apportioned <-
    counts_per_bin_per_set_id_one_year %>%
    filter(resolution == 1) %>%
    bind_rows(counts_per_10_mm_bin_all_apportioned) %>%
    arrange(id,
            resolution,
            wmin)

  return(counts_per_bin_per_set_id_one_year_apportioned)
}
