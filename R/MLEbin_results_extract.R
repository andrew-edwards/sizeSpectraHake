##' Extract MLEbin results for each year from a `res_all_years` list
##'
##' @param res_all_years list object from `fit_all_years()`
##' @return tibble of MLEbin results
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' MLEbin_results_extract(res_all_years) # See .Rmd
##' }
MLEbin_results_extract <- function(res_all_years){
  MLEbin_tibble <- tibble::tibble(
                             year = unlist(lapply(res_all_years, '[[',
                                                  "year")),
                             xmin = unlist(lapply(res_all_years, '[[',
                                                  "xmin")),
                             xmax = unlist(lapply(res_all_years, '[[',
                                                  "xmax")),
                             n = unlist(lapply(res_all_years, '[[', "n")),
                             b_l = unlist(lapply(res_all_years, '[[',
                                                 "b_l")),
                             b_l_confMin = unlist(lapply(res_all_years,
                                                         '[[',
                                                         "b_l_confMin")),
                             b_l_confMax = unlist(lapply(res_all_years,
                                                         '[[',
                                                         "b_l_confMax"))) %>%
    mutate(b_w = b_l_to_b_w(b_l),
           b_w_confMin = b_l_to_b_w(b_l_confMin),
           b_w_confMax = b_l_to_b_w(b_l_confMax),
           b_l_stdErr = (abs(b_l_confMin - b_l) + abs(b_l_confMax -
                                                      b_l))/(2*1.96))
  return(MLEbin_tibble)
}
