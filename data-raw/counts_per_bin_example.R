# Example tibble in the right format to use in testing and examples, that has a
# gap (empty bins that aren't explicitly mentioned) before the final bin
# Loosely based on 2004 data.
counts_per_bin_example <- tibble::tibble(
  binCount = c(9743.8, 9097.0, 5872.5, 4105.1, 3046.1, 2706.6, 1451.3, 1990.0,
               1432.4, 366.1, 42),
  binMid = c(28:37, 40)) %>%
  dplyr::mutate(binMin = binMid - 0.5,
                binMax = binMid + 0.5)

counts_per_bin_example

use_data(counts_per_bin_example)
