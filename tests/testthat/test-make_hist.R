test_that("make_hist works on simple example (doesn't test all the output though)", {
  res <- make_hist(counts_per_bin_example)

  expect_equal(res$counts,
               c(9743.8, 9097.0, 5872.5, 4105.1, 3046.1, 2706.6, 1451.3, 1990.0, 1432.4,  366.1,
                 0.0, 0.0, 42.0))
  expect_equal(res$breaks,
               seq(27.5, 40.5, 1))
  expect_error(make_hist(counts_per_bin_example,
                         bin_width = 2))
})
