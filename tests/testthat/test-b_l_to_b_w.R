test_that("b_l_to_b_w() and b_w_to_b_l() work", {
  expect_equal(b_l_to_b_w(-5.1, beta = 3),
               -2.366667)     # Though took from the function
  expect_equal(b_w_to_b_l(-2, beta = 3),
               -4)

  b_w_random <- runif(1, -5, 0)

  expect_equal(b_w_to_b_l(
    b_l_to_b_w(b_w_random, beta = 2.7),
    beta = 2.7),
    b_w_random)
})
