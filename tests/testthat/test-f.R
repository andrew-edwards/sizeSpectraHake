test_that("f works", {
  expect_equal(f(1000.735, digits = 2),
               "1,000.74")
})
