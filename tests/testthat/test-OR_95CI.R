test_that("OR_95CI works", {
  expect_equal(OR_95CI(1, 0.05, 0.05,2), '2.72 (2.46, 3.00)')
})
