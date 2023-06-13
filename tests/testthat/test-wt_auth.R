test_that("Test wt_auth with Guest input", {
  Sys.setenv(WT_USERNAME = "Guest", WT_PASSWORD = "Apple123")
  result <- wt_auth()
  expect_no_error(result)
})
#> Test passed for wt_auth :fireworks:

