testthat::test_that("Test wt_audio_scanner", {
  f <- wt_audio_scanner("/users/alexandremacphail/wildRtrax/tests/testthat/", file_type = "wav", extra_cols = TRUE)
  rows <- nrow(f)
  result <- (rows == 1)
  testthat::expect_no_error(result)
})
