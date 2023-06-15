testthat::test_that("Test wt_chop", {
  f <- wt_audio_scanner("/users/alexandremacphail/wildRtrax/tests/testthat/", file_type = "wav", extra_cols = TRUE)
  wt_chop(input = f %>% slice(1), segment_length = 60, output = ".")
  df <- fs::dir_ls(".", regexp = ".wav") %>%
    as_tibble()
  rows <- nrow(df)
  result <- (rows == 11)
  testthat::expect_no_error(result)
}
)

