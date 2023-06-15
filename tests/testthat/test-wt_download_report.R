testthat::test_that("Testing APIs", {
  Sys.setenv(WT_USERNAME = "Guest", WT_PASSWORD = "Apple123")
  wt_auth()
  my_project <- wt_download_report(project_id = 32, sensor_id = "ARU", report = "main", weather_cols = F) %>%
    as_tibble()
  result <- if ("project_id" %in% colnames(my_project)) {TRUE}
  expect_true(result)
})
