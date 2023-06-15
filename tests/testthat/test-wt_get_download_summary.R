testthat::test_that("Testing APIs", {
  Sys.setenv(WT_USERNAME = "Guest", WT_PASSWORD = "Apple123")
  wt_auth()
  my_projects <- wt_get_download_summary(sensor_id = "ARU") %>%
    as_tibble() %>%
    unnest(project) %>%
    unnest(project_id) %>%
    unnest(sensorId) %>%
    unnest(tasks) %>%
    unnest(status)
  result <- (nrow(my_projects) == 68)
  expect_true(result)
})
