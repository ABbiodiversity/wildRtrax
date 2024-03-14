library(testthat)

Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
wt_auth(force = TRUE)

################################### Camera Test suite

test_that("Downloading CAM report", {
  abmi_amph_cam <- wt_download_report(391, 'CAM', 'main', FALSE)
  expect_true(!is.null(abmi_amph_cam))
})

##wt_summarise_cam
##wt_ind_detect
