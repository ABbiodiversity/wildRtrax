library(testthat)
library(wildRtrax)

test_that("Authentication works correctly", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  # Add assertions to check if data processing is successful
  expect_true(!is.null(wt_get_download_summary(sensor_id = 'ARU')))
  })


test_that("Downloading report and processing data", {
  mpb <- wt_download_report(605, 'ARU', 'main', FALSE)
  # Add assertions to check if data processing is successful
  expect_true(!is.null(mpb))
})

test_that("Downloading report and processing data", {
  mpb_tidy <- wt_tidy_species(mpb, remove = c("mammal", "abiotic", "amphibians"))
  # Add assertions to check if data processing is successful
  expect_true(nrow(mpb_tidy) < nrow(mpb))
})

# mpb_tmtt <- wt_replace_tmtt(mpb_tidy)
#
# siteCovs <- mpb_tmtt %>%
#   select(location) %>%
#   distinct() %>%
#   mutate(tree = runif(n(), 0, 1)) %>%
#   arrange(location)
#
# my_occu_data <- wt_format_occupancy(mpb_tmtt,
#                                     species="OVEN",
#                                     siteCovs = siteCovs)
#
# my_occu_model <- occu(~ observer ~ tree,
#                       my_occu_data)
#
# my_occu_model@estimates@estimates[["det"]]
#
# mpb_wide <- wt_make_wide(mpb_tmtt, sound = "all")
# mpb_qpad <- wt_qpad_offsets(mpb_wide, species = "OVEN", version = 3, together = F, sensor = 'ARU')
# mpb_occu <- wt_format_occupancy(mpb_tidy, species = "OVEN")
# lm(OVEN ~ 1, offset=mpb_qpad$OVEN, data=mpb_wide)
#
