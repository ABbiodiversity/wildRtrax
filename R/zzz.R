# Create environment to store Auth0 token
._wt_auth_env_ <- NULL

.onLoad <- function(...) {
  ._wt_auth_env_ <<- new.env(parent = .GlobalEnv)
}
