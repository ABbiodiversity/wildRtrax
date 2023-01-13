#' Internal functions
#'
#' WildTrax authentication
#'
#' @description Get Auth0 token and assign information to the hidden environment
#'
#' @import httr
#'
.wt_auth <- function() {

  # ABMI Auth0 client ID
  cid <- rawToChar(
    as.raw(c(0x45, 0x67, 0x32, 0x4d, 0x50, 0x56, 0x74, 0x71, 0x6b,
             0x66, 0x33, 0x53, 0x75, 0x4b, 0x53, 0x35, 0x75, 0x58, 0x7a, 0x50,
             0x39, 0x37, 0x6e, 0x78, 0x55, 0x31, 0x33, 0x5a, 0x32, 0x4b, 0x31,
             0x69)))

  # POST request using env
  r <- httr::POST(
    url = "https://abmi.auth0.com/oauth/token",
    encode = "form",
    body = list(
      audience = "http://www.wildtrax.ca",
      grant_type = "password",
      client_id = cid,
      username = Sys.getenv("WT_USERNAME"),
      password = Sys.getenv("WT_PASSWORD")
    )
  )

  u <- getOption("HTTPUserAgent")
  if (is.null(u)) {
    u <- sprintf("R/%s; R (%s)",
                 getRversion(),
                 paste(getRversion(), R.version$platform, R.version$arch, R.version$os))
  }

  # Add wildRtrax version information:
  u <- paste0("wildRtrax ", as.character(packageVersion("wildRtrax")), "; ", u)

  # Prepare temporary file:
  tmp <- tempfile(fileext = ".zip")
  # tmp directory
  td <- tempdir()

  # Write error message if POST request failed:
  if (httr::http_error(r))
    stop(sprintf(
      "Authentication failed [%s]\n%s",
      httr::status_code(r),
      httr::content(r)$error_description
    ),
    call. = FALSE)

  # Retrieve content from request
  x <- httr::content(r)

  # Time until token expires
  t0 <- Sys.time()
  x$expiry_time <- t0 + x$expires_in

  # Check that ._wt_auth_env_ exists
  if (!exists("._wt_auth_env_"))
    stop("Cannot find the correct environment.", call. = FALSE)

  # Send content ('x') to ._wt_auth_env_
  list2env(x, envir = ._wt_auth_env_)

  message("Authentication into WildTrax successful.")

  invisible(NULL)

}

#' Internal function to check if Auth0 token has expired
#'
#' @description Check if the Auth0 token has expired
#'
#'
.wt_auth_expired <- function () {

  if (!exists("._wt_auth_env_"))
    stop("Cannot find the correct environment.", call. = TRUE)

  if (is.null(._wt_auth_env_$expiry_time))
    return(TRUE)

  ._wt_auth_env_$expiry_time <= Sys.time()
}

#' An internal function to handle generic POST requests to WildTrax API
#'
#' @description Generic function to handle certain POST requests
#'
#' @param path The path to the API
#' @param ... Argument to pass along into POST query
#'
#' @import httr
#'
.wt_api_pr <- function(path, ...) {

  # Check if authentication has expired:
  if (.wt_auth_expired())
    stop("Please authenticate with wt_auth().", call. = FALSE)

  ## User agent
  u <- getOption("HTTPUserAgent")
  if (is.null(u)) {
    u <- sprintf("R/%s; R (%s)",
                 getRversion(),
                 paste(getRversion(), R.version$platform, R.version$arch, R.version$os))
  }
  # Add wildRtrax version information:
  u <- paste0("wildRtrax ", as.character(packageVersion("wildRtrax")), "; ", u)

  # POST request body
  r <- httr::POST(
    httr::modify_url("https://www-api.wildtrax.ca", path = path),
    query = list(...),
    httr::add_headers(Authorization = paste("Bearer", ._wt_auth_env_$access_token)),
    httr::user_agent(u))

  if (httr::http_error(r))
    stop(sprintf(
      "Authentication failed [%s]\n%s",
      httr::status_code(r),
      httr::content(r)$message),
      call. = FALSE)
  r

}

