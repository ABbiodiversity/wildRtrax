
cid <- rawToChar(
  as.raw(c(0x45, 0x67, 0x32, 0x4d, 0x50, 0x56, 0x74, 0x71, 0x6b,
           0x66, 0x33, 0x53, 0x75, 0x4b, 0x53, 0x35, 0x75, 0x58, 0x7a, 0x50,
           0x39, 0x37, 0x6e, 0x78, 0x55, 0x31, 0x33, 0x5a, 0x32, 0x4b, 0x31,
           0x69)))

mytok <-  httr::content(httr::POST(
  url = "https://abmi.auth0.com/oauth/token",
  encode = "form",
  body = list(
    audience = "http://www.wildtrax.ca",
    grant_type = "password",
    client_id = cid,
    username = Sys.getenv("WT_USERNAME"),
    password = Sys.getenv("WT_PASSWORD")
  )
))

mypr <- function(path, ...) {


  u <- getOption("HTTPUserAgent")
  u <- paste0("wildRtrax ", as.character(packageVersion("wildRtrax")), "; ", u)

  r <- httr::POST(
    httr::modify_url("https://www-api.wildtrax.ca", path = path),
    query = list(...),
    httr::add_headers(Authorization = paste("Bearer", mytok$access_token)),
    httr::user_agent(u))

}

mydat <- mypr(
  path = "/bis/get-download-summary",
  sensorId = "ARU",
  sort = "fullNm",
  order = "asc"
)



rrrr <- httr::POST(
  httr::modify_url("https://www-api.wildtrax.ca", path = "/bis/download-report"),
  query = list(
    projectIds = 1104,
    sensorId = "ARU",
    splitLocation = FALSE
  ),
  accept = "application/zip",
  httr::add_headers(Authorization = paste("Bearer", mytok$access_token)),
  httr::user_agent(u),
  httr::progress()
)




x <- data.frame(do.call(rbind, httr::content(mydat)$results))
