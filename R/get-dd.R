wt_dd_summary <- function(ne = "", sw = "", polygon = "", sensor = "ARU", species = NULL){

  cid <- rawToChar(
    as.raw(c(0x45, 0x67, 0x32, 0x4d, 0x50, 0x56, 0x74, 0x71, 0x6b,
             0x66, 0x33, 0x53, 0x75, 0x4b, 0x53, 0x35, 0x75, 0x58, 0x7a, 0x50,
             0x39, 0x37, 0x6e, 0x78, 0x55, 0x31, 0x33, 0x5a, 0x32, 0x4b, 0x31,
             0x69)))

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

  x <- httr::content(r)

  my_tok <- x$access_token

  query_list <- list(
    sensorId = "ARU"
  )

  u <- getOption("HTTPUserAgent")
  if (is.null(u)) {
    u <- sprintf("R/%s; R (%s)",
                 getRversion(),
                 paste(getRversion(), R.version$platform, R.version$arch, R.version$os))
  }

  # Add wildRtrax version information:
  u <- paste0("wildRtrax ", as.character(packageVersion("wildRtrax")), "; ", u)

  payload <- list(
    isSpeciesTab = FALSE,
    zoomLevel = 1,
    bounds = list(
      `_sw` = list(
        lng = sw$lng,
        lat = sw$lat
      ),
      `_ne` = list(
        lng = ne$lng,
        lat = ne$lat
      )
    ),
    sensorId = "ARU",
    polygonBoundary = NULL,
    organizationIds = NULL,
    projectIds = NULL,
    speciesIds = list(species)  # Wrap the integer value in a list to make it an array
  )

  rr <- httr::POST(
    httr::modify_url("https://www-api.wildtrax.ca", path = "/bis/get-data-discoverer-long-lat-summary"),
    query = query_list,
    accept = "application/json",
    httr::add_headers(
      Authorization = paste("Bearer", my_tok),
      Origin = "https://discover.wildtrax.ca",
      Pragma = "no-cache",
      Referer = "https://discover.wildtrax.ca/"
    ),
    httr::user_agent(u),
    httr::progress(),
    body = payload,
    encode = "json" # Specify that the payload should be encoded as JSON
  )

  rpps <- httr::content(rr)

  # Extracting data
  orgs <- map_chr(rpps$organizations, pluck, "organizationName")
  counts <- map_dbl(rpps$projects, pluck, "count")
  projectNames <- map(rpps$projects, ~pluck(., "projectName")) %>% map_chr(~ ifelse(is.null(.x), "", .x))
  projectIds <- map(rpps$projects, ~pluck(., "projectId")) %>% map_int(~ ifelse(is.null(.x), NA_integer_, .x))

  spp <- httr::POST(
    httr::modify_url("https://www-api.wildtrax.ca", path = "/bis/get-all-species"),
    accept = "application/json",
    httr::add_headers(Authorization = paste("Bearer", my_tok)),
    httr::user_agent(u)
  )

  spps <- httr::content(spp)

  spp_table <- tibble(
    species_id = map_dbl(spps, ~ ifelse(!is.null(.x$id), .x$id, NA)),
    species_code = map_chr(spps, ~ ifelse(!is.null(.x$code), .x$code, NA)),
    species_common_name = map_chr(spps, ~ ifelse(!is.null(.x$commonName), .x$commonName, NA)),
    species_class = map_chr(spps, ~ ifelse(!is.null(.x$className), .x$className, NA)),
    species_order = map_chr(spps, ~ ifelse(!is.null(.x$order), .x$order, NA)),
    species_scientific_name = map_chr(spps, ~ ifelse(!is.null(.x$scientificName), .x$scientificName, NA))
  )

  # Create tibble
  rpps_tibble <- tibble(
    projectId = projectIds,
    project_name = projectNames,
    species_id = 2441,
    count = counts
  ) |>
    inner_join(spp_table, by = "species_id") |>
    select(projectId, project_name, count, species_common_name, species_code, species_scientific_name) |>
    distinct()

  return(rpps_tibble)

}

wt_dd_summary(ne = list(lng = -76.496, lat = 43.106),
              sw = list(lng = -76.485, lat = 43.115),
              sensor = "ARU",
              species = 2441)
