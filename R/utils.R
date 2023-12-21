#' Internal functions
#'
#' WildTrax authentication
#'
#' @description Get Auth0 token and assign information to the hidden environment
#'
#' @keywords internal
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

  # POST request
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
#' @keywords internal
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
#' @keywords internal
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

#' Internal functions
#'
#' QPAD offsets, wrapped by the `wt_qpad_offsets` function.
#'
#' @description Functions to format reports for qpad offset calculation.
#'
#' @param data Dataframe output from the `wt_make_wide` function.
#' @param tz Character; whether or not the data is in local or UTC time ("local", or "utc"). Defaults to "local".
#' @param check_xy Logical; check whether coordinates are within the range that QPAD offsets are valid for.
#'
#' @keywords internal
#'
#' @import QPAD dplyr intrval terra
#' @importFrom suntools sunriset

.make_x <- function(data, tz="local", check_xy=TRUE) {

  # Download message
  message("Download geospatial assets. This may take a moment.")

  # Get tifs from assets repo
  .rlcc <- terra::raster(download.file("https://raw.githubusercontent.com/ABbiodiversity/wildRtrax-assets/main/lcc.tif", destfile = "lcc.tif"))
  .rtree <- terra::raster(download.file("https://raw.githubusercontent.com/ABbiodiversity/wildRtrax-assets/main/tree.tif", destfile = "tree.tif"))
  .rd1 <- terra::raster(download.file("https://raw.githubusercontent.com/ABbiodiversity/wildRtrax-assets/main/seedgrow.tif", destfile = "seedgrow.tif"))
  .rtz <- terra::raster(download.file("https://raw.githubusercontent.com/ABbiodiversity/wildRtrax-assets/main/utcoffset.tif", destfile = "utcoffset.tif"))

  message("Reading tifs and removing assets")

  # Remove once downloaded and read
  file.remove(list.files(pattern = "*.tif"))

  crs <- terra::crs(.rtree)

  #get vars
  date <- str_sub(data$recording_date_time, 1, 10)
  time <- str_sub(data$recording_date_time, 12, 19)
  lon <- as.numeric(data$longitude)
  lat <- as.numeric(data$latitude)
  dur <- as.numeric(str_sub(data$task_duration, -100, -2))
  dis <- Inf

  #parse date+time into POSIXlt
  if(tz=="local"){
    dtm <- strptime(paste0(date, " ", time, ":00"),
                    format="%Y-%m-%d %H:%M:%S", tz="America/Edmonton")
  }
  if(tz=="utc"){
    dtm <- strptime(paste0(date, " ", time, ":00"),
                    format="%Y-%m-%d %H:%M:%S", tz="GMT")
  }
  day <- as.integer(dtm$yday)
  hour <- as.numeric(round(dtm$hour + dtm$min/60, 2))

  #checks
  checkfun <- function(x, name="", range=c(-Inf, Inf)) {
    if (any(x[!is.na(x)] %)(% range))
      stop(sprintf("Parameter %s is out of range [%.0f, %.0f]", name, range[1], range[2]))
    invisible(NULL)
  }
  #Coordinates
  if (check_xy) {
    checkfun(lon, "lon", c(-164, -52))
    checkfun(lat, "lat", c(39, 69))
  }
  if (any(is.infinite(lon)))
    stop("Parameter lon must be finite")
  if (any(is.infinite(lat)))
    stop("Parameter lat must be finite")

  #handling missing values
  ok_xy <- !is.na(lon) & !is.na(lat)
  #Other fields
  checkfun(day, "day", c(0, 365))
  checkfun(hour, "hour", c(0, 24))
  checkfun(dur, "dur", c(0, Inf))

  #intersect here
  xydf <- data.frame(x=lon, y=lat)
  xydf$x[is.na(xydf$x)] <- mean(xydf$x, na.rm=TRUE)
  xydf$y[is.na(xydf$y)] <- mean(xydf$y, na.rm=TRUE)
  xy <- terra::vect(xydf, geom=c("x", "y"), crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  xy <- terra::project(xy, crs)

  #LCC4 and LCC2
  vlcc <- terra::extract(.rlcc, xy)$lcc
  lcclevs <- c("0"="", "1"="Conif", "2"="Conif", "3"="", "4"="",
               "5"="DecidMixed", "6"="DecidMixed", "7"="", "8"="Open", "9"="",
               "10"="Open", "11"="Open", "12"="Open", "13"="Open", "14"="Wet",
               "15"="Open", "16"="Open", "17"="Open", "18"="", "19"="")
  lcc4 <- factor(lcclevs[vlcc+1], c("DecidMixed", "Conif", "Open", "Wet"))
  lcc2 <- lcc4
  levels(lcc2) <- c("Forest", "Forest", "OpenWet", "OpenWet")

  #TREE
  vtree <- terra::extract(.rtree, xy)$tree
  TREE <- vtree / 100
  TREE[TREE %)(% c(0, 1)] <- 0

  #raster::extract seedgrow value (this is rounded)
  d1 <- terra::extract(.rd1, xy)$seedgrow

  #UTC offset + 7 makes Alberta 0 (MDT offset) for local times
  if(tz=="local"){
    ltz <- terra::extract(.rtz, xy)$utcoffset + 7
  }
  if(tz=="utc"){
    ltz <- 0
  }

  #sunrise time adjusted by offset
  ok_dt <- !is.na(dtm)
  dtm[is.na(dtm)] <- mean(dtm, na.rm=TRUE)
  if(tz=="local"){
    sr <- suntools::sunriset(cbind("X"=xydf$x, "Y"=xydf$y),
                   as.POSIXct(dtm, tz="America/Edmonton"),
                   direction="sunrise", POSIXct.out=FALSE) * 24
  }
  if(tz=="utc"){
    sr <- suntools::sunriset(cbind("X"=xydf$x, "Y"=xydf$y),
                   as.POSIXct(dtm, tz="GMT"),
                   direction="sunrise", POSIXct.out=FALSE) * 24
  }
  TSSR <- round(unname((hour - sr + ltz) / 24), 4)

  #days since local spring
  DSLS <- (day - d1) / 365

  #transform the rest
  JDAY <- round(day / 365, 4) # 0-365
  TREE <- round(vtree / 100, 4)
  MAXDIS <- round(dis / 100, 4)
  MAXDUR <- round(dur, 4)

  out <- data.frame(
    TSSR=TSSR,
    JDAY=JDAY,
    DSLS=DSLS,
    LCC2=lcc2,
    LCC4=lcc4,
    TREE=TREE,
    MAXDUR=MAXDUR,
    MAXDIS=MAXDIS)
  out$TSSR[!ok_xy | !ok_dt] <- NA
  out$DSLS[!ok_xy] <- NA
  out$LCC2[!ok_xy] <- NA
  out$LCC4[!ok_xy] <- NA
  out$TREE[!ok_xy] <- NA

  return(out)

}

#' QPAD offsets, wrapped by the `wt_qpad_offsets` function.
#'
#' @description Functions to get the offsets.
#'
#' @param spp species for offset calculation.
#' @param x Dataframe out from the `.make_x` function.
#'
#' @keywords internal
#'
#' @import QPAD dplyr intrval
#'

.make_off <- function(spp, x){

  if (length(spp) > 1L)
    stop("spp argument must be length 1, please loop or map for multiple species")
  spp <- as.character(spp)

  #checks
  if (!(spp %in% getBAMspecieslist()))
    stop(sprintf("Species %s has no QPAD estimate", spp))

  #constant for NA cases
  cf0 <- exp(unlist(coefBAMspecies(spp, 0, 0)))

  #best model
  mi <- bestmodelBAMspecies(spp, type="BIC")
  cfi <- coefBAMspecies(spp, mi$sra, mi$edr)

  TSSR <- x$TSSR
  DSLS <- x$DSLS
  JDAY <- x$JDAY
  lcc2 <- x$LCC2
  lcc4 <- x$LCC4
  TREE <- x$TREE
  MAXDUR <- x$MAXDUR
  MAXDIS <- x$MAXDIS
  n <- nrow(x)

  #Design matrices for singing rates (`Xp`) and for EDR (`Xq`)
  Xp <- cbind(
    "(Intercept)"=1,
    "TSSR"=TSSR,
    "JDAY"=JDAY,
    "TSSR2"=TSSR^2,
    "JDAY2"=JDAY^2,
    "DSLS"=DSLS,
    "DSLS2"=DSLS^2)

  Xq <- cbind("(Intercept)"=1,
              "TREE"=TREE,
              "LCC2OpenWet"=ifelse(lcc4 %in% c("Open", "Wet"), 1, 0),
              "LCC4Conif"=ifelse(lcc4=="Conif", 1, 0),
              "LCC4Open"=ifelse(lcc4=="Open", 1, 0),
              "LCC4Wet"=ifelse(lcc4=="Wet", 1, 0))

  p <- rep(NA, n)
  A <- q <- p

  #design matrices matching the coefs
  Xp2 <- Xp[,names(cfi$sra),drop=FALSE]
  OKp <- rowSums(is.na(Xp2)) == 0
  Xq2 <- Xq[,names(cfi$edr),drop=FALSE]
  OKq <- rowSums(is.na(Xq2)) == 0

  #calculate p, q, and A based on constant phi and tau for the respective NAs
  p[!OKp] <- sra_fun(MAXDUR[!OKp], cf0[1])
  unlim <- ifelse(MAXDIS[!OKq] == Inf, TRUE, FALSE)
  A[!OKq] <- ifelse(unlim, pi * cf0[2]^2, pi * MAXDIS[!OKq]^2)
  q[!OKq] <- ifelse(unlim, 1, edr_fun(MAXDIS[!OKq], cf0[2]))

  #calculate time/lcc varying phi and tau for non-NA cases
  phi1 <- exp(drop(Xp2[OKp,,drop=FALSE] %*% cfi$sra))
  tau1 <- exp(drop(Xq2[OKq,,drop=FALSE] %*% cfi$edr))
  p[OKp] <- sra_fun(MAXDUR[OKp], phi1)
  unlim <- ifelse(MAXDIS[OKq] == Inf, TRUE, FALSE)
  A[OKq] <- ifelse(unlim, pi * tau1^2, pi * MAXDIS[OKq]^2)
  q[OKq] <- ifelse(unlim, 1, edr_fun(MAXDIS[OKq], tau1))

  #log(0) is not a good thing, apply constant instead
  ii <- which(p == 0)
  p[ii] <- sra_fun(MAXDUR[ii], cf0[1])

  #package output
  data.frame(
    p=p,
    q=q,
    A=A,
    correction=p*A*q,
    offset=log(p) + log(A) + log(q))

}


