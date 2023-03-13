

fr <- .GlobalEnv$wt_audio_scanner(path = "/volumes/budata/cws-nor/aru/fr", file_type = "all", extra_cols = F)

#
token <- httr::content(httr::POST(
url = "https://abmi.auth0.com/oauth/token",
encode = "form",
body = list(
  audience = "http://www.wildtrax.ca",
  grant_type = "password",
  client_id = cid,
  username = Sys.getenv("WT_USERNAME"),
  password = Sys.getenv("WT_PASSWORD")
)
))$access_token

token
tok_web <- "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6Ik1rSkdOa1JDTWtNd09FTTRNa1pGTVRWRE1UaEJOVVkzUmpNd1FqWkROREZGT0RGRU16WTJOQSJ9.eyJhbGwtcm9sZXMiOltdLCJpc3MiOiJodHRwczovL2FibWkuYXV0aDAuY29tLyIsInN1YiI6ImF1dGgwfDYxM2MyNDNiYjRmNjY0MDA3MWM3NzFjMiIsImF1ZCI6WyJodHRwOi8vd3d3LndpbGR0cmF4LmNhIiwiaHR0cHM6Ly9hYm1pLmF1dGgwLmNvbS91c2VyaW5mbyJdLCJpYXQiOjE2NzM1NDQ5MzQsImV4cCI6MTY3MzU3MzczNCwiYXpwIjoiRWcyTVBWdHFrZjNTdUtTNXVYelA5N254VTEzWjJLMWkiLCJzY29wZSI6Im9wZW5pZCBwcm9maWxlIGVtYWlsIn0.gm7XJZePCw5DElBd5kcKkIMFbxV72pbep4aODc_8hYWbMQim8_fQ205X0FBGdK4Bxa4Gqht9ELqdIy3jJfLPVCvmuKOpx_uJjJGNDoCDetfoIySbFCrVWH9uSJmsN9Npq4lt_fmxwbTAlMSGqNf9WaXugI36WUl82tnU15VL51pd7rWwRcltAxOjKdl50BJ-6gMoPusdpNizVPLqsi_f5RbXyLmrq6D9XYRyxG5srvEM9BR9SR0b8DZVGk7KpbdoG9hVipCvSUS_ZQidYTvQYW-SOEQTnLq-3ouH7kkcHHbQIRvY0YGZqgsJptksMjiECji548mglEA4mJHyB9xzZA"

token == tok_web

# Replace with your client ID and redirect URI
gcid <- "413330073155-s14t1r2cqqginjhdsih59va5tmul2068.apps.googleusercontent.com"
gruri <- "https://www.wildtrax.ca"

# Build the authorization URL
url <- sprintf("https://accounts.google.com/o/oauth2/auth?client_id=%s&redirect_uri=%s&response_type=code",
               gcid, gruri)

k <- httr::GET(url)


################## HALFWAY
google_bearer_token <- "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6Ik1rSkdOa1JDTWtNd09FTTRNa1pGTVRWRE1UaEJOVVkzUmpNd1FqWkROREZGT0RGRU16WTJOQSJ9.eyJhbGwtcm9sZXMiOltdLCJpc3MiOiJodHRwczovL2FibWkuYXV0aDAuY29tLyIsInN1YiI6Imdvb2dsZS1vYXV0aDJ8MTAzMTk3MTExNDk4MTk0Nzg4NTE5IiwiYXVkIjpbImh0dHA6Ly93d3cud2lsZHRyYXguY2EiLCJodHRwczovL2FibWkuYXV0aDAuY29tL3VzZXJpbmZvIl0sImlhdCI6MTY3MzQ3NDc1NiwiZXhwIjoxNjczNTAzNTU2LCJhenAiOiJFZzJNUFZ0cWtmM1N1S1M1dVh6UDk3bnhVMTNaMksxaSIsInNjb3BlIjoib3BlbmlkIHByb2ZpbGUgZW1haWwifQ.A6ggQIZosQ526m3A-hUYKf5iQZH4Lg5mATms528yzOQPi8pkAenFrzlBIMmTXqhaz2FkVtq7o9Bw58oSXPXl6syQxm2SKo2iWv1Udh1PCfJdCECUgv8yk_dEzefvafGlPldrx2387XsNtua8caNmcn8dixfEqZmE-CijDGV5y9j0GkFwmh2HKHOeM1j5W5wShQznG8urTQKLghj58pM-GkfdOM53EbBT_wXWmpjkub7mNdgZ2_rF-FUKIBK10DBKAS04JNsfY805AT7t88Tc12eHYenEblH26XpJFQReWDy35k7lqq2nCd7KKOGsBZlHtfWhJZWCnLNCxtbR4pVjXQ"

# Create POST request
g <- httr::POST(
  httr::modify_url("https://www-api.wildtrax.ca", path = "/bis/download-report"),
  query = list(
    projectIds = 47,
    sensorId = 'ARU',
    splitLocation = FALSE
  ),
  accept = "application/zip",
  httr::add_headers(Authorization = paste("Bearer", google_bearer_token)),
  httr::progress()
)













