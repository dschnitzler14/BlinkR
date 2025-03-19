options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = "BlinkR_app/.secrets"
)

#only run once:
#gs4_auth(email = "appdemo41@gmail.com", cache = "BlinkR_app/.secrets")
#drive_auth(email = "appdemo41@gmail.com", cache = "BlinkR_app/.secrets")

googlesheets4::gs4_auth()
googledrive::drive_auth()

