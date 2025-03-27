library(googlesheets4)
library(googledrive)

#enter your gmail address here
email_address = "appdemo41@gmail.com"

options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = ".secrets"
)

#only run once:
# gs4_auth(email = email_address, cache = ".secrets")
# drive_auth(email = email_address, cache = ".secrets")

googlesheets4::gs4_auth()
googledrive::drive_auth()

