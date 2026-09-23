# Deploy the app to shinyapps.io. Credentials live in the git-ignored
# secrets.yaml (shinyapps: name / token / secret). Deployment records for this
# machine are written to rsconnect/shinyapps.io/atlantabg/ (also git-ignored).
library(rsconnect)

# Which shinyapps.io app to update.
app_name <- "GAMMA_dev2"

creds <- yaml::read_yaml("secrets.yaml")
rsconnect::setAccountInfo(
  name   = creds$shinyapps$name,
  token  = creds$shinyapps$token,
  secret = creds$shinyapps$secret
)

rsconnect::deployApp(
  appDir      = getwd(),
  appName     = app_name,
  account     = "atlantabg",
  server      = "shinyapps.io",
  forceUpdate = TRUE
)
