library(shiny)
library(rsconnect)
library(yaml)

creds <- yaml::read_yaml("secrets.yaml")

rsconnect::setAccountInfo(
  name   = creds$shinyapps$name,
  token  = creds$shinyapps$token,
  secret = creds$shinyapps$secret
)

rsconnect::deployApp(
  appDir    = getwd(),
  appName   = "GAMMA_dev",
  account   = "atlantabg",
  server    = "shinyapps.io",
  forceUpdate = TRUE
)
