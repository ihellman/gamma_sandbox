library(shiny)
rsconnect::deployApp(
  appDir = getwd(),
  appName = "GAMMA_dev",
  account = "atlantabg",
  server = "shinyapps.io",
  forceUpdate = TRUE
)
