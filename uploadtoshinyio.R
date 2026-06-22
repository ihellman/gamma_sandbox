library(shiny)
library(rsconnect)
library(yaml)
# load in secrets 
creds <- yaml::read_yaml("secrets.yaml")
# 
rsconnect::setAccountInfo(
  name   = creds$shinyapps$name,
  token  = creds$shinyapps$token,
  secret = creds$shinyapps$secret
)

# 
test<- FALSE

if(isTRUE(test)){

  rsconnect::deployApp(
    appDir    = getwd(),
    appName   = "GAMMA_test",
    account   = "atlantabg",
    server    = "shinyapps.io",
    forceUpdate = TRUE
  )

}else{
    rsconnect::deployApp(
    appDir    = getwd(),
    appName   = "GAMMA_dev",
    account   = "atlantabg",
    server    = "shinyapps.io",
    forceUpdate = TRUE
  )

}

