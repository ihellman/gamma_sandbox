# app.R
library(shiny)
library(bslib)
library(shinyjs)
library(readr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(DT)
library(yaml)
library(markdown)

# necesary only if app is called from the "play" button in positron
source("R/leaflet_maps.R")

# Load text from YAML file
# Uses a relative path from the app.R file location
landing_text <- read_yaml("appData/landing_text.yml")
# altering a bit before attaching to merge back in
# Custom CSS for navbar styling and landing page
custom_css <- "
/* Hide navbar initially */
.navbar {
  display: none;
}

/* Show navbar when app is launched */
body.app-launched .navbar {
  display: block !important;
}

/* Hide landing page when app is launched */
body.app-launched .landing-page {
  display: none !important;
}

/* Show landing page when on home */
body.on-home .landing-page {
  display: flex !important;
}

/* Hide navbar when on home */
body.on-home .navbar {
  display: none !important;
}

/* Landing page styles */
.landing-page {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 100vh;
  display: flex;
  flex-direction: column;
  z-index: 1000;
  overflow-y: auto;
}

.hero-section {
  position: relative;
  height: 60vh;
  min-height: 400px;
  display: flex;
  align-items: center;
  justify-content: center;
  color: white;
  text-align: center;
  overflow: hidden;
  clip-path: ellipse(120% 100% at 50% 0%);
}

.hero-image {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  object-fit: cover;
  z-index: -2;
}

.hero-overlay {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background-color: rgba(0, 0, 0, 0.5);
  z-index: -1;
}

.hero-content {
  max-width: 800px;
  padding: 20px;
}

.hero-content h1 {
  font-size: 4rem;
  margin-bottom: 1rem;
  font-weight: 700;
}

.content-section {
  padding: 60px 20px;
  text-align: center;
  background: white;
}

.features-container {
  display: flex;
  justify-content: center;
  align-items: center;
  gap: 20px;
  margin-top: 40px;
  margin-bottom: 50px;
  flex-wrap: wrap;
  align-items: stretch;
}

.feature-box {
  width: 300px;
  height: 300px;
  flex-basis: 280px;
  padding: 20px;
  border-radius: 8px;
  box-shadow: 0 4px 12px rgba(0,0,0,0.1);
  transition: transform 0.2s ease-in-out, box-shadow 0.2s ease-in-out;
  display: flex;
  flex-direction: column;
  min-height: 340px;
}

.feature-box:hover {
  transform: translateY(-5px);
  box-shadow: 0 8px 20px rgba(0,0,0,0.15);
}

.feature-box img {
  width: 100%;
  height: 160px;
  object-fit: cover;
  border-radius: 4px;
  margin-bottom: 15px;
}

.feature-box h4 {
  margin: 15px 0 10px 0;
}

.nav-buttons {
  margin-top: 30px;
}

.nav-buttons .btn {
  margin: 0 10px;
}

.footer-banner {
  background-color: #f8f9fa;
  padding: 20px;
  border-top: 1px solid #dee2e6;
  text-align: center;
  margin-top: auto;
}

.footer-banner img {
  height: 50px;
  margin: 0 20px;
  vertical-align: middle;
}

/* Custom navbar styling */
.navbar {
  background-color: #f8f9fa !important;
  box-shadow: none;
  padding: 10px 20px;
  border-bottom: 1px solid #dee2e6;
}

.navbar-brand {
  font-size: 1.5rem;
  font-weight: bold;
  color: #212529 !important;
}

.navbar-nav .nav-link {
  color: #495057 !important;
  font-weight: 500;
  padding: 0.5rem 1rem !important;
  margin: 0 0.25rem;
  border-radius: 0.25rem;
  transition: all 0.2s ease;
  background-color: #f8f9fa;
  border: 1px solid transparent;
}

.navbar-nav .nav-link:hover {
  background-color: #e9ecef;
  color: #212529 !important;
}

.navbar-nav .nav-link.active {
  background-color: #e9ecef !important;
  color: #212529 !important;
  border: 1px solid #dee2e6;
}
.action-bar-container {
  padding: 40px 0; /* Vertical padding, no horizontal padding */
  background-color: #f8f9fa; /* A light grey, similar to your navbar */
  text-align: center;
  border-bottom: 1px solid #dee2e6; 
  /* Removed radius, shadow, and margin, as it's a full-width banner now */
}

.action-bar-container h3 {
  font-size: 2rem;
  font-weight: 600;
  margin-bottom: 10px;
}

.action-bar-container p {
  font-size: 1.1rem;
  color: #6c757d; /* A muted text color */
  margin-bottom: 25px;
}
.action-bar-buttons {
  display: flex;        /* 1. Turns on Flexbox (like your example) */
  flex-wrap: nowrap;  /* Ensures buttons stay horizontal */
  gap: 1rem;          /* 2. Sets space *between* buttons (like your example) */
}
.action-bar-buttons .btn {  flex: 1; /* 3. Makes all buttons grow to be the same width (like your example) */
}

.features-container > a {
  text-decoration: none !important;
  color: inherit !important;
}

/* Add pointer cursor to feature box on hover */
.feature-box:hover {
  transform: translateY(-5px);
  box-shadow: 0 8px 20px rgba(0,0,0,0.15);
  cursor: pointer; /* <-- This is new */
}
  /* Inside your custom_css string in app.R */
.modal-body img {
  max-width: 80%;
  height: auto;
  display: block; /* to allow margin: auto */
  margin: 0 auto; /* to center the image */
  padding: 10px 0; /* Add some vertical spacing */
  box-sizing: border-box; /* Include padding/border in element's total width/height */
}
"

# MAIN UI ----------------------------------------------------------------------------------
ui <- page_navbar(
  title = "Data Explorer",
  id = "navbar",
  theme = bs_theme(version = 5),
  header = tags$head(
    tags$style(HTML(custom_css))
  ),
  nav_spacer(),

  # Initialize shinyjs
  useShinyjs(),

  # Landing page overlay
  landingUI("landing", landing_text = landing_text),

  # Navigation pages
  nav_panel("Home", value = "home", div()),
  nav_panel("Data Analysis", value = "data", dataAnalysisUI("data_analysis")),
  nav_panel("GAP Analysis", value = "gap", gapAnalysisUI("gap_analysis")),
  nav_panel("About", value = "about", aboutUI("about"))
)

# MAIN SERVER -------------------------------------------------------------------------------
server <- function(input, output, session) {
  combined_data <- reactiveVal(data.frame())
  selected_points <- reactiveVal(numeric(0))

  # Landing page module
  launch_actions <- landingServer("landing", landing_text = landing_text)

  # Observe launch button (Get Started)
  observeEvent(launch_actions$launch(), {
    req(launch_actions$launch() > 0)
    shinyjs::addClass(selector = "body", class = "app-launched")
    shinyjs::removeClass(selector = "body", class = "on-home")
    updateNavbarPage(session, "navbar", selected = "data")
  })

  # Observe learn more button
  observeEvent(launch_actions$learn_more(), {
    req(launch_actions$learn_more() > 0)
    shinyjs::addClass(selector = "body", class = "app-launched")
    shinyjs::removeClass(selector = "body", class = "on-home")
    updateNavbarPage(session, "navbar", selected = "about")
  })

  # Observe navbar changes - if user clicks Home, show landing page
  observe({
    if (!is.null(input$navbar) && input$navbar == "home") {
      shinyjs::addClass(selector = "body", class = "on-home")
    } else if (!is.null(input$navbar)) {
      shinyjs::removeClass(selector = "body", class = "on-home")
    }
  })

  # Initialize other modules
  dataAnalysisServer("data_analysis", combined_data, selected_points)
  controlsModuleServer("controls", combined_data, selected_points)
  gapAnalysisServer("gap_analysis")
  aboutServer("about")
}

# Run the application
shinyApp(ui, server)
