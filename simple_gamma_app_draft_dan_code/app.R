# Load the Shiny and bslib libraries
library(shiny)
library(bslib)
library(readr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(shiny)
library(reactable)

# ---- Landing Page Module ----
landingUI <- function(id) {
  ns <- NS(id)
  # No tagList or tags$style here anymore. Just return the UI.
  div(
    div(class = "hero-section",
        tags$img(class = "hero-image", src="https://plus.unsplash.com/premium_photo-1690031000842-1ac0508f18b7?ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&q=80&w=1470"),
        div(class = "hero-overlay"),
        div(class = "hero-content",
            h1("GAMMA"),
            p("Observing the meta collection.", style="font-size: 1.2rem;")
        )
    ),
    div(class="container content-section",
        # Feature Boxes
        div(class="features-container",
            div(class="feature-box",
                tags$img(src="https://images.unsplash.com/photo-1516738901171-8eb4fc13bd20?ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&q=80&w=1470"),
                h4("Gather your Data"),
                p("Upload your datasets, compare against public data, and prepare them for analysis.")
            ),
            div(class="feature-arrow", HTML("&#8594;")),
            div(class="feature-box",
                tags$img(src="https://images.unsplash.com/photo-1730804518415-75297e8d2a41?ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&q=80&w=1462"),
                h4("Find the gaps"),
                p("Geographic gap analysis to locations to priortize collections.")
            ),
            div(class="feature-arrow", HTML("&#8594;")),
            div(class="feature-box",
                tags$img(src="https://plus.unsplash.com/premium_photo-1726754516964-7ee4209343a6?ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&q=80&w=1470"),
                h4("Share the results"),
                p("Export and share your findings")
            )
        ),
        
        # Navigation Buttons
        div(class="nav-buttons",
            actionButton(ns("go_da"), "Get Started", class="btn-primary btn-lg"),
            actionButton(ns("go_about"), "Learn More", class="btn-info btn-lg")
        )
    )
  )
}



landingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Return a list of reactive expressions that fire on button clicks
    return(
      list(
        go_da = reactive(input$go_da),
        go_ga = reactive(input$go_ga),
        go_about = reactive(input$go_about)
      )
    )
  })
}


# ---- Data Analysis Module ----
dataAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    mapModuleUI(ns("map")),
    tableModuleUI(ns("table"))
  )
}

dataAnalysisServer <- function(id, combined_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    mapModuleServer("map", combined_data, selected_points)
    tableModuleServer("table", combined_data, selected_points)
  })
}


# ---- Gap Analysis Module ----
gapAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Gap Analysis Page")
  )
}

gapAnalysisServer <- function(id, dataset, obs) {
  moduleServer(id, function(input, output, session) {

  })
}


# ---- About Page Module ----
aboutUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("About This Application"),
    p("This Shiny application was created to demonstrate a multi-page layout using a combination of full-width and sidebar layouts with modules. It serves as a template for building more complex data science applications in R."),
    br(),
    h4("Technologies Used:"),
    tags$ul(
      tags$li("R"),
      tags$li("Shiny (with Modules)"),
      tags$li("bslib for theming")
    ),
    br(),
    p("Feel free to replace this content with information about your specific project, data sources, and contact information.")
  )
}

aboutServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server logic needed
  })
}


# ---- Main App UI ----
ui <- fluidPage(
  title = "GAMMA Shiny App",
  theme = bs_theme(version = 4, bootswatch = "litera"),
  
  # --- GLOBAL STYLES ---
  # Styles moved here from landingUI to load immediately
  tags$head(
    tags$style(HTML("
      /* Hide non-landing content on initial load to prevent flicker.
       * This rule is loaded in the <head> so it applies before the
       * page body is rendered.
      */
      .conditional-content {
        display: none;
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
      .content-section {
        padding: 60px 20px;
        text-align: center;
      }
      .features-container {
        display: flex;
        justify-content: center;
        gap: 20px;
        margin-top: 40px;
        margin-bottom: 50px;
        flex-wrap: wrap;
        align-items: center;
      }
      .feature-box {
        flex-basis: 280px;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        transition: transform 0.2s ease-in-out, box-shadow 0.2s ease-in-out;
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
      .feature-arrow {
        font-size: 2.5rem;
        color: #cccccc;
      }
      .nav-buttons .btn {
        margin: 0 10px;
      }
      .top-nav-banner {
        background-color: #f8f9fa;
        padding: 10px 20px;
        border-bottom: 1px solid #dee2e6;
        margin-bottom: 20px;
        display: flex;
        gap: 10px;
        align-items: center;
      }
      .top-nav-banner .brand {
        font-size: 1.5rem;
        font-weight: bold;
        margin-right: auto;
      }
      .footer-banner {
        background-color: #f8f9fa;
        padding: 20px;
        border-top: 1px solid #dee2e6;
        text-align: center;
        margin-top: 50px;
      }
      .footer-banner img {
        height: 50px; /* Logo height */
        margin: 0 20px;
        vertical-align: middle;
      }
    "))
  ),
  
  # A container for all UI
  div(
    # Conditional panel for the landing page (no sidebar)
    conditionalPanel(
      condition = "output.page === 'landing'",
      landingUI("landing_page")
    ),
    
    # Conditional panel for all other pages (with top nav banner and sidebar)
    # Add class to enable CSS-based hiding on initial load
    conditionalPanel(
      # Need to not return null (e.g. "output.page !== 'landing'").  Otherwise, page may flicker navbar on initial load. 
      condition = "output.page === 'data_analysis' || 
                   output.page === 'gap_analysis' || 
                   output.page === 'about'",
      class = "conditional-content",
      style = "display: none;",
      
      # Top navigation banner
      div(class="top-nav-banner",
          div(class="brand", "GAMMA"),
          actionButton("nav_da", "Data Analysis", class="btn-light"),
          actionButton("nav_ga", "Gap Analysis", class="btn-light"),
          actionButton("nav_about", "About", class="btn-light"),
          actionButton("go_home", "Home", class="btn-secondary")
      ),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          # Conditional controls now depend on output.page
          conditionalPanel(
            condition = "output.page === 'data_analysis'",
            h4("Data Analysis Controls"),
            controlsModuleUI("controls"),
          ),
          conditionalPanel(
            condition = "output.page === 'gap_analysis'",
            h4("Gap Analysis Controls")
          ),
          conditionalPanel(
            condition = "output.page === 'about'",
            p("Navigate using the banner above.")
          )
        ),
        mainPanel(
          # Main content panels are also conditional on the current page
          conditionalPanel(
            condition = "output.page === 'data_analysis'",
            dataAnalysisUI("data_analysis_page")
          ),
          conditionalPanel(
            condition = "output.page === 'gap_analysis'",
            gapAnalysisUI("gap_analysis_page")
          ),
          conditionalPanel(
            condition = "output.page === 'about'",
            aboutUI("about_page")
          )
        )
      )
    )
  ),
  
  # Footer Banner
  div(class="footer-banner",
      tags$a(href="https://www.bgci.org/", target="_blank",
             tags$img(src="https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/Botanic_Gardens_Conservation_International_logo.svg/320px-Botanic_Gardens_Conservation_International_logo.svg.png", 
                      alt="Botanic Gardens Conservation International Logo")
      ),
      tags$a(href="https://www.usbg.gov/", target="_blank",
             tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/5a/US_Botanic_Garden_logo.svg/320px-US_Botanic_Garden_logo.svg.png", 
                      alt="US Botanic Garden Logo")
      )
  )
)

# ---- Main App Server ----
server <- function(input, output, session) {
  # Shared reactive values accessible by all pages
  combined_data <- reactiveVal(data.frame())
  selected_points <- reactiveVal(numeric(0))

  # --- Page Navigation State ---
  currentPage <- reactiveVal("landing")
  
  # Expose page state to the UI for conditionalPanels
  output$page <- reactive(currentPage())
  outputOptions(output, "page", suspendWhenHidden = FALSE)
  
  # --- Navigation Event Handling ---
  # 1. From Landing Page Buttons
  landing_nav <- landingServer("landing_page")
  
  observeEvent(landing_nav$go_da(), {
    currentPage("data_analysis")
  })
  observeEvent(landing_nav$go_ga(), {
    currentPage("gap_analysis")
  })
  observeEvent(landing_nav$go_about(), {
    currentPage("about")
  })
  
  # 2. From Top Nav Banner Buttons
  observeEvent(input$go_home, {
    currentPage("landing")
  })
  observeEvent(input$nav_da, {
    currentPage("data_analysis")
  })
  observeEvent(input$nav_ga, {
    currentPage("gap_analysis")
  })
  observeEvent(input$nav_about, {
    currentPage("about")
  })
  

  
  # --- Initialize Module Servers ---
  # Pass the single shared dataset reactive to both modules
  dataAnalysisServer("data_analysis_page", combined_data, selected_points)
  controlsModuleServer("controls", combined_data, selected_points)
  gapAnalysisServer("gap_analysis_page")
  aboutServer("about_page")
}

# Run the application
shinyApp(ui = ui, server = server)