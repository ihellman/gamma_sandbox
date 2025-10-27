# Load the Shiny and bslib libraries
library(shiny)
library(bslib)

# ---- Landing Page Module ----
landingUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "hero-section",
        tags$img(class = "hero-image", src="https://plus.unsplash.com/premium_photo-1690031000842-1ac0508f18b7?ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&q=80&w=1470"),
        div(class = "hero-overlay"),
        div(class = "hero-content",
            h1("GAMMA"),
            p("Observing the meta collection.", style="font-size: 1.2rem;")
        )
    )
  )
}

landingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Landing page logic here
  })
}


# ---- Data Analysis Module ----
dataAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    page_sidebar(
      sidebar = sidebar(
            h5("Data Evaluation Page"),
            controlsModuleUI(ns("controls"))
          )
        ,
    mapModuleUI(ns("map")),
    hr(),
    tableModuleUI(ns("table"))
  )
  )
}

dataAnalysisServer <- function(id, combined_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    
    # Initialize nested modules
    controlsModuleServer("controls", combined_data, selected_points)
    mapModuleServer("map", combined_data, selected_points)
    tableModuleServer("table", combined_data, selected_points)
  })
}


# ---- Gap Analysis Module ----
gapAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Gap Analysis"),
    p("Gap analysis content goes here.")
  )
}

gapAnalysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Gap analysis logic here
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
    # About page logic here
  })
}


# ---- Main App UI ----
ui <- fluidPage(
  title = "GAMMA Shiny App",
  theme = bs_theme(version = 4, bootswatch = "litera"),
  
  tags$style(HTML("
    .hero-section {
      position: relative;
      height: calc(100vh - 125px); 
      min-height: 500px;
      display: flex;
      align-items: center;
      justify-content: center;
      color: white;
      text-align: center;
      overflow: hidden;
      clip-path: ellipse(80% 40% at 50% 50%);
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
    .top-nav-banner {
      background-color: #f8f9fa;
      padding: 10px 20px;
      border-bottom: 1px solid #dee2e6;
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
    }
    .footer-banner img {
      height: 50px;
      margin: 0 20px;
      vertical-align: middle;
    }
  ")),
  
  # Top navigation banner (always visible)
  div(class="top-nav-banner",
      div(class="brand", "GAMMA"),
      actionButton("nav_home", "Home", class="btn-light"),
      actionButton("nav_da", "Data Analysis", class="btn-light"),
      actionButton("nav_ga", "Gap Analysis", class="btn-light"),
      actionButton("nav_about", "About", class="btn-light")
  ),
  
  # Main page content (rendered dynamically)
  uiOutput("page_content"),
  
  # Footer Banner (always visible)
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
  
  # Page navigation state (tracks which page to show)
  currentPage <- reactiveVal("landing")
  
  # Track which modules have been initialized (for lazy loading)
  initialized <- reactiveValues(
    landing = FALSE,
    data_analysis = FALSE,
    gap_analysis = FALSE,
    about = FALSE
  )
  
  # Navigation observers (listen for button clicks and update the page state)
  observeEvent(input$nav_home, { currentPage("landing") })
  observeEvent(input$nav_da, { currentPage("data_analysis") })
  observeEvent(input$nav_ga, { currentPage("gap_analysis") })
  observeEvent(input$nav_about, { currentPage("about") })
  
  # Render page content based on current page (the routing logic)
  output$page_content <- renderUI({
    switch(currentPage(),
      "landing" = landingUI("landing_page"),
      "data_analysis" = dataAnalysisUI("data_analysis_page"),
      "gap_analysis" = gapAnalysisUI("gap_analysis_page"),
      "about" = aboutUI("about_page")
    )
  })
  
  # Lazy initialization: only initialize module servers when first visited
  # This makes the home page load faster by not initializing heavy modules upfront
  
  observeEvent(currentPage(), {
    page <- currentPage()
    
    if (page == "landing" && !initialized$landing) {
      landingServer("landing_page")
      initialized$landing <- TRUE
    } else if (page == "data_analysis" && !initialized$data_analysis) {
      dataAnalysisServer("data_analysis_page", combined_data, selected_points)
      initialized$data_analysis <- TRUE
    } else if (page == "gap_analysis" && !initialized$gap_analysis) {
      gapAnalysisServer("gap_analysis_page")
      initialized$gap_analysis <- TRUE
    } else if (page == "about" && !initialized$about) {
      aboutServer("about_page")
      initialized$about <- TRUE
    }
  }, ignoreNULL = FALSE)  # ignoreNULL = FALSE ensures it runs on initial load
}

# Run the application
shinyApp(ui = ui, server = server)