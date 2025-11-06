library(shiny)
library(bslib)
library(leaflet)

# --- 1. Analysis Module ---
# analysis_ui: Defines the UI for the Analysis page.
analysis_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    sidebar = sidebar(
      title = "Analysis Controls",
      selectInput(ns("dataset"), "Select Dataset:",
                  choices = c("Dataset 1" = "ds1", "Dataset 2" = "ds2", "Dataset 3" = "ds3")),
      selectInput(ns("type"), "Analysis Type:",
                  choices = c("Summary Statistics" = "summary", 
                              "Correlation" = "correlation",
                              "Regression" = "regression")),
      actionButton(ns("run"), "Run Analysis", class = "btn-primary")
    ),
    card(
      card_header("Analysis Results"),
      card_body(
        h4("Analysis Output"),
        verbatimTextOutput(ns("output"))
      )
    )
  )
}

# analysis_server: Handles the logic for the Analysis page.
analysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$output <- renderText({
      if (input$run == 0) return("Click 'Run Analysis' to see results")
      
      paste("Analysis completed for", input$dataset, 
            "using", input$type, "method.\n",
            "Results show significant patterns in the data.\n",
            "Processing completed at:", Sys.time())
    })
  })
}


# --- 2. GAP Module (Map) ---
# gap_ui: Defines the UI for the GAP (Geospatial Analysis Platform) page.
gap_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    sidebar = sidebar(
      title = "GAP Controls",
      h5("Map Interactions"),
      actionButton(ns("add_random_marker"), "Add Random Marker", class = "btn-success"),
      actionButton(ns("clear_markers"), "Clear All Markers", class = "btn-warning"),
      br(), br(),
      h5("Settings"),
      numericInput(ns("threshold"), "Threshold Value:", value = 0.5, min = 0, max = 1, step = 0.1),
      selectInput(ns("method"), "Method:",
                  choices = c("Method A" = "methodA", "Method B" = "methodB")),
      verbatimTextOutput(ns("map_info"))
    ),
    card(
      card_header("Interactive Map"),
      card_body(
        # Leaflet map output
        leafletOutput(ns("map"), height = "500px"),
        br(),
        h5("Map Statistics"),
        tableOutput(ns("table"))
      )
    )
  )
}

# gap_server: Handles the logic for the GAP page, including map interactions and state.
gap_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive value to hold map markers data
    map_markers <- reactiveVal(data.frame(
      lat = numeric(0),
      lng = numeric(0),
      id = character(0)
    ))
    
    # Initialize the Leaflet map (render only once!)
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = -93.65, lat = 42.0285, zoom = 4) %>%
        addProviderTiles(providers$CartoDB.Positron)
    })
    
    # Observer to add random markers
    observeEvent(input$add_random_marker, {
      # Generate random coordinates
      new_lat <- runif(1, 25, 49)
      new_lng <- runif(1, -125, -66)
      new_id <- paste0("marker_", Sys.time())
      
      # Update reactive data
      current_markers <- map_markers()
      new_marker <- data.frame(lat = new_lat, lng = new_lng, id = new_id)
      map_markers(rbind(current_markers, new_marker))
      
      # Add marker to map using leafletProxy
      # FIX: Changed "gap-map" to the correct local output ID "map"
      leafletProxy("map") %>% 
        addMarkers(
          lng = new_lng, 
          lat = new_lat, 
          layerId = new_id,
          popup = paste("Marker:", new_id)
        )
    })
    
    # Observer to clear all markers
    observeEvent(input$clear_markers, {
      map_markers(data.frame(lat = numeric(0), lng = numeric(0), id = character(0)))
      # FIX: Changed "gap-map" to the correct local output ID "map"
      leafletProxy("map") %>%
        clearMarkers()
    })
    
    # Handle map clicks to add markers
    observeEvent(input$map_click, { # Map click input ID is always just the output ID + _click
      click <- input$map_click
      new_id <- paste0("click_", Sys.time())
      
      # Update reactive data
      current_markers <- map_markers()
      new_marker <- data.frame(lat = click$lat, lng = click$lng, id = new_id)
      map_markers(rbind(current_markers, new_marker))
      
      # Add marker to map
      # FIX: Changed "gap-map" to the correct local output ID "map"
      leafletProxy("map") %>%
        addMarkers(
          lng = click$lng, 
          lat = click$lat, 
          layerId = new_id,
          popup = paste("Clicked at:", round(click$lat, 4), ",", round(click$lng, 4))
        )
    })
    
    # Map info display
    output$map_info <- renderText({
      markers <- map_markers()
      paste("Total markers:", nrow(markers))
    })
    
    # GAP table with map data
    output$table <- renderTable({
      markers <- map_markers()
      if (nrow(markers) == 0) {
        return(data.frame(
          Statistic = c("Total Markers", "Threshold", "Method"),
          Value = c("0", input$threshold, input$method)
        ))
      }
      
      data.frame(
        Statistic = c("Total Markers", "Avg Latitude", "Avg Longitude", "Threshold", "Method"),
        Value = c(
          nrow(markers),
          round(mean(markers$lat), 4),
          round(mean(markers$lng), 4),
          input$threshold,
          input$method
        )
      )
    })
  })
}


# --- 3. About Module ---
# about_ui: Defines the UI for the static About page.
about_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    sidebar = sidebar(
      title = "Information",
      h5("Quick Links"),
      tags$ul(
        tags$li(tags$a("Documentation", href = "#", onclick = "return false;")),
        tags$li(tags$a("Support", href = "#", onclick = "return false;")),
        tags$li(tags$a("Contact", href = "#", onclick = "return false;"))
      ),
      br(),
      h5("Version Info"),
      p("Version: 1.0.0"),
      p("Last Updated: 2024")
    ),
    card(
      card_header("About This Application"),
      card_body(
        h4("Data Analytics Platform"),
        p("This application provides comprehensive data analysis capabilities with an intuitive interface."),
        h5("Features:"),
        tags$ul(
          tags$li("Interactive data analysis tools"),
          tags$li("GAP analysis with interactive mapping"),
          tags$li("Real-time visualization"),
          tags$li("Modular and scalable structure")
        )
      )
    )
  )
}

# about_server: Placeholder server logic (optional for static pages).
about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No reactive logic needed for this static page
  })
}


# --- 4. Main App UI ---
# Define UI
ui <- fluidPage(
  theme = bs_theme(version = 5),
  
  # Custom CSS to hide navbar on landing page (kept outside modules)
  tags$head(
    tags$style(HTML("
      .landing-page {
        height: 100vh;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        text-align: center;
      }
      
      .landing-title {
        font-size: 3rem;
        font-weight: bold;
        margin-bottom: 2rem;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
      }
      
      .landing-subtitle {
        font-size: 1.5rem;
        margin-bottom: 3rem;
        opacity: 0.9;
      }
      
      .enter-btn {
        padding: 15px 30px;
        font-size: 1.2rem;
        font-weight: bold;
        background-color: #ffffff;
        color = #667eea;
        border: none;
        border-radius: 50px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.2);
        transition: all 0.3s ease;
      }
      
      .enter-btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(0,0,0,0.3);
        background-color: #f8f9fa;
      }
      
      .hide-navbar .navbar {
        display: none !important;
      }
    "))
  ),
  
  # Landing Page (always rendered, conditionally shown)
  conditionalPanel(
    condition = "output.show_landing == true",
    div(
      class = "landing-page hide-navbar",
      div(
        class = "landing-title",
        "Welcome to Data Analytics Platform"
      ),
      div(
        class = "landing-subtitle",
        "Powerful insights at your fingertips"
      ),
      actionButton(
        "enter_app",
        "Start Analysis",
        class = "enter-btn"
      )
    )
  ),
  
  # Main App (always rendered, conditionally shown)
  conditionalPanel(
    condition = "output.show_landing == false",
    page_navbar(
      title = "Analytics Platform",
      id = "navbar",
      
      # Home Page (returns to landing - kept simple)
      nav_panel(
        title = "Home",
        value = "home",
        icon = icon("home"),
        div()
      ),
      
      # Analysis Page - using module UI
      nav_panel(
        title = "Analysis",
        value = "analysis",
        analysis_ui("analysis_mod")
      ),
      
      # GAP Page - using module UI
      nav_panel(
        title = "GAP",
        value = "gap",
        gap_ui("gap_mod")
      ),
      
      # About Page - using module UI
      nav_panel(
        title = "About",
        value = "about",
        about_ui("about_mod")
      )
    )
  )
)


# --- 5. Main App Server ---
# Define server logic
server <- function(input, output, session) {
  
  # Central state management for landing page
  current_page <- reactiveVal("landing")
  
  # Control landing page visibility
  output$show_landing <- reactive({
    current_page() == "landing"
  })
  outputOptions(output, "show_landing", suspendWhenHidden = FALSE)
  
  # Observer for navigation from landing to main app
  observeEvent(input$enter_app, {
    # Set the initial page to 'analysis' upon entering the main app
    current_page("analysis") 
    # Must use updateNavbarPage to actually switch the tab when visibility changes
    updateNavbarPage(session, "navbar", selected = "analysis")
  })
  
  # Observer to handle navbar navigation including returning to Home/Landing
  observeEvent(input$navbar, {
    if (input$navbar == "home") {
      current_page("landing")
    } else {
      current_page(input$navbar)
    }
  })
  
  # Call module servers
  analysis_server("analysis_mod")
  gap_server("gap_mod")
  about_server("about_mod")
}

# Run the application
shinyApp(ui = ui, server = server)