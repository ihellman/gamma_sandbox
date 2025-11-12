# app.R
library(shiny)
library(bslib)
library(shinyjs)
library(shiny)
library(bslib)
library(readr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(shiny)
library(reactable)
library(yaml)
library(markdown)
library(here)

# Load text from YAML file
landing_text <- read_yaml("appData/landing_text.yml")

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

.feature-arrow {
  font-size: 2.5rem;
  color: #cccccc;
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

# LANDING PAGE MODULE -------------------------------------------------------------------
landingUI <- function(id, landing_text) {
  # <-- Renamed
  ns <- NS(id)
  div(
    class = "landing-page",
    # --- HERO SECTION ---
    div(
      class = "hero-section",
      tags$img(
        class = "hero-image",
        src = "https://plus.unsplash.com/premium_photo-1690031000842-1ac0508f18b7?ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&q=80&w=1470"
      ),
      div(class = "hero-overlay"),
      div(
        class = "hero-content",
        h1(landing_text$hero$title),
        p(landing_text$hero$subtitle, style = "font-size: 1.2rem;")
      )
    ),

    # --- FULL-WIDTH ACTION BAR ---
    div(
      class = "action-bar-container",
      div(
        class = "container",
        div(
          class = "action-bar-buttons",
          # tags$a(
          #   "Botanic Garden Conservation International",
          #   href = "https://www.bgci.org/",
          #   target = "_blank",
          #   rel = "noopener noreferrer",
          #   class = "btn btn-outline-secondary btn-lg"
          # ),
          actionButton(
            ns("launch"),
            "Get Started",
            class = "btn btn-outline-success btn-lg"
          ),
          class = "btn-group-right",
          actionButton(
            ns("learn_more"),
            "Learn More",
            class = "btn btn-outline-success btn-lg"
          ),
          # tags$a(
          #   "Global Conservation Consortia (GCC)",
          #   href = "https://www.bgci.org/our-work/networks/global-conservation-consortia-gcc/",
          #   target = "_blank",
          #   rel = "noopener noreferrer",
          #   class = "btn btn-outline-secondary btn-lg"
          # )
        )
      )
    ),

    # --- MAIN CONTENT SECTION ---
    div(
      class = "container content-section",

      # --- SUMMARY SECTION ---
      div(
        class = "summary-section",
        p(landing_text$summary$text1),
        p(landing_text$summary$text2)
      ),

      # --- FEATURES CONTAINER ---
      div(
        class = "features-container",

        # --- Feature Box 1 (from YAML) ---
        actionLink(
          ns("show_gather"),
          label = div(
            class = "feature-box",
            tags$img(
              src = "https://images.unsplash.com/photo-1516738901171-8eb4fc13bd20?ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&q=80&w=1470"
            ),
            h4(landing_text$gather$title),
            p(landing_text$gather$text)
          )
        ),

        # --- Feature Box 2 (from YAML) ---
        actionLink(
          ns("show_find"),
          label = div(
            class = "feature-box",
            tags$img(
              src = "https://images.unsplash.com/photo-1730804518415-75297e8d2a41?ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&q=80&w=1462"
            ),
            h4(landing_text$find$title),
            p(landing_text$find$text)
          )
        ),

        # --- Feature Box 3 (from YAML) ---
        actionLink(
          ns("show_share"),
          label = div(
            class = "feature-box",
            tags$img(
              src = "https://plus.unsplash.com/premium_photo-1726754516964-7ee4209343a6?q=80&w=1740&auto=format&fit=crop&ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D"
            ),
            h4(landing_text$share$title),
            p(landing_text$share$text)
          )
        )
      )
    ),

    # --- FOOTER ---
    div(
      class = "footer-banner",
      tags$a(
        href = "https://www.bgci.org/",
        target = "_blank",
        tags$img(
          src = "https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/Botanic_Gardens_Conservation_International_logo.svg/320px-Botanic_Gardens_Conservation_International_logo.svg.png",
          alt = "Botanic Gardens Conservation International Logo"
        )
      ),
      tags$a(
        href = "https://www.usbg.gov/",
        target = "_blank",
        tags$img(
          src = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5a/US_Botanic_Garden_logo.svg/320px-US_Botanic_Garden_logo.svg.png",
          alt = "US Botanic Garden Logo"
        )
      )
    )
  )
}
landingServer <- function(id, landing_text) {
  # <-- Renamed
  moduleServer(id, function(input, output, session) {
    # --- Modal for "Gather Data" ---
    observeEvent(input$show_gather, {
      showModal(modalDialog(
        title = landing_text$gather$modal_title,
        includeMarkdown(here::here(
          # "simple_app_no_conditional", # if running localing this is required, need to  commit out before deploying
          "appData",
          "gather_modal.md"
        )),
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "xl"
      ))
    })

    # --- Modal for "Find Gaps" ---
    observeEvent(input$show_find, {
      showModal(modalDialog(
        title = landing_text$find$modal_title,
        p(landing_text$find$modal_text),
        footer = modalButton("Close"),
        easyClose = TRUE
      ))
    })

    # --- Modal for "Share Results" ---
    observeEvent(input$show_share, {
      showModal(modalDialog(
        title = landing_text$share$modal_title,
        p(landing_text$share$modal_text),
        footer = modalButton("Close"),
        easyClose = TRUE,
      ))
    })

    # --- Return the launch button actions ---
    return(
      list(
        launch = reactive(input$launch),
        learn_more = reactive(input$learn_more)
      )
    )
  })
}

# DATA ANALYSIS MODULE ------------------------------------------------------------------
dataAnalysisUI <- function(id) {
  ns <- NS(id)
  div(
    controlsModuleUI("controls"),
    mapModuleUI(ns("map")),
    tableModuleUI(ns("table"))
  )
}

dataAnalysisServer <- function(id, combined_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    ## Working just being called at the end of the application?
    # controlsModuleServer("controls", combined_data, selected_points)
    mapModuleServer("map", combined_data, selected_points)
    tableModuleServer("table", combined_data, selected_points)
  })
}
# Controls Module
controlsModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(inputId = ns("loadGBIF"), label = "Load GBIF"),
    br(),
    br(),
    actionButton(inputId = ns("loadupload"), label = "Load Upload"),
    br(),
    br(),
    actionButton(inputId = ns("clearSelection"), label = "Clear Selection"),
    br(),
    br(),
    actionButton(
      inputId = ns("deleteSelection"),
      label = "Delete Selection",
      class = "btn-danger"
    ),
    br()
  )
}

controlsModuleServer <- function(id, combined_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    # Load sample data (for testing)
    gbifPoints <- read_csv(
      "appData/Magnolia_acuminata_data.csv",
      col_types = cols(.default = "c")
    ) %>%
      sf::st_as_sf(
        coords = c("Longitude", "Latitude"),
        crs = 4326,
        remove = FALSE
      ) %>%
      mutate(index = dplyr::row_number())

    uploadPoints <- read_csv(
      "appData/upload_sample.csv",
      col_types = cols(.default = "c")
    ) %>%
      sf::st_as_sf(
        coords = c("Longitude", "Latitude"),
        crs = 4326,
        remove = FALSE
      ) %>%
      mutate(index = dplyr::row_number())

    # Load GBIF data
    observeEvent(input$loadGBIF, {
      current <- combined_data()

      if (nrow(current) == 0) {
        combined_data(gbifPoints)
      } else {
        new_dat <- bind_rows(current, gbifPoints) %>%
          mutate(index = row_number())
        combined_data(new_dat)
      }
    })

    # Load upload data
    observeEvent(input$loadupload, {
      current <- combined_data()

      if (nrow(current) == 0) {
        combined_data(uploadPoints)
      } else {
        new_dat <- bind_rows(current, uploadPoints) %>%
          mutate(index = row_number())
        combined_data(new_dat)
      }
    })

    # Clear selection
    observeEvent(input$clearSelection, {
      selected_points(numeric(0))
    })

    # Delete selected points
    observeEvent(input$deleteSelection, {
      req(nrow(combined_data()) > 0)
      current_data <- combined_data()
      current_selection <- selected_points()

      # Only proceed if there are selected points
      #if (length(current_selection) > 0 && nrow(current_data) > 0) {
      # Remove selected rows
      updated_data <- current_data %>%
        filter(!index %in% current_selection) %>%
        mutate(index = row_number()) # Re-index after deletion

      # Update the data
      combined_data(updated_data)

      # Clear the selection
      selected_points(numeric(0))
      #}
    })
  })
}

# MAP MODULE -----------------------------------------------------------------------------

mapModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("dataEvalMap"), height = "50vh") # vh = viewport height. 50% doesnt work.
  )
}

mapModuleServer <- function(id, combined_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    # Initial map render
    output$dataEvalMap <- renderLeaflet({
      data_eval_base_map()
    })

    # Update map when data or selection changes
    observe({
      data_eval_map_gbif_proxy(
        mapID = "dataEvalMap",
        allPoints = combined_data(),
        selected = selected_points()
      )
    })

    # Handle marker clicks
    observeEvent(input$dataEvalMap_marker_click, {
      click <- input$dataEvalMap_marker_click
      current_selected <- selected_points()

      if (click$id %in% current_selected) {
        selected_points(setdiff(current_selected, click$id))
      } else {
        selected_points(c(current_selected, click$id))
      }
    })

    # Handle polygon selection
    observeEvent(input$dataEvalMap_draw_new_feature, {
      req(nrow(combined_data()) > 0)
      currentData <- combined_data()

      # Extract polygon feature
      feature <- input$dataEvalMap_draw_new_feature
      coords <- feature$geometry$coordinates[[1]]
      selectionPoly <- st_polygon(list(matrix(
        unlist(coords),
        ncol = 2,
        byrow = TRUE
      )))

      # Find points within the polygon
      pointsInPoly <- st_filter(currentData, selectionPoly)

      # Get index values for new points
      newPolySelection <- pointsInPoly %>% pull(index)

      # Combine with previously selected and save
      newSelection <- c(selected_points(), newPolySelection) %>% unique()
      selected_points(newSelection)
    })
  })
}

# TABLE MODULE -------------------------------------------------------------------------
tableModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    reactableOutput(outputId = ns("pointsTable"))
  )
}

tableModuleServer <- function(id, combined_data, selected_points) {
  moduleServer(id, function(input, output, session) {
    output$pointsTable <- renderReactable({
      req(nrow(combined_data()) > 0)
      data <- st_drop_geometry(combined_data())
      selected <- selected_points()

      calc_width <- function(text) {
        max(100, nchar(text) * 12) # Roughly 8px per character + padding
      }

      reactable(
        data = data,
        selection = "multiple",
        defaultSelected = which(data$index %in% selected),
        onClick = "select",
        highlight = TRUE,
        pagination = FALSE,
        groupBy = "source",
        columns = lapply(names(data), function(col) {
          colDef(
            minWidth = calc_width(col)
          )
        }) %>%
          setNames(names(data))
      )
    })

    # Handle table row selection
    observeEvent(
      getReactableState("pointsTable", "selected"),
      {
        table_selected <- getReactableState("pointsTable", "selected")
        print(table_selected)
        if (length(table_selected) > 0) {
          selected_points(combined_data()$index[table_selected])
        } else {
          selected_points(numeric(0))
        }
      },
      ignoreNULL = FALSE
    )
  })
}

# Gap ANALYSIS MODULE ----------------------------------------------------------------
gapAnalysisUI <- function(id) {
  ns <- NS(id)
  div(
    h2("Gap Analysis"),
    p("This is the Gap Analysis page."),
    plotOutput(ns("plot"))
  )
}

gapAnalysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      plot(
        1:10,
        1:10,
        main = "Sample Gap Analysis Plot",
        xlab = "X",
        ylab = "Y",
        col = "purple",
        pch = 19,
        cex = 2
      )
    })
  })
}

# ABOUT MODULE ------------------------------------------------------------------------
aboutUI <- function(id) {
  ns <- NS(id)
  div(
    h2("About"),
    p("more to come")
  )
}

aboutServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server logic needed
  })
}

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
  nav_panel("Gap Analysis", value = "gap", gapAnalysisUI("gap_analysis")),
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
shinyApp(ui, server) # not capturing the relative paths of objects
# using the instead
