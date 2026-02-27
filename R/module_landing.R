landingUI <- function(id, landing_text) {
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

    # --- ACTION BAR ---
    div(
      class = "action-bar-container",
      div(
        class = "container",
        div(
          class = "action-bar-buttons",
          actionButton(
            ns("launch"),
            "Get Started",
            class = "btn btn-outline-success btn-lg"
          ),
          actionButton(
            ns("learn_more"),
            "Learn More",
            class = "btn btn-outline-success btn-lg"
          )
        )
      )
    ),

    # --- MAIN CONTENT SECTION ---
    div(
      class = "container content-section",

      div(
        class = "summary-section",
        p(landing_text$summary$text1),
        p(landing_text$summary$text2)
      ),

      # --- FEATURES CONTAINER ---
      div(
        class = "features-container",

        # BOX 1: Gather
        actionLink(
          ns("show_gather"),
          label = div(
            id = ns("box_gather"), # Added ID for active state styling
            class = "feature-box",
            tags$img(
              src = "https://images.unsplash.com/photo-1516738901171-8eb4fc13bd20?ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&q=80&w=1470"
            ),
            h4(landing_text$gather$title),
            p(landing_text$gather$text)
          )
        ),

        # BOX 2: Find
        actionLink(
          ns("show_find"),
          label = div(
            id = ns("box_find"), 
            class = "feature-box",
            tags$img(
              src = "https://images.unsplash.com/photo-1730804518415-75297e8d2a41?ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&q=80&w=1462"
            ),
            h4(landing_text$find$title),
            p(landing_text$find$text)
          )
        ),

        # BOX 3: Share
        actionLink(
          ns("show_share"),
          label = div(
            id = ns("box_share"), # Added ID for active state styling
            class = "feature-box",
            tags$img(
              src = "https://plus.unsplash.com/premium_photo-1726754516964-7ee4209343a6?q=80&w=1740&auto=format&fit=crop&ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D"
            ),
            h4(landing_text$share$title),
            p(landing_text$share$text)
          )
        )
      ),

      # --- DETAILS SECTION (Shared by all boxes) ---
      uiOutput(ns("feature_details"))
    ),

    # --- FOOTER ---
    footer_ui()
  )
}

landingServer <- function(id, landing_text) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Track which sections are currently active as a character vector
    # Values: character(0) (all closed), or any combination of "gather", "find", "share"
    active_sections <- reactiveVal(character(0))
    
    # --- Toggle Logic Helper ---
    toggle_section <- function(section_name) {
      current <- active_sections()
      
      if (section_name %in% current) {
        # If it's already open, remove it from the list
        active_sections(setdiff(current, section_name))
      } else {
        # If it's closed, add it to the list
        active_sections(c(current, section_name))
      }
    }
    
    # --- Observers for Clicks ---
    observeEvent(input$show_gather, { toggle_section("gather") })
    observeEvent(input$show_find,   { toggle_section("find") })
    observeEvent(input$show_share,  { toggle_section("share") })
    
    # --- Render the Expandable Panel ---
    output$feature_details <- renderUI({
      current <- active_sections()
      req(length(current) > 0) # Only render if at least one section is active
      
      # --- THE FIX: Force the display order ---
      master_order <- c("gather", "find", "share")
      ordered_sections <- intersect(master_order, current)
      
      # Build a list of UI elements for all active sections
      # Notice we now loop over 'ordered_sections' instead of 'current'
      stacked_content <- lapply(ordered_sections, function(section) {
        content <- switch(section,
                          "gather" = includeMarkdown("appData/gather_modal.md"),
                          "find"   = includeMarkdown("appData/find_gaps.md"),
                          "share"  = includeMarkdown("appData/share_results.md")
        )
        
        # Wrap each section in a div with a bottom border to separate them
        div(class = "mb-4 pb-3 border-bottom", content)
      })
      
      div(
        class = "feature-details-panel slide-down",
        tagList(stacked_content),
        # Internal Close Button 
        actionButton(
          ns("close_details"),
          "Close All Sections",
          class = "btn btn-sm btn-outline-secondary mt-3"
        )
      )
    })
    
    # --- Handle Close Button ---
    observeEvent(input$close_details, {
      # Clear all selections
      active_sections(character(0)) 
    })
    
    # --- Visual "Active" State Management ---
    observe({
      current <- active_sections()
      
      # Helper function to toggle classes based on presence in the 'current' vector
      toggle_box_class <- function(box_id, sec_name) {
        if (sec_name %in% current) {
          shinyjs::addClass(id = box_id, class = "active-feature-box")
        } else {
          shinyjs::removeClass(id = box_id, class = "active-feature-box")
        }
      }
      
      toggle_box_class("box_gather", "gather")
      toggle_box_class("box_find", "find")
      toggle_box_class("box_share", "share")
    })
    
    return(list(
      launch = reactive(input$launch),
      learn_more = reactive(input$learn_more)
    ))
  })
}