landingUI <- function(id, landing_text) {
  ns <- NS(id)
  
  div(
    class = "landing-page",
    # --- HERO SECTION ---
    div(
      class = "hero-section",
      # UPDATE THIS LINE: Match the exact name and extension of your background image
      tags$img(class = "hero-image", src = "background_photo.jpg"),
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
            id = ns("box_gather"), 
            class = "feature-box",
            # UPDATE THIS LINE: Match the exact name of your people/gather image
            tags$img(src = "gather_photo.jpg"),
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
            # UPDATE THIS LINE: Match the exact name of your cycad image
            tags$img(src = "cycad.jpg"),
            h4(landing_text$find$title),
            p(landing_text$find$text)
          )
        ),
        
        # BOX 3: Share
        actionLink(
          ns("show_share"),
          label = div(
            id = ns("box_share"), 
            class = "feature-box",
            # UPDATE THIS LINE: Match the exact name of your seedling image
            tags$img(src = "seedling.jpg"),
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
    active_sections <- reactiveVal(character(0))
    
    # --- Toggle Logic Helper ---
    toggle_section <- function(section_name) {
      current <- active_sections()
      
      if (section_name %in% current) {
        active_sections(setdiff(current, section_name))
      } else {
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
      req(length(current) > 0)
      
      master_order <- c("gather", "find", "share")
      ordered_sections <- intersect(master_order, current)
      
      stacked_content <- lapply(ordered_sections, function(section) {
        content <- switch(section,
                          "gather" = includeMarkdown("appData/gather_modal.md"),
                          "find"   = includeMarkdown("appData/find_gaps.md"),
                          "share"  = includeMarkdown("appData/share_results.md")
        )
        
        div(class = "mb-4 pb-3 border-bottom", content)
      })
      
      div(
        class = "feature-details-panel slide-down",
        tagList(stacked_content),
        actionButton(
          ns("close_details"),
          "Close All Sections",
          class = "btn btn-sm btn-outline-secondary mt-3"
        )
      )
    })
    
    # --- Handle Close Button ---
    observeEvent(input$close_details, {
      active_sections(character(0)) 
    })
    
    # --- Visual "Active" State Management ---
    observe({
      current <- active_sections()
      
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