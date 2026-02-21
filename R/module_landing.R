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

    # Track which section is currently active
    # Values: NULL (closed), "gather", "find", "share"
    active_section <- reactiveVal(NULL)

    # --- Toggle Logic Helper ---
    toggle_section <- function(section_name) {
      current <- active_section()
      if (!is.null(current) && current == section_name) {
        # If clicking the already open section, close it
        active_section(NULL)
      } else {
        # Otherwise, open the new section
        active_section(section_name)
      }
    }

    # --- Observers for Clicks ---
    observeEvent(input$show_gather, { toggle_section("gather") })
    observeEvent(input$show_find,   { toggle_section("find") })
    observeEvent(input$show_share,  { toggle_section("share") })

    # --- Render the Expandable Panel ---
    output$feature_details <- renderUI({
      req(active_section()) # Only render if a section is active
      
      section <- active_section()
      
      # Determine content based on active section
      content <- switch(section,
        "gather" = includeMarkdown("appData/gather_modal.md"),
        "find"   = includeMarkdown("appData/find_gaps.md"),
        "share"  = div(
                     h3(landing_text$share$modal_title),
                     p(landing_text$share$modal_text, style = "font-size: 1.1rem; line-height: 1.6;")
                   )
      )

      div(
        class = "feature-details-panel slide-down",
        content,
        # Internal Close Button
        actionButton(
          ns("close_details"),
          "Close Section",
          class = "btn btn-sm btn-outline-secondary mt-3"
        )
      )
    })

    # --- Handle Close Button ---
    observeEvent(input$close_details, {
      active_section(NULL)
    })

    # --- Visual "Active" State Management ---
    observe({
      current <- active_section()
      
      # Reset all boxes first
      shinyjs::removeClass(id = "box_gather", class = "active-feature-box")
      shinyjs::removeClass(id = "box_find",   class = "active-feature-box")
      shinyjs::removeClass(id = "box_share",  class = "active-feature-box")
      
      # Highlight the active one
      if (!is.null(current)) {
        if (current == "gather") shinyjs::addClass(id = "box_gather", class = "active-feature-box")
        if (current == "find")   shinyjs::addClass(id = "box_find",   class = "active-feature-box")
        if (current == "share")  shinyjs::addClass(id = "box_share",  class = "active-feature-box")
      }
    })

    return(list(
      launch = reactive(input$launch),
      learn_more = reactive(input$learn_more)
    ))
  })
}