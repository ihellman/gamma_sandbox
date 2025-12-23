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

        # BOX 1: Triggers Modal
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

        # BOX 2: Triggers Expandable Panel (Hybrid)
        actionLink(
          ns("show_find"),
          label = div(
            id = ns("box_find"), # ID needed for adding 'active' class
            class = "feature-box",
            tags$img(
              src = "https://images.unsplash.com/photo-1730804518415-75297e8d2a41?ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&q=80&w=1462"
            ),
            h4(landing_text$find$title),
            p(landing_text$find$text)
          )
        ),

        # BOX 3: Triggers Modal
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
      ),

      # --- DETAILS SECTION (Only used by Box 2) ---
      uiOutput(ns("feature_details"))
    ),

    # --- FOOTER ---
    div(
      class = "footer-banner",
      tags$a(
        href = "https://www.bgci.org/",
        target = "_blank",
        tags$img(src = "bgci.png", alt = "BGCI Logo")
      ),
      tags$a(
        href = "https://www.usbg.gov/",
        target = "_blank",
        tags$img(src = "abg.jpg", alt = "USBG Logo")
      )
    )
  )
}

landingServer <- function(id, landing_text) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Track selection ONLY for the expandable box (Box 2)
    # TRUE = Open, FALSE = Closed
    find_box_open <- reactiveVal(FALSE)

    # --- BOX 1: Modal (Legacy) ---
    observeEvent(input$show_gather, {
      showModal(modalDialog(
        title = landing_text$gather$modal_title,
        includeMarkdown("appData/gather_modal.md"),
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "xl"
      ))
    })

    # --- BOX 2: Expandable Panel (New Method) ---
    observeEvent(input$show_find, {
      # Toggle the state
      find_box_open(!find_box_open())
    })

    output$feature_details <- renderUI({
      req(find_box_open()) # Only render if True

      div(
        class = "feature-details-panel slide-down",
        includeMarkdown("appData/find_gaps.md"),
        # Internal Close Button
        actionButton(
          ns("close_details"),
          "Close Section",
          class = "btn btn-sm btn-outline-secondary mt-3"
        )
      )
    })

    # Handle the internal close button
    observeEvent(input$close_details, {
      find_box_open(FALSE)
    })

    # Handle visual "Active" state for Box 2
    observe({
      if (find_box_open()) {
        shinyjs::addClass(id = "box_find", class = "active-feature-box")
      } else {
        shinyjs::removeClass(id = "box_find", class = "active-feature-box")
      }
    })

    # --- BOX 3: Modal (Legacy) ---
    observeEvent(input$show_share, {
      showModal(modalDialog(
        title = landing_text$share$modal_title,
        p(landing_text$share$modal_text),
        footer = modalButton("Close"),
        easyClose = TRUE,
      ))
    })

    return(list(
      launch = reactive(input$launch),
      learn_more = reactive(input$learn_more)
    ))
  })
}
