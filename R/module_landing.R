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
        includeMarkdown(
          "appData/gather_modal.md"
        ),
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