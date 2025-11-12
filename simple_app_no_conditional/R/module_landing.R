# LANDING PAGE MODULE -------------------------------------------------------------------
landingUI <- function(id) {
  ns <- NS(id)
  div(
    class = "landing-page",
    div(
      class = "hero-section",
      tags$img(
        class = "hero-image",
        src = "https://plus.unsplash.com/premium_photo-1690031000842-1ac0508f18b7?ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&q=80&w=1470"
      ),
      div(class = "hero-overlay"),
      div(
        class = "hero-content",
        h1("GAMMA"),
        p("Observing the meta collection.", style = "font-size: 1.2rem;")
      )
    ),
    div(
      class = "container content-section",
      div(
        class = "features-container",

        # --- Feature Box 1 (Now wrapped in an actionLink) ---
        actionLink(
          ns("show_gather"),
          label = div(
            class = "feature-box",
            tags$img(
              src = "https://images.unsplash.com/photo-1516738901171-8eb4fc13bd20?ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&q=80&w=1470"
            ),
            h4("Gather your Data"),
            p(
              "Upload your datasets, compare against public data, and prepare them for analysis."
            )
          )
        ),
        div(
          class = "feature-box",
          tags$img(
            src = "https://images.unsplash.com/photo-1730804518415-75297e8d2a41?ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&q=80&w=1462"
          ),
          h4("Find the gaps"),
          p("Geographic gap analysis to locations to priortize collections.")
        ),
        div(class = "feature-arrow", HTML("&#8594;")),
        div(
          class = "feature-box",
          tags$img(
            src = "https://plus.unsplash.com/premium_photo-1726754516964-7ee4209343a6?ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&q=80&w=1470"
          ),
          h4("Share the results"),
          p("Export and share your findings")
        )
      ),
      div(
        class = "nav-buttons",
        actionButton(ns("launch"), "Get Started", class = "btn-primary btn-lg"),
        actionButton(ns("learn_more"), "Learn More", class = "btn-info btn-lg")
      )
    ),
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

landingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(
      list(
        launch = reactive(input$launch),
        learn_more = reactive(input$learn_more)
      )
    )
  })
}
