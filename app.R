# =============================================================================
# CHARISMAM — visionneuse d'images de mammifères
# -----------------------------------------------------------------------------
# Les images sont lues directement sur Scaleway (bucket public).
# La liste des images vient de images.csv (créé par scripts/viewer_manifest.R).
#
# En local :  shiny::runApp("app/viewer")
# En ligne :  publiée sur GitHub Pages (Shinylive) à chaque push sur main,
#             via .github/workflows/deploy-viewer.yml
# =============================================================================

library(shiny)

base_url <- "https://biodiful-bucket.s3.fr-par.scw.cloud/charismam/pool/"

img <- read.csv("images.csv", stringsAsFactors = FALSE)
img$url <- paste0(base_url, img$file)
multi   <- ave(img$n, img$species, img$type, FUN = max) > 1   # plusieurs photos du même type
img$lab <- ifelse(multi, paste(img$type, img$n), img$type)

sp_tab  <- unique(img[, c("species", "family", "order")])
sp_tab  <- sp_tab[order(sp_tab$species), ]
fam_tab <- aggregate(species ~ family + order, data = sp_tab, FUN = length)
fam_tab <- fam_tab[order(fam_tab$family), ]

fam_choices <- c("Toutes les familles" = "",
                 setNames(fam_tab$family,
                          sprintf("%s (%s, %d)", fam_tab$family, fam_tab$order, fam_tab$species)))

# Lien cliquable vers une espèce (nom en italique)
sp_link <- function(sp) {
  tags$a(href = "#", class = "sp-link",
         onclick = sprintf("Shiny.setInputValue('go_sp', '%s', {priority: 'event'}); return false;", sp),
         tags$em(sp))
}

# Vignette : image + légende (espèce en italique, type de photo)
card <- function(r, clickable = FALSE) {
  im <- tags$img(src = r$url, loading = "lazy", alt = r$species)
  im <- if (clickable) {
    tags$a(href = "#", title = "Voir l'espèce",
           onclick = sprintf("Shiny.setInputValue('go_sp', '%s', {priority: 'event'}); return false;", r$species),
           im)
  } else {
    tags$a(href = r$url, target = "_blank", title = "Ouvrir en grand", im)
  }
  tags$figure(class = "card", im,
              tags$figcaption(tags$em(r$species), tags$span(class = "type", r$lab)))
}

ui <- fluidPage(
  title = "CHARISMAM — visionneuse",
  tags$head(tags$style(HTML("
    body { background: #fafafa; }
    h2.app-title { margin: 18px 0 4px; font-weight: 600; }
    .sub { color: #777; margin-bottom: 14px; }
    .gallery { display: grid; grid-template-columns: repeat(auto-fill, minmax(230px, 1fr)); gap: 16px; }
    .gallery.big { grid-template-columns: repeat(auto-fill, minmax(340px, 1fr)); }
    .card { margin: 0; background: #fff; border: 1px solid #e3e3e3; border-radius: 6px; overflow: hidden; }
    .card img { width: 100%; aspect-ratio: 1 / 1; object-fit: cover; display: block; background: #eee; }
    .card figcaption { padding: 6px 10px; font-size: 14px; display: flex; justify-content: space-between; gap: 8px; }
    .card .type { color: #888; font-size: 12px; white-space: nowrap; }
    .sp-head { margin: 4px 0 14px; }
    .sp-head h3 { margin: 0; font-style: italic; }
    .sp-head .tax { color: #777; }
    .sp-list { max-height: 45vh; overflow-y: auto; padding-left: 2px; }
    .sp-list a { display: block; padding: 2px 0; }
    .sp-list a.current { font-weight: 700; }
    .hint { color: #888; margin-top: 40px; text-align: center; font-size: 16px; }
  "))),
  h2(class = "app-title", "CHARISMAM — visionneuse"),
  div(class = "sub", sprintf("%d espèces, %d familles, %d photos",
                             nrow(sp_tab), nrow(fam_tab), nrow(img))),
  sidebarLayout(
    sidebarPanel(width = 3,
      selectizeInput("family", "Famille", choices = fam_choices,
                     options = list(placeholder = "Toutes les familles")),
      selectizeInput("species", "Espèce", choices = c("Tapez un nom..." = "", sp_tab$species),
                     options = list(placeholder = "Tapez un nom...")),
      radioButtons("mode", "Afficher",
                   choices = c("L'espèce choisie" = "species", "Toute la famille" = "family")),
      uiOutput("sp_list")
    ),
    mainPanel(width = 9, uiOutput("gallery"))
  )
)

server <- function(input, output, session) {

  # Famille choisie -> restreint la liste des espèces
  observeEvent(input$family, {
    sp <- if (input$family == "") sp_tab$species else sp_tab$species[sp_tab$family == input$family]
    keep <- if (input$species %in% sp) input$species else ""
    updateSelectizeInput(session, "species", choices = c("Tapez un nom..." = "", sp), selected = keep)
  }, ignoreInit = TRUE)

  # Espèce choisie -> cale la famille dessus
  observeEvent(input$species, {
    if (input$species == "") return()
    f <- sp_tab$family[sp_tab$species == input$species][1]
    if (!identical(input$family, f)) updateSelectizeInput(session, "family", selected = f)
  })

  # Clic sur un nom ou une photo -> affiche l'espèce
  observeEvent(input$go_sp, {
    f <- sp_tab$family[sp_tab$species == input$go_sp][1]
    updateSelectizeInput(session, "family", selected = f)
    updateSelectizeInput(session, "species",
                         choices = c("Tapez un nom..." = "", sp_tab$species[sp_tab$family == f]),
                         selected = input$go_sp)
    updateRadioButtons(session, "mode", selected = "species")
  })

  # Liste cliquable des espèces de la famille
  output$sp_list <- renderUI({
    if (input$family == "") return(NULL)
    sp <- sp_tab$species[sp_tab$family == input$family]
    tagList(tags$label(sprintf("Espèces de la famille (%d)", length(sp))),
            div(class = "sp-list", lapply(sp, function(s) {
              a <- sp_link(s)
              if (identical(s, input$species)) a$attribs$class <- "sp-link current"
              a
            })))
  })

  output$gallery <- renderUI({
    if (input$mode == "family") {
      if (input$family == "")
        return(div(class = "hint", "Choisissez une famille pour voir toutes ses photos."))
      d  <- img[img$family == input$family, ]
      fo <- fam_tab[fam_tab$family == input$family, ]
      tagList(
        div(class = "sp-head", h3(style = "font-style:normal", input$family),
            span(class = "tax", sprintf("%s — %d espèces, %d photos (cliquer une photo pour voir l'espèce)",
                                        fo$order, fo$species, nrow(d)))),
        div(class = "gallery", lapply(seq_len(nrow(d)), function(i) card(d[i, ], clickable = TRUE)))
      )
    } else {
      if (input$species == "")
        return(div(class = "hint", "Cherchez une espèce par son nom, ou choisissez une famille puis cliquez sur une espèce."))
      d  <- img[img$species == input$species, ]
      d  <- d[order(d$n, d$type), ]                  # all puis head, photo 1 puis 2
      tagList(
        div(class = "sp-head", h3(input$species),
            span(class = "tax", sprintf("%s — %s", d$family[1], d$order[1]))),
        div(class = "gallery big", lapply(seq_len(nrow(d)), function(i) card(d[i, ])))
      )
    }
  })
}

shinyApp(ui, server)
