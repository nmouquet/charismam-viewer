# =============================================================================
# CHARISMAM — visionneuse d'images de mammifères
# -----------------------------------------------------------------------------
# Les images sont lues directement sur Scaleway (bucket public).
# La liste des images vient de images.csv (créé par scripts/deploy_viewer.R).
#
# En local :  shiny::runApp("app/viewer")
# En ligne :  https://nmouquet.github.io/charismam-viewer/ (Shinylive, GitHub Pages)
# =============================================================================

library(shiny)

base_url <- "https://biodiful-bucket.s3.fr-par.scw.cloud/charismam/pool/"

# Commentaires : URL de l'Apps Script Google (fichier écrit par deploy_viewer.R).
# Sans ce fichier, la partie commentaires n'apparaît pas.
comments_url <- if (file.exists("comments_url.txt")) trimws(readLines("comments_url.txt", warn = FALSE)[1]) else ""
if (is.na(comments_url)) comments_url <- ""

img <- read.csv("images.csv", stringsAsFactors = FALSE)
img$url <- paste0(base_url, img$file)
multi   <- ave(img$n, img$species, img$type, FUN = max) > 1   # plusieurs photos du même type
img$lab <- ifelse(multi, paste(img$type, img$n), img$type)

sp_tab  <- unique(img[, c("species", "family", "order")])
sp_tab  <- sp_tab[order(sp_tab$species), ]
fam_tab <- aggregate(species ~ family + order, data = sp_tab, FUN = length)
fam_tab <- fam_tab[order(fam_tab$family), ]
ord_tab <- aggregate(species ~ order, data = sp_tab, FUN = length)
ord_tab <- ord_tab[order(ord_tab$order), ]

ALL <- ""   # valeur "tous / toutes" dans les menus

ord_choices <- function() c("Tous les ordres" = ALL,
  setNames(ord_tab$order, sprintf("%s (%d)", ord_tab$order, ord_tab$species)))
fam_choices <- function(ord) {
  f <- if (ord == ALL) fam_tab else fam_tab[fam_tab$order == ord, ]
  c("Toutes les familles" = ALL, setNames(f$family, sprintf("%s (%d)", f$family, f$species)))
}
sp_choices <- function(ord, fam) {
  s <- sp_tab
  if (ord != ALL) s <- s[s$order == ord, ]
  if (fam != ALL) s <- s[s$family == fam, ]
  c("Tapez un nom..." = ALL, s$species)
}

# Vignette : image (clic = agrandir) + légende (espèce en italique, type de photo)
card <- function(r) {
  cap <- sprintf("<i>%s</i> &mdash; %s", r$species, r$lab)
  tags$figure(class = "card",
    tags$img(src = r$url, loading = "lazy", alt = r$species, title = "Agrandir",
             onclick = sprintf("showBig('%s', '%s')", r$url, cap)),
    tags$figcaption(tags$em(r$species), tags$span(class = "type", r$lab)))
}
gallery <- function(d, big = FALSE)
  div(class = if (big) "gallery big" else "gallery", lapply(seq_len(nrow(d)), function(i) card(d[i, ])))

ui <- fluidPage(
  title = "CHARISMAM — visionneuse",
  tags$head(
    tags$style(HTML("
      body { background: #fafafa; }
      h2.app-title { margin: 18px 0 4px; font-weight: 600; }
      .sub { color: #777; margin-bottom: 14px; }
      .gallery { display: grid; grid-template-columns: repeat(auto-fill, minmax(200px, 1fr)); gap: 14px; }
      .gallery.big { grid-template-columns: repeat(auto-fill, minmax(340px, 1fr)); }
      .card { margin: 0; background: #fff; border: 1px solid #e3e3e3; border-radius: 6px; overflow: hidden; }
      .card img { width: 100%; aspect-ratio: 1 / 1; object-fit: cover; display: block; background: #eee; cursor: zoom-in; }
      .card figcaption { padding: 6px 10px; font-size: 14px; display: flex; justify-content: space-between; gap: 8px; }
      .card .type { color: #888; font-size: 12px; white-space: nowrap; }
      .head { margin: 4px 0 14px; }
      .head h3 { margin: 0; }
      .head .tax { color: #777; }
      .grp { margin: 22px 0 8px; font-size: 17px; border-bottom: 1px solid #ddd; padding-bottom: 3px; }
      .sp-list { max-height: 40vh; overflow-y: auto; }
      .sp-list a { display: block; padding: 2px 0; }
      .sp-list a.current { font-weight: 700; }
      .hint { color: #888; margin-top: 40px; text-align: center; font-size: 16px; }
      .com { margin-top: 28px; padding-top: 10px; border-top: 1px solid #ddd; max-width: 900px; }
      .com h4 { margin-bottom: 10px; }
      .com-item { background: #fff; border: 1px solid #e3e3e3; border-radius: 6px; padding: 8px 12px; margin-bottom: 8px; }
      .com-meta { color: #888; font-size: 12px; margin-bottom: 3px; }
      .com-text { white-space: pre-wrap; }
      .com-form { background: #fff; border: 1px solid #e3e3e3; border-radius: 6px; padding: 12px; margin-top: 12px; }
      .com-form .row2 { display: flex; gap: 10px; flex-wrap: wrap; }
      .com-form .row2 > * { flex: 1; min-width: 180px; }
      .com-form textarea { width: 100%; min-height: 80px; }
      #com-status { margin-left: 10px; color: #888; }
      #big { display: none; position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%);
             z-index: 9999; background: #fff; border: 1px solid #ccc; border-radius: 8px;
             box-shadow: 0 6px 30px rgba(0,0,0,.25); padding: 28px 12px 8px; cursor: zoom-out; }
      #big img { display: block; width: min(680px, 88vw, 78vh); height: auto; }
      #big .cap { margin-top: 8px; font-size: 15px; text-align: center; }
      #big .close { position: absolute; top: 2px; right: 10px; color: #666; font-size: 24px; line-height: 1; }
    ")),
    tags$script(HTML(sprintf("
      function showBig(src, cap) {
        var b = document.getElementById('big');
        document.getElementById('big-img').src = src;
        document.getElementById('big-cap').innerHTML = cap;
        b.style.display = 'block';
      }
      var COMMENTS_URL = '%s';
      function esc(x) { var d = document.createElement('div'); d.textContent = x == null ? '' : String(x); return d.innerHTML; }
      function loadComments(sp) {
        var box = document.getElementById('com-list'); if (!box || !COMMENTS_URL) return;
        box.innerHTML = '<span class=\"com-meta\">Chargement des commentaires...</span>';
        fetch(COMMENTS_URL + '?species=' + encodeURIComponent(sp))
          .then(function(r) { return r.json(); })
          .then(function(j) {
            var c = (j && j.comments) || [];
            if (!c.length) { box.innerHTML = '<span class=\"com-meta\">Aucun commentaire pour l&#39;instant.</span>'; return; }
            box.innerHTML = c.map(function(x) {
              return '<div class=\"com-item\"><div class=\"com-meta\"><b>' + esc(x.name) + '</b> &middot; ' + esc(x.date) +
                     (x.photo ? ' &middot; ' + esc(x.photo) : '') + '</div><div class=\"com-text\">' + esc(x.comment) + '</div></div>';
            }).join('');
          })
          .catch(function() { box.innerHTML = '<span class=\"com-meta\">Commentaires indisponibles pour le moment.</span>'; });
      }
      function sendComment(sp) {
        var name = document.getElementById('com-name').value.trim(),
            photo = document.getElementById('com-photo').value,
            text = document.getElementById('com-text').value.trim(),
            st = document.getElementById('com-status'), bt = document.getElementById('com-send');
        if (!name || !text) { st.textContent = 'Nom et commentaire obligatoires.'; return; }
        try { localStorage.setItem('charismam_name', name); } catch (e) {}
        bt.disabled = true; st.textContent = 'Envoi...';
        fetch(COMMENTS_URL, { method: 'POST', body: JSON.stringify({ species: sp, photo: photo, name: name, comment: text }) })
          .then(function(r) { return r.json(); })
          .then(function(j) {
            bt.disabled = false;
            if (j && j.ok) { st.textContent = 'Merci, commentaire enregistré.'; document.getElementById('com-text').value = ''; loadComments(sp); }
            else st.textContent = 'Erreur : ' + ((j && j.error) || 'inconnue');
          })
          .catch(function() { bt.disabled = false; st.textContent = 'Envoi impossible, réessayer plus tard.'; });
      }
      function initComments(sp) {
        try { var n = localStorage.getItem('charismam_name'); if (n) document.getElementById('com-name').value = n; } catch (e) {}
        loadComments(sp);
      }
      function hideBig() { document.getElementById('big').style.display = 'none'; }
      document.addEventListener('keydown', function(e) { if (e.key === 'Escape') hideBig(); });
      document.addEventListener('click', function(e) {
        var b = document.getElementById('big');
        if (b.style.display === 'block' && !b.contains(e.target) && e.target.tagName !== 'IMG') hideBig();
      });
    ", comments_url)))
  ),
  div(id = "big", onclick = "hideBig()",
      span(class = "close", HTML("&times;")),
      tags$img(id = "big-img", src = ""), div(id = "big-cap", class = "cap")),

  h2(class = "app-title", "CHARISMAM — visionneuse"),
  div(class = "sub", sprintf("%d espèces, %d familles, %d ordres, %d photos — cliquer une photo pour la voir en grand",
                             nrow(sp_tab), nrow(fam_tab), nrow(ord_tab), nrow(img))),
  sidebarLayout(
    sidebarPanel(width = 3,
      selectizeInput("order",   "Ordre",   choices = ord_choices()),
      selectizeInput("family",  "Famille", choices = fam_choices(ALL)),
      selectizeInput("species", "Espèce",  choices = sp_choices(ALL, ALL),
                     options = list(placeholder = "Tapez un nom...")),
      radioButtons("mode", "Afficher",
                   choices = c("L'espèce choisie" = "species",
                               "Toute la famille" = "family",
                               "Tout l'ordre"     = "order")),
      uiOutput("sp_list")
    ),
    mainPanel(width = 9, uiOutput("gallery"), uiOutput("comments"))
  )
)

server <- function(input, output, session) {

  set_sel <- function(id, choices, selected) {
    updateSelectizeInput(session, id, choices = choices, selected = selected)
  }

  # Ordre -> restreint familles et espèces
  observeEvent(input$order, {
    fam <- if (input$family %in% fam_choices(input$order)) input$family else ALL
    sp  <- if (input$species %in% sp_choices(input$order, fam)) input$species else ALL
    set_sel("family",  fam_choices(input$order), fam)
    set_sel("species", sp_choices(input$order, fam), sp)
  }, ignoreInit = TRUE)

  # Famille -> cale l'ordre, restreint les espèces
  observeEvent(input$family, {
    ord <- if (input$family == ALL) input$order else fam_tab$order[fam_tab$family == input$family][1]
    if (!identical(ord, input$order)) {
      updateSelectizeInput(session, "order", selected = ord)
      set_sel("family", fam_choices(ord), input$family)
    }
    sp <- if (input$species %in% sp_choices(ord, input$family)) input$species else ALL
    set_sel("species", sp_choices(ord, input$family), sp)
  }, ignoreInit = TRUE)

  # Espèce -> cale famille et ordre
  observeEvent(input$species, {
    if (input$species == ALL) return()
    r <- sp_tab[sp_tab$species == input$species, ][1, ]
    if (!identical(r$order, input$order))   updateSelectizeInput(session, "order", selected = r$order)
    if (!identical(r$family, input$family)) set_sel("family", fam_choices(r$order), r$family)
  })

  # Clic sur un nom dans la liste -> affiche l'espèce
  observeEvent(input$go_sp, {
    r <- sp_tab[sp_tab$species == input$go_sp, ][1, ]
    updateSelectizeInput(session, "order", selected = r$order)
    set_sel("family",  fam_choices(r$order), r$family)
    set_sel("species", sp_choices(r$order, r$family), r$species)
    updateRadioButtons(session, "mode", selected = "species")
  })

  # Liste cliquable des espèces de la famille
  output$sp_list <- renderUI({
    if (input$family == ALL) return(NULL)
    sp <- sp_tab$species[sp_tab$family == input$family]
    tagList(tags$label(sprintf("Espèces de la famille (%d)", length(sp))),
            div(class = "sp-list", lapply(sp, function(s)
              tags$a(href = "#", class = if (identical(s, input$species)) "current" else NULL,
                     onclick = sprintf("Shiny.setInputValue('go_sp', '%s', {priority: 'event'}); return false;", s),
                     tags$em(s)))))
  })

  # Commentaires de l'espèce affichée (lus / envoyés par le navigateur vers Google Sheets)
  output$comments <- renderUI({
    if (comments_url == "" || input$mode != "species" || input$species == ALL) return(NULL)
    sp <- input$species
    ph <- img[img$species == sp, ]; ph <- ph[order(ph$n, ph$type), ]
    div(class = "com",
      h4("Commentaires sur ", tags$em(sp)),
      div(id = "com-list"),
      div(class = "com-form",
        div(class = "row2",
          tags$input(id = "com-name", type = "text", class = "form-control", placeholder = "Votre nom", maxlength = 80),
          tags$select(id = "com-photo", class = "form-control",
                      tags$option(value = "", "L'espèce en général"),
                      lapply(ph$lab, function(l) tags$option(value = l, paste("Photo", l))))),
        tags$textarea(id = "com-text", class = "form-control", style = "margin-top:8px",
                      placeholder = "Votre commentaire", maxlength = 2000),
        div(style = "margin-top:8px",
          tags$button(id = "com-send", type = "button", class = "btn btn-primary",
                      onclick = sprintf("sendComment('%s')", sp), "Envoyer"),
          tags$span(id = "com-status"))),
      tags$script(HTML(sprintf("setTimeout(function(){ initComments('%s'); }, 0);", sp))))
  })

  output$gallery <- renderUI({
    switch(input$mode,

      species = {
        if (input$species == ALL)
          return(div(class = "hint", "Cherchez une espèce par son nom, ou choisissez un ordre / une famille puis une espèce."))
        d <- img[img$species == input$species, ]
        d <- d[order(d$n, d$type), ]                 # all puis head, photo 1 puis 2
        tagList(div(class = "head", h3(tags$em(input$species)),
                    span(class = "tax", sprintf("%s — %s", d$family[1], d$order[1]))),
                gallery(d, big = TRUE))
      },

      family = {
        if (input$family == ALL)
          return(div(class = "hint", "Choisissez une famille pour voir toutes ses photos."))
        d  <- img[img$family == input$family, ]
        tagList(div(class = "head", h3(input$family),
                    span(class = "tax", sprintf("%s — %d espèces, %d photos", d$order[1],
                                                length(unique(d$species)), nrow(d)))),
                gallery(d))
      },

      order = {
        if (input$order == ALL)
          return(div(class = "hint", "Choisissez un ordre pour voir toutes ses photos."))
        d <- img[img$order == input$order, ]
        tagList(div(class = "head", h3(input$order),
                    span(class = "tax", sprintf("%d familles, %d espèces, %d photos",
                                                length(unique(d$family)), length(unique(d$species)), nrow(d)))),
                lapply(sort(unique(d$family)), function(f)
                  tagList(div(class = "grp", f), gallery(d[d$family == f, ]))))
      })
  })
}

shinyApp(ui, server)
