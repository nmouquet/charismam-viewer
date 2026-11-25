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

img <- read.csv("images.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = character())
for (v in c("name_en", "name_fr")) if (!v %in% names(img)) img[[v]] <- ""
# nom commun affiché : "Moose · Élan" (vide si aucun nom connu)
img$common <- mapply(function(a, b) paste(c(a, b)[nzchar(c(a, b))], collapse = " \u00b7 "), img$name_en, img$name_fr)
img$url <- paste0(base_url, img$file)
multi   <- ave(img$n, img$species, img$type, FUN = max) > 1   # plusieurs photos du même type
img$lab <- ifelse(multi, paste(img$type, img$n), img$type)

sp_tab  <- unique(img[, c("species", "family", "order", "common", "name_en")])
sp_tab  <- sp_tab[order(sp_tab$species), ]
fam_tab <- aggregate(species ~ family + order, data = sp_tab, FUN = length)
fam_tab <- fam_tab[order(fam_tab$family), ]
ord_tab <- aggregate(species ~ order, data = sp_tab, FUN = length)
ord_tab <- ord_tab[order(ord_tab$order), ]

ALL <- ""   # valeur "tous / toutes" dans les menus

ord_choices <- function() c("All orders" = ALL,
  setNames(ord_tab$order, sprintf("%s (%d)", ord_tab$order, ord_tab$species)))
fam_choices <- function(ord) {
  f <- if (ord == ALL) fam_tab else fam_tab[fam_tab$order == ord, ]
  c("All families" = ALL, setNames(f$family, sprintf("%s (%d)", f$family, f$species)))
}
sp_choices <- function(ord, fam) {
  s <- sp_tab
  if (ord != ALL) s <- s[s$order == ord, ]
  if (fam != ALL) s <- s[s$family == fam, ]
  c("Latin or common name..." = ALL,
    setNames(s$species, ifelse(nzchar(s$common), paste0(s$species, " \u2014 ", s$common), s$species)))
}

# Vignette : image (clic = cadre agrandi + commentaires) + légende (espèce en italique, type de photo)
card <- function(r) {
  tags$figure(class = "card",
    tags$img(src = r$url, loading = "lazy", alt = r$species, title = "Enlarge / comment",
             `data-sp` = r$species, `data-lab` = r$lab, `data-common` = r$common,
             onclick = "showBigEl(this)"),
    tags$figcaption(div(class = "l1", tags$em(r$species), tags$span(class = "type", r$lab)),
                    if (nzchar(r$common)) div(class = "common", r$common)))
}
gallery <- function(d, big = FALSE)
  div(class = if (big) "gallery big" else "gallery", lapply(seq_len(nrow(d)), function(i) card(d[i, ])))

ui <- fluidPage(
  title = "CHARISMAM — image viewer",
  tags$head(
    tags$style(HTML("
      body { background: #fafafa; }
      h2.app-title { margin: 18px 0 4px; font-weight: 600; }
      .sub { color: #777; margin-bottom: 14px; }
      .gallery { display: grid; grid-template-columns: repeat(auto-fill, minmax(200px, 1fr)); gap: 14px; }
      .gallery.big { grid-template-columns: repeat(auto-fill, minmax(230px, 1fr)); }
      .card { margin: 0; background: #fff; border: 1px solid #e3e3e3; border-radius: 6px; overflow: hidden; }
      .card img { width: 100%; aspect-ratio: 1 / 1; object-fit: cover; display: block; background: #eee; cursor: zoom-in; }
      .card figcaption { padding: 6px 10px; font-size: 14px; }
      .card figcaption .l1 { display: flex; justify-content: space-between; gap: 8px; }
      .common { color: #666; font-size: 12.5px; }
      .head .common { font-size: 15px; color: #555; margin: 2px 0; }
      #big .cap .common { font-size: 14px; }
      .sp-list .common { font-size: 11.5px; color: #888; margin-left: 4px; }
      .card .type { color: #888; font-size: 12px; white-space: nowrap; }
      .head { margin: 4px 0 14px; }
      .head h3 { margin: 0; }
      .head .tax { color: #777; }
      .grp { margin: 22px 0 8px; font-size: 17px; border-bottom: 1px solid #ddd; padding-bottom: 3px; }
      .sp-list { max-height: 40vh; overflow-y: auto; }
      .sp-list a { display: block; padding: 2px 0; }
      .sp-list a.current { font-weight: 700; }
      .hint { color: #888; margin-top: 40px; text-align: center; font-size: 16px; }
      #big { display: none; position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%);
             z-index: 9999; background: #fff; border: 1px solid #ccc; border-radius: 8px;
             box-shadow: 0 6px 30px rgba(0,0,0,.25); padding: 30px 14px 12px;
             width: max-content; max-width: 97vw; max-height: 95vh; overflow: auto; }
      #big .close { position: absolute; top: 4px; right: 12px; color: #666; font-size: 26px; line-height: 1; cursor: pointer; }
      .big-wrap { display: flex; gap: 16px; flex-wrap: wrap; align-items: flex-start; }
      #big img { display: block; width: min(680px, 92vw, 80vh); height: auto; }
      #big .cap { margin-top: 6px; font-size: 15px; text-align: center; }
      .big-side { width: 330px; max-width: 92vw; display: flex; flex-direction: column; max-height: min(700px, 82vh); }
      .big-side h5 { margin: 0 0 8px; font-weight: 600; }
      #com-list { overflow-y: auto; flex: 1; min-height: 60px; }
      .com-item { background: #f7f7f7; border: 1px solid #e6e6e6; border-radius: 6px; padding: 6px 10px; margin-bottom: 6px; font-size: 14px; }
      .com-item.this { border-color: #9ec1e6; background: #f1f7fd; }
      .com-meta { color: #888; font-size: 12px; margin-bottom: 2px; }
      .com-text { white-space: pre-wrap; }
      .com-form { border-top: 1px solid #e6e6e6; padding-top: 8px; margin-top: 6px; }
      .com-form input, .com-form textarea { width: 100%; margin-bottom: 6px; }
      .com-form textarea { min-height: 70px; }
      #com-status { margin-left: 8px; color: #888; font-size: 13px; }
    ")),
    tags$script(HTML(sprintf("
      var COMMENTS_URL = '%s', CUR = { sp: '', lab: '' };
      function esc(x) { var d = document.createElement('div'); d.textContent = x == null ? '' : String(x); return d.innerHTML; }
      function showBigEl(el) { showBig(el.src, el.dataset.sp, el.dataset.lab, el.dataset.common); }
      function showBig(src, sp, lab, common) {
        CUR = { sp: sp, lab: lab };
        document.getElementById('big-img').src = src;
        document.getElementById('big-cap').innerHTML = '<i>' + esc(sp) + '</i> &mdash; ' + esc(lab) +
          (common ? '<div class=\"common\">' + esc(common) + '</div>' : '');
        document.getElementById('big-side').style.display = COMMENTS_URL ? 'flex' : 'none';
        if (COMMENTS_URL) {
          document.getElementById('com-sp').innerHTML = 'Comments on <i>' + esc(sp) + '</i>';
          document.getElementById('com-status').textContent = '';
          try { var n = localStorage.getItem('charismam_name'); if (n) document.getElementById('com-name').value = n; } catch (e) {}
          loadComments(sp, lab);
        }
        document.getElementById('big').style.display = 'block';
      }
      function loadComments(sp, lab) {
        var box = document.getElementById('com-list');
        box.innerHTML = '<span class=\"com-meta\">Loading comments...</span>';
        fetch(COMMENTS_URL + '?species=' + encodeURIComponent(sp))
          .then(function(r) { return r.json(); })
          .then(function(j) {
            if (CUR.sp !== sp) return;                       // une autre photo a été ouverte entre-temps
            var c = (j && j.comments) || [];
            if (!c.length) { box.innerHTML = '<span class=\"com-meta\">No comments yet on this species.</span>'; return; }
            box.innerHTML = c.map(function(x) {
              return '<div class=\"com-item' + (x.photo === lab ? ' this' : '') + '\"><div class=\"com-meta\"><b>' + esc(x.name) +
                     '</b> &middot; ' + esc(x.date) + (x.photo ? ' &middot; photo ' + esc(x.photo) : '') +
                     '</div><div class=\"com-text\">' + esc(x.comment) + '</div></div>';
            }).join('');
            box.scrollTop = box.scrollHeight;
          })
          .catch(function() { box.innerHTML = '<span class=\"com-meta\">Comments are unavailable at the moment.</span>'; });
      }
      function sendComment() {
        var sp = CUR.sp, lab = CUR.lab,
            name = document.getElementById('com-name').value.trim(),
            text = document.getElementById('com-text').value.trim(),
            st = document.getElementById('com-status'), bt = document.getElementById('com-send');
        if (!name || !text) { st.textContent = 'Name and comment are required.'; return; }
        try { localStorage.setItem('charismam_name', name); } catch (e) {}
        bt.disabled = true; st.textContent = 'Sending...';
        fetch(COMMENTS_URL, { method: 'POST', body: JSON.stringify({ species: sp, photo: lab, name: name, comment: text }) })
          .then(function(r) { return r.json(); })
          .then(function(j) {
            bt.disabled = false;
            if (j && j.ok) { st.textContent = 'Thank you, saved.'; document.getElementById('com-text').value = ''; loadComments(sp, lab); }
            else st.textContent = 'Error: ' + ((j && j.error) || 'unknown');
          })
          .catch(function() { bt.disabled = false; st.textContent = 'Could not send, please try again later.'; });
      }
      function hideBig() { document.getElementById('big').style.display = 'none'; }
      document.addEventListener('keydown', function(e) { if (e.key === 'Escape') hideBig(); });
      document.addEventListener('click', function(e) {
        var b = document.getElementById('big');
        if (b.style.display === 'block' && !b.contains(e.target) && e.target.tagName !== 'IMG') hideBig();
      });
    ", comments_url)))
  ),
  div(id = "big",
      span(class = "close", title = "Close", onclick = "hideBig()", HTML("&times;")),
      div(class = "big-wrap",
        div(tags$img(id = "big-img", src = ""), div(id = "big-cap", class = "cap")),
        div(id = "big-side", class = "big-side",
          tags$h5(id = "com-sp", "Comments"),
          div(id = "com-list"),
          div(class = "com-form",
            tags$input(id = "com-name", type = "text", class = "form-control", placeholder = "Your name", maxlength = 80),
            tags$textarea(id = "com-text", class = "form-control", placeholder = "Your comment on this photo", maxlength = 2000),
            tags$button(id = "com-send", type = "button", class = "btn btn-primary btn-sm", onclick = "sendComment()", "Send"),
            tags$span(id = "com-status"))))),

  h2(class = "app-title", "CHARISMAM — image viewer"),
  div(class = "sub", sprintf(if (comments_url != "") "%d species, %d families, %d orders, %d photos — click a photo to enlarge it and leave a comment" else "%d species, %d families, %d orders, %d photos — click a photo to enlarge it",
                             nrow(sp_tab), nrow(fam_tab), nrow(ord_tab), nrow(img))),
  sidebarLayout(
    sidebarPanel(width = 3,
      selectizeInput("order",   "Order",   choices = ord_choices()),
      selectizeInput("family",  "Family", choices = fam_choices(ALL)),
      selectizeInput("species", "Species",  choices = sp_choices(ALL, ALL),
                     options = list(placeholder = "Latin or common name...")),
      radioButtons("mode", "Show",
                   choices = c("Selected species" = "species",
                               "Whole family"     = "family",
                               "Whole order"      = "order")),
      uiOutput("sp_list")
    ),
    mainPanel(width = 9, uiOutput("gallery"))
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
    tagList(tags$label(sprintf("Species in this family (%d)", length(sp))),
            div(class = "sp-list", lapply(sp, function(s)
              tags$a(href = "#", class = if (identical(s, input$species)) "current" else NULL,
                     onclick = sprintf("Shiny.setInputValue('go_sp', '%s', {priority: 'event'}); return false;", s),
                     tags$em(s),
                     { en <- sp_tab$name_en[sp_tab$species == s][1]; if (nzchar(en)) tags$span(class = "common", en) }))))
  })

  output$gallery <- renderUI({
    switch(input$mode,

      species = {
        if (input$species == ALL)
          return(div(class = "hint", "Search a species by name, or pick an order / a family and then a species."))
        d <- img[img$species == input$species, ]
        d <- d[order(d$n, d$type), ]                 # all puis head, photo 1 puis 2
        cm <- d$common[1]
        tagList(div(class = "head", h3(tags$em(input$species)), if (nzchar(cm)) div(class = "common", cm),
                    span(class = "tax", sprintf("%s — %s", d$family[1], d$order[1]))),
                gallery(d, big = TRUE))
      },

      family = {
        if (input$family == ALL)
          return(div(class = "hint", "Pick a family to see all its photos."))
        d  <- img[img$family == input$family, ]
        tagList(div(class = "head", h3(input$family),
                    span(class = "tax", sprintf("%s — %d species, %d photos", d$order[1],
                                                length(unique(d$species)), nrow(d)))),
                gallery(d))
      },

      order = {
        if (input$order == ALL)
          return(div(class = "hint", "Pick an order to see all its photos."))
        d <- img[img$order == input$order, ]
        tagList(div(class = "head", h3(input$order),
                    span(class = "tax", sprintf("%d families, %d species, %d photos",
                                                length(unique(d$family)), length(unique(d$species)), nrow(d)))),
                lapply(sort(unique(d$family)), function(f)
                  tagList(div(class = "grp", f), gallery(d[d$family == f, ]))))
      })
  })
}

shinyApp(ui, server)
