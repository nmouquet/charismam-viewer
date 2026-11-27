# =============================================================================
# CHARISMAM — mammal image viewer (Shiny / Shinylive)
# -----------------------------------------------------------------------------
# Online:  https://nmouquet.github.io/charismam-viewer/ (GitHub Pages)
# Locally: shiny::runApp("app/viewer")
#
# WHAT THE APP DOES
#   - browse the photos (all / head) by order, family or species (menus on one
#     line above the photos); the species search always covers all species
#     (Latin or common name); what is shown follows the menus: a species if one
#     is chosen, otherwise the family, otherwise the order
#   - sort the species (alphabetical, public or research attention) and show
#     only the photos that have comments
#   - click on a photo: enlarged frame with the photo credit (author, licence,
#     source) and two tabs
#       Profile   species sheet (attention, range & discovery, IUCN, CITES,
#                 funding, zoonoses, zoos and hazard to visitors, traits), with
#                 a tooltip on each variable; every bar is a rank among all
#                 mammal species (full bar = highest value, "top x%" = share of
#                 species above)
#       Comments  visitors' comments on the photo and on the species
#     keys: left / right arrow = previous / next photo, Esc = close
#   - IUCN badge next to the name, badge with the number of comments on thumbnails
#
# FILES READ (all written by scripts/deploy_viewer.R — do not edit by hand)
#   images.csv         one row per photo online (file, species, taxonomy,
#                      English / French common names, photo credit)
#   species_info.csv   profile of each species (sources detailed in deploy_viewer.R)
#   comments_url.txt   address of the Google Apps Script for comments (optional)
# Photos are read directly from Scaleway (public bucket biodiful-bucket, folder
# charismam/pool/). Comments are read and written by the browser in a Google
# Sheet (scripts/comments_apps_script.gs).
#
# Interface in English (viewer shared with collaborators).
# =============================================================================

library(shiny)
`%||%` <- function(a, b) if (is.null(a)) b else a

base_url <- "https://biodiful-bucket.s3.fr-par.scw.cloud/charismam/pool/"

# Comments: URL of the Google Apps Script (file written by deploy_viewer.R).
# Without this file, the comments part does not appear.
comments_url <- if (file.exists("comments_url.txt")) trimws(readLines("comments_url.txt", warn = FALSE)[1]) else ""
if (is.na(comments_url)) comments_url <- ""

img <- read.csv("images.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = character())
for (v in c("name_en", "name_fr", "author", "license", "source_url")) if (!v %in% names(img)) img[[v]] <- ""
# common name shown: "Moose · Élan" (empty if no known name)
img$common <- mapply(function(a, b) {
  v <- c(a, b)[nzchar(c(a, b))]; v <- v[!duplicated(tolower(v))]   # "Jaguar · Jaguar" -> "Jaguar"
  paste(v, collapse = " \u00b7 ")
}, img$name_en, img$name_fr)
img$url <- paste0(base_url, img$file)
multi   <- ave(img$n, img$species, img$type, FUN = max) > 1   # several photos of the same type
img$lab <- ifelse(multi, paste(img$type, img$n), img$type)

# Species profile ("Profile" tab of the frame), written by deploy_viewer.R
info <- if (file.exists("species_info.csv"))
  read.csv("species_info.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = "") else NULL
info_json <- if (is.null(info)) "[]" else jsonlite::toJSON(info, dataframe = "rows", na = "null", digits = NA)
iucn_of <- function(sp) if (is.null(info)) NA else info$iucn_cat[match(sp, info$species)]
# attention of each species, for the sort menu (NA last)
att_of  <- function(sp, v) if (is.null(info) || !v %in% names(info)) rep(NA_real_, length(sp)) else info[[v]][match(sp, info$species)]
iucn_names <- c(LC = "Least Concern", NT = "Near Threatened", VU = "Vulnerable", EN = "Endangered",
                CR = "Critically Endangered", EW = "Extinct in the Wild", EX = "Extinct", DD = "Data Deficient")
iucn_badge <- function(cat) if (is.na(cat) || !nzchar(cat)) NULL else
  tags$span(class = paste0("iucn iucn-", cat), title = paste("IUCN:", iucn_names[cat]), cat)

sp_tab  <- unique(img[, c("species", "family", "order", "common", "name_en")])
sp_tab  <- sp_tab[order(sp_tab$species), ]
fam_tab <- aggregate(species ~ family + order, data = sp_tab, FUN = length)
fam_tab <- fam_tab[order(fam_tab$family), ]
ord_tab <- aggregate(species ~ order, data = sp_tab, FUN = length)
ord_tab <- ord_tab[order(ord_tab$order), ]

ALL <- ""   # "all" value in the menus

ord_choices <- function() c("All orders" = ALL,
  setNames(ord_tab$order, sprintf("%s (%d)", ord_tab$order, ord_tab$species)))
fam_choices <- function(ord) {
  f <- if (ord == ALL) fam_tab else fam_tab[fam_tab$order == ord, ]
  c("All families" = ALL, setNames(f$family, sprintf("%s (%d)", f$family, f$species)))
}
# The Species menu always offers ALL species (global search)
sp_choices <- function() {
  s <- sp_tab
  c("Latin or common name..." = ALL,
    setNames(s$species, ifelse(nzchar(s$common), paste0(s$species, " \u2014 ", s$common), s$species)))
}

# Thumbnail: image (click = enlarged frame + comments) + caption (species in italics, photo type)
card <- function(r) {
  tags$figure(class = "card",
    tags$img(src = r$url, loading = "lazy", alt = r$species, title = "Enlarge / comment",
             `data-sp` = r$species, `data-lab` = r$lab, `data-common` = r$common,
             `data-author` = r$author, `data-lic` = r$license, `data-src` = r$source_url,
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
      .card { position: relative; margin: 0; background: #fff; border: 1px solid #e3e3e3; border-radius: 6px; overflow: hidden; }
      .card img { width: 100%; aspect-ratio: 1 / 1; object-fit: cover; display: block; background: #eee; cursor: zoom-in; }
      .card figcaption { padding: 6px 10px; font-size: 14px; }
      .card figcaption .l1 { display: flex; justify-content: space-between; gap: 8px; }
      .common { color: #666; font-size: 12.5px; }
      .head .common { font-size: 15px; color: #555; margin: 2px 0; }
      #big .cap .common { font-size: 14px; }
      .card .type { color: #888; font-size: 12px; white-space: nowrap; }
      .head { margin: 4px 0 14px; }
      .head h3 { margin: 0; }
      .head .tax { color: #777; }
      .grp { margin: 22px 0 8px; font-size: 17px; border-bottom: 1px solid #ddd; padding-bottom: 3px; }
      .filters { display: flex; flex-wrap: wrap; gap: 4px 16px; align-items: flex-end; }
      .filters .form-group { margin-bottom: 4px; }
      .filters .f-sel { width: 230px; } .filters .f-sp { width: 330px; } .filters .f-sort { width: 190px; }
      .filters .f-chk { padding-bottom: 10px; }
      .filters .checkbox { margin: 0; }
      .sub { color: #777; margin: 2px 0 16px; }
      #big .credit { font-size: 12px; color: #888; text-align: center; margin-top: 3px; }
      #big .credit a { color: #888; text-decoration: underline; }
      #big .nav { position: absolute; top: 4px; left: 14px; color: #999; font-size: 12px; }
      .hint { color: #888; margin-top: 40px; text-align: center; font-size: 16px; }
      #big { display: none; position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%);
             z-index: 9999; background: #fff; border: 1px solid #ccc; border-radius: 8px;
             box-shadow: 0 6px 30px rgba(0,0,0,.25); padding: 30px 14px 12px;
             width: max-content; max-width: 97vw; max-height: 95vh; overflow: auto; }
      #big .close { position: absolute; top: 4px; right: 12px; color: #666; font-size: 26px; line-height: 1; cursor: pointer; }
      .big-wrap { display: flex; gap: 16px; flex-wrap: wrap; align-items: flex-start; }
      #big img { display: block; width: min(680px, 92vw, 80vh); height: auto; }
      #big .cap { margin-top: 6px; font-size: 15px; text-align: center; }
      .big-side { width: 380px; max-width: 92vw; display: flex; flex-direction: column; max-height: min(700px, 82vh); }
      .big-side h5 { margin: 0 0 8px; font-weight: 600; }
      #com-list { overflow-y: auto; flex: 1; min-height: 60px; }
      .com-item { background: #f7f7f7; border: 1px solid #e6e6e6; border-radius: 6px; padding: 6px 10px; margin-bottom: 6px; font-size: 14px; }
      .iucn { display: inline-block; font-size: 11px; font-weight: 700; padding: 1px 6px; border-radius: 4px;
              color: #fff; vertical-align: middle; font-style: normal; letter-spacing: .3px; }
      .iucn-LC { background: #60c659; } .iucn-NT { background: #a8c63a; } .iucn-VU { background: #e8c400; color: #222; }
      .iucn-EN { background: #fc7f3f; } .iucn-CR { background: #d81e05; } .iucn-EW { background: #542344; }
      .iucn-EX { background: #000; } .iucn-DD { background: #b5b5ab; }
      .haz { display: inline-block; font-size: 11px; font-weight: 700; padding: 1px 6px; border-radius: 4px; color: #fff; }
      .haz-1A { background: #8b0000; } .haz-1 { background: #d9480f; } .haz-2 { background: #e8a200; } .haz-3 { background: #9aa39a; }
      .head h3 .iucn { margin-left: 8px; font-size: 13px; }
      .tabs { display: flex; gap: 4px; border-bottom: 1px solid #ddd; margin-bottom: 8px; }
      .tab { border: 1px solid transparent; border-bottom: none; background: none; padding: 5px 12px; font-size: 13.5px;
             color: #666; border-radius: 5px 5px 0 0; margin-bottom: -1px; }
      .tab.on { border-color: #ddd; background: #fff; color: #222; font-weight: 600; }
      .tabc { overflow-y: auto; min-height: 0; flex: 1; }
      #tab-comments { flex-direction: column; }
      .pf-h { font-size: 12px; font-weight: 700; color: #555; text-transform: uppercase; letter-spacing: .4px; margin: 10px 0 3px; }
      .pf-h:first-child { margin-top: 2px; }
      table.pf { width: 100%; font-size: 13.5px; border-collapse: collapse; }
      table.pf th { font-weight: 400; color: #777; width: 34%; vertical-align: top; padding: 2px 6px 2px 0; }
      table.pf td { padding: 2px 0; vertical-align: top; }
      .na { color: #aaa; }
      .tip { border-bottom: 1px dotted #aaa; cursor: help; }
      .muted { color: #888; font-size: 12px; }
      .bar { display: inline-block; width: 60px; height: 7px; background: #e6e6e6; border-radius: 4px; vertical-align: middle; }
      .bar span { display: block; height: 100%; background: #4a8cc9; border-radius: 4px; }
      .badge-com { position: absolute; top: 6px; right: 6px; background: rgba(255,255,255,.92); border-radius: 10px;
                   padding: 1px 7px; font-size: 12px; box-shadow: 0 1px 3px rgba(0,0,0,.25); pointer-events: none; }
      .com-h { font-size: 12.5px; font-weight: 600; color: #555; margin: 4px 0 4px; }
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
      var CUR_EL = null;
      function showBigEl(el) { CUR_EL = el; showBig(el.src, el.dataset.sp, el.dataset.lab, el.dataset.common, el.dataset); }
      // visible thumbnails, in page order (for the arrow keys)
      function cards() { return Array.prototype.filter.call(document.querySelectorAll('.card img[data-sp]'), function(x) { return x.offsetParent !== null; }); }
      function stepBig(d) {
        var c = cards(), i = c.indexOf(CUR_EL);
        if (i < 0 || !c.length) return;
        showBigEl(c[(i + d + c.length) %% c.length]);
      }
      function creditHTML(ds) {
        if (!ds) return '';
        var a = ds.author || '', l = ds.lic || '', u = ds.src || '';
        if (!a && !l && !u) return '';
        var parts = [];
        if (a) parts.push((l && l !== 'Public domain' ? '' : '&copy; ') + esc(a));
        if (l) parts.push(esc(l));
        if (u) parts.push('<a href=\"' + esc(u) + '\" target=\"_blank\" rel=\"noopener\">source</a>');
        return 'Photo: ' + parts.join(' &middot; ');
      }
      function showBig(src, sp, lab, common, ds) {
        CUR = { sp: sp, lab: lab };
        document.getElementById('big-img').src = src;
        var ic = (window.INFO && INFO[sp] && INFO[sp].iucn_cat) ? ' ' + iucnBadge(INFO[sp].iucn_cat) : '';
        document.getElementById('big-cap').innerHTML = '<i>' + esc(sp) + '</i>' + ic + ' &mdash; ' + esc(lab) +
          (common ? '<div class=\"common\">' + esc(common) + '</div>' : '');
        document.getElementById('big-credit').innerHTML = creditHTML(ds);
        var n = cards().length;
        document.getElementById('big-nav').innerHTML = n > 1 ? '&larr; &rarr; previous / next photo' : '';
        document.getElementById('big-side').style.display = 'flex';
        document.getElementById('tb-comments').style.display = COMMENTS_URL ? '' : 'none';
        if (window.profileHTML) {
          var r = INFO[sp];
          document.getElementById('tab-profile').innerHTML = profileHTML(sp);
        }
        document.getElementById('tb-comments').textContent = 'Comments';
        showTab('profile');
        if (COMMENTS_URL) {
          document.getElementById('com-sp').innerHTML = 'Comments on <i>' + esc(sp) + '</i>';
          document.getElementById('com-status').textContent = '';
          try { var n = localStorage.getItem('charismam_name'); if (n) document.getElementById('com-name').value = n; } catch (e) {}
          loadComments(sp, lab);
        }
        document.getElementById('big').style.display = 'block';
      }
      function comItem(x, showPhoto) {
        return '<div class=\"com-item\"><div class=\"com-meta\"><b>' + esc(x.name) + '</b> &middot; ' + esc(x.date) +
               (showPhoto && x.photo ? ' &middot; photo ' + esc(x.photo) : '') +
               '</div><div class=\"com-text\">' + esc(x.comment) + '</div></div>';
      }
      function loadComments(sp, lab) {
        var box = document.getElementById('com-list');
        box.innerHTML = '<span class=\"com-meta\">Loading comments...</span>';
        fetch(COMMENTS_URL + '?species=' + encodeURIComponent(sp))
          .then(function(r) { return r.json(); })
          .then(function(j) {
            if (CUR.sp !== sp) return;                       // another photo was opened in the meantime
            var c = (j && j.comments) || [];
            document.getElementById('tb-comments').textContent = 'Comments (' + c.length + ')';
            var mine = c.filter(function(x) { return x.photo === lab; }),
                oth  = c.filter(function(x) { return x.photo !== lab; });
            var h = '<div class=\"com-h\">On this photo (' + mine.length + ')</div>' +
                    (mine.length ? mine.map(function(x) { return comItem(x, false); }).join('')
                                 : '<div class=\"com-meta\">No comment on this photo yet.</div>');
            if (oth.length) h += '<div class=\"com-h\" style=\"margin-top:10px\">On other photos of this species (' + oth.length + ')</div>' +
                                 oth.map(function(x) { return comItem(x, true); }).join('');
            box.innerHTML = h;
          })
          .catch(function() { box.innerHTML = '<span class=\"com-meta\">Comments are unavailable at the moment.</span>'; });
      }
      // \"Number of comments\" badges on the thumbnails
      var ALLC = [];
      function loadAllComments() {
        if (!COMMENTS_URL) return;
        fetch(COMMENTS_URL).then(function(r) { return r.json(); })
          .then(function(j) { ALLC = (j && j.comments) || []; applyBadges(); pushKeys(); }).catch(function() {});
      }
      // photos with comments -> Shiny (filter 'Only photos with comments')
      function pushKeys() {
        if (!(window.Shiny && Shiny.setInputValue)) return;
        var k = {}; ALLC.forEach(function(x) { k[x.species + '|' + x.photo] = 1; });
        Shiny.setInputValue('commented', Object.keys(k).concat(['__loaded__']));
      }
      $(document).on('shiny:connected', function() { if (ALLC.length) pushKeys(); });
      function applyBadges() {
        var cnt = {};
        ALLC.forEach(function(x) { var k = x.species + '|' + x.photo; cnt[k] = (cnt[k] || 0) + 1; });
        document.querySelectorAll('.card img[data-sp]').forEach(function(im) {
          var fig = im.parentNode, bd = fig.querySelector('.badge-com'), n = cnt[im.dataset.sp + '|' + im.dataset.lab] || 0;
          if (n) {
            if (!bd) { bd = document.createElement('span'); bd.className = 'badge-com'; fig.appendChild(bd); }
            bd.innerHTML = '&#128172; ' + n;
          } else if (bd) bd.remove();
        });
      }
      $(document).on('shiny:value', function(e) { if (e.name === 'gallery') setTimeout(applyBadges, 50); });
      $(function() { loadAllComments(); });
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
            if (j && j.ok) { st.textContent = 'Thank you, saved.'; document.getElementById('com-text').value = ''; loadComments(sp, lab); loadAllComments(); }
            else st.textContent = 'Error: ' + ((j && j.error) || 'unknown');
          })
          .catch(function() { bt.disabled = false; st.textContent = 'Could not send, please try again later.'; });
      }
      function hideBig() { document.getElementById('big').style.display = 'none'; }
      document.addEventListener('keydown', function(e) {
        if (document.getElementById('big').style.display !== 'block') return;
        var t = (e.target && e.target.tagName) || '';
        if (t === 'INPUT' || t === 'TEXTAREA') return;           // typing a comment
        if (e.key === 'Escape') hideBig();
        else if (e.key === 'ArrowRight') { e.preventDefault(); stepBig(1); }
        else if (e.key === 'ArrowLeft')  { e.preventDefault(); stepBig(-1); }
      });
      document.addEventListener('click', function(e) {
        var b = document.getElementById('big');
        if (b.style.display === 'block' && !b.contains(e.target) && e.target.tagName !== 'IMG') hideBig();
      });
    ", comments_url)))
  ),
  tags$script(HTML(paste0("var INFO_ARR = ", info_json, ";"))),
  tags$script(HTML(r"---(
      var INFO = {};
      INFO_ARR.forEach(function(r) { INFO[r.species] = r; });
      var IUCN_NAMES = { LC: 'Least Concern', NT: 'Near Threatened', VU: 'Vulnerable', EN: 'Endangered',
                         CR: 'Critically Endangered', EW: 'Extinct in the Wild', EX: 'Extinct', DD: 'Data Deficient' };
      function isNA(x) { return x === null || x === undefined || x === ''; }
      function NA_() { return '<span class="na">NA</span>'; }
      function txt(x) { return isNA(x) ? NA_() : esc(x); }
      function iucnBadge(c) { return '<span class="iucn iucn-' + esc(c) + '" title="IUCN: ' + esc(IUCN_NAMES[c] || c) + '">' + esc(c) + '</span>'; }
      function money(x) {
        x = +x;
        if (x >= 1e9) return (x / 1e9).toFixed(1) + ' B';
        if (x >= 1e6) return (x / 1e6).toFixed(1) + ' M';
        if (x >= 1e3) return Math.round(x / 1e3) + ' k';
        return Math.round(x).toString();
      }
      function mass(g) {
        if (isNA(g)) return NA_();
        g = +g;
        if (g >= 1000) return (g >= 100000 ? Math.round(g / 1000) : (g / 1000).toFixed(1)) + ' kg';
        return (g >= 10 ? Math.round(g) : g.toFixed(1)) + ' g';
      }
      // Every bar = rank among all mammal species: bar length = share of species with a
      // lower value (p, 0-100); "top x%" = share of species above. Full bar = highest.
      function rankBar(p, txt, word) {
        if (isNA(p)) return isNA(txt) ? NA_() : txt;
        p = Math.min(100, Math.max(0, +p));
        var top = Math.max(1, Math.round(100 - p));
        return '<span class="bar" title="' + (word || 'Higher than') + ' ' + Math.round(p) + '% of mammal species"><span style="width:' +
               Math.max(2, p) + '%"></span></span> ' + (isNA(txt) ? '' : txt + ' ') +
               '<span class="muted" title="' + (word || 'Higher than') + ' ' + Math.round(p) + '% of mammal species">' +
               (isNA(txt) ? '' : '&middot; ') + 'top ' + top + '%</span>';
      }
      function score(x, p) { return isNA(x) ? NA_() : rankBar(p, (+x).toFixed(2)); }
      // Variable tooltips (hover over the label)
      var TIPS = {
        'Academic': 'Scientific attention: composite index (0-1) built from the scientific literature, citations, research fields and genetic sequences (NCBI). Bar = rank among all mammal species (full = highest); top x% = share of species with a higher index.',
        'Public': 'Public attention: composite index (0-1) built from Wikipedia page views, the number of common names and Flickr photos. Bar = rank among all mammal species (full = highest); top x% = share of species with a higher index.',
        'iNaturalist': 'Number of research-grade iNaturalist observations (identification confirmed by the community), export of February 2025 (Jensen et al. 2026, J. Mammal.). Bar = rank among all mammal species (full = most observed); top x% = share of species with more observations.',
        'Range': 'Geographic range size (number of occupied grid cells). Bar = rank among all mammal species (full = widest range); top x% = share of species with a wider range.',
        'Category': 'IUCN Red List category of the latest global assessment (year in brackets).',
        'Population trend': 'Population trend reported in the IUCN Red List assessment.',
        'Threats': 'Number of threats coded in the IUCN assessment, followed by the broad threat groups (IUCN Threats Classification, level 1).',
        'Uses': 'Uses and trade documented in the IUCN assessment (IUCN Use and Trade Classification).',
        'Appendix': 'Strictest CITES Appendix found in the trade records (I = most restricted). Inferred from trade, not from the official listing.',
        'Shipments': 'Number of shipment records in the CITES Trade Database for this species (first and last year in brackets).',
        'Countries': 'Number of distinct importing and exporting countries in the CITES trade records.',
        'Wild / captive': 'Shipments of wild-sourced specimens (source W) versus captive-bred or born in captivity (sources C, D, F).',
        'Main purpose': 'Most frequent declared purpose of the CITES shipments.',
        'Main source': 'Most frequent declared source of the CITES specimens.',
        'Main item': 'Most frequent type of item traded (live animals, skins, trophies, specimens...).',
        'Gu&eacute;nard et al. 2025': 'Conservation projects funding this species and total amount (US$), from Gu\u00e9nard et al. 2025.',
        'Pathogens': 'Host-pathogen associations in CLOVER 1.0. A pathogen is zoonotic when it is also recorded in humans. Counts depend on research effort.',
        'By type': 'Number of distinct pathogens by group (CLOVER 1.0).',
        'Visitor hazard': 'Hazard to zoo visitors (UK Standards of Modern Zoo Practice, 2025, Appendix B): 1A highest, 1 greater, 2 less, 3 least risk. Listed mostly by genus: a species inherits the category of its genus; unlisted taxa are category 3.',
        'Held in zoos (ZIMS)': 'Whether the species is recorded in zoos or aquariums of the ZIMS network (Species360), via DSKI (Conde et al. 2019).',
        'Demographic knowledge': 'Demographic Species Knowledge Index (Conde et al. 2019): how much is known on mortality and fertility (None / Low / Fair / High).',
        'Described': 'Authority and year of the original description (Mammal Diversity Database v2.5). Parentheses mean the species was first described in another genus. Bar = how early the species was described among all mammal species (full = among the first, 1758); top x% = share of species described earlier.',
        'Body mass': 'Adult body mass (COMBINE, otherwise EltonTraits).',
        'Max longevity': 'Maximum recorded age at death (COMBINE).',
        'Litter size': 'Mean number of young per litter (COMBINE).',
        'Litters per year': 'Mean number of litters per year (COMBINE).',
        'Gestation': 'Mean gestation length (COMBINE).',
        'Generation length': 'Average age of parents of the current cohort, as used by the IUCN (COMBINE).',
        'Realm': 'Biogeographical realm(s) occupied (COMBINE).',
        'Habitats': 'Number of IUCN habitat types used (habitat breadth, COMBINE).',
        'Home range': 'Mean home range size of individuals or groups (COMBINE).',
        'Density': 'Mean population density (COMBINE).',
        'Group size': 'Mean social group size (COMBINE).',
        'Hibernation / torpor': 'Whether the species hibernates or uses daily torpor (COMBINE).',
        'Diet': 'Main diet class (EltonTraits).',
        'Foraging stratum': 'Main foraging stratum (EltonTraits).',
        'Activity': 'Daily activity pattern (EltonTraits).'
      };
      function row(k, v) {
        var t = TIPS[k];
        var lab = t ? '<span class="tip" title="' + t.replace(/"/g, '&quot;') + '">' + k + '</span>' : k;
        return '<tr><th>' + lab + '</th><td>' + v + '</td></tr>';
      }
      function num(x, d) { return isNA(x) ? NA_() : (+x).toFixed(d).replace(/\.0$/, ''); }
      function sig(x) { x = +x; return x >= 100 ? Math.round(x).toLocaleString('en-US') : +x.toPrecision(3) + ''; }
      function yrs(d) { return isNA(d) ? NA_() : (+d / 365.25).toFixed(+d >= 3652 ? 0 : 1).replace(/\.0$/, '') + ' years'; }
      function len(mm) {
        if (isNA(mm)) return NA_();
        mm = +mm;
        return mm >= 1000 ? (mm / 1000).toFixed(2).replace(/0$/, '') + ' m' : (mm >= 100 ? Math.round(mm / 10) : (mm / 10).toFixed(1)) + ' cm';
      }
      var HAZ = { '1A': 'Highest risk', '1': 'Greater risk', '2': 'Less risk', '3': 'Least risk' };
      var HAZ_FLAG = { K: 'kicking', V: 'venom', E: 'electric', P: 'pecking' };
      function hazardHTML(r) {
        if (isNA(r.hazard)) return NA_();
        var c = String(r.hazard);
        var fl = isNA(r.hazard_flags) ? '' : String(r.hazard_flags).split('').map(function(f) { return HAZ_FLAG[f] || f; }).join(', ');
        return '<span class="haz haz-' + c + '" title="UK Standards of Modern Zoo Practice (2025), Appendix B">Cat. ' + c + '</span> ' +
               HAZ[c] + (fl ? ' <span class="muted">(special risk: ' + fl + ')</span>' : '') +
               (isNA(r.hazard_cond) ? '' : '<br><span class="muted">' + esc(r.hazard_cond) + '</span>');
      }
      var CITES_PURPOSE = { T: 'Commercial', Z: 'Zoo', S: 'Scientific', P: 'Personal', H: 'Hunting trophy',
                            Q: 'Circus / travelling exhibition', M: 'Medical', E: 'Educational', B: 'Captive breeding',
                            N: 'Reintroduction', L: 'Law enforcement', G: 'Botanical garden' };
      var CITES_SOURCE  = { W: 'Wild', C: 'Captive-bred', F: 'Born in captivity (F1)', R: 'Ranched',
                            D: 'Captive-bred (App. I, registered)', I: 'Confiscated / seized', O: 'Pre-Convention',
                            U: 'Unknown', X: 'Marine (high seas)', A: 'Artificially propagated' };
      function nf(x) { return (+x).toLocaleString('en-US'); }
      function zooRows(r) {
        if (isNA(r.zoo_status)) return row('Status', NA_());
        if (r.zoo_status === 'not_sampled') return row('Status', 'No host&ndash;pathogen record');
        var pl = function(n, w) { return nf(n) + ' ' + w + (+n === 1 ? '' : 's'); };
        return row('Pathogens', pl(r.zoo_n_path, 'pathogen') + ' known &middot; <b>' + nf(r.zoo_n_zoon) + ' zoonotic</b>' +
                               (isNA(r.zoo_n_refs) ? '' : ' <span class="muted">(' + pl(r.zoo_n_refs, 'reference') + ')</span>')) +
               row('By type', pl(r.zoo_virus, 'virus').replace('viruss', 'viruses') + ' &middot; ' + nf(r.zoo_bacteria) + ' bacteria &middot; ' +
                              pl(r.zoo_helminth, 'helminth') + ' &middot; ' + nf(r.zoo_protozoa) + ' protozoa &middot; ' + nf(r.zoo_fungi) + ' fungi');
      }
      function citesRows(r) {
        if (isNA(r.cites_in_db)) return row('Status', NA_());
        if (!r.cites_in_db || +r.cites_n_ship === 0) return row('Status', 'Not listed / no trade recorded');
        var yrs = isNA(r.cites_first) ? '' : ' <span class="muted">(' + r.cites_first + (r.cites_last && r.cites_last !== r.cites_first ? '&ndash;' + r.cites_last : '') + ')</span>';
        var dec = function(d, c) { return isNA(c) ? NA_() : esc(d[c] || c); };
        return row('Appendix', isNA(r.cites_app) ? NA_() : 'Appendix ' + esc(r.cites_app)) +
               row('Shipments', nf(r.cites_n_ship) + yrs) +
               row('Countries', isNA(r.cites_n_imp) ? NA_() : nf(r.cites_n_imp) + ' importers &middot; ' + nf(r.cites_n_exp) + ' exporters') +
               row('Wild / captive', isNA(r.cites_wild) ? NA_() : nf(r.cites_wild) + ' wild &middot; ' + nf(r.cites_captive) + ' captive') +
               row('Main purpose', dec(CITES_PURPOSE, r.cites_purpose)) +
               row('Main source', dec(CITES_SOURCE, r.cites_source)) +
               row('Main item', txt(r.cites_term));
      }
      function profileHTML(sp) {
        var r = INFO[sp];
        if (!r) return '<div class="com-meta">No data available for this species.</div>';
        var h = '<div class="pf-h">Attention, range &amp; discovery</div><table class="pf">' +
          row('Academic', score(r.acad, r.acad_pct)) + row('Public', score(r.public, r.public_pct)) +
          row('Range', isNA(r.range_pct) ? NA_() : rankBar(r.range_pct, null)) +
          row('iNaturalist', isNA(r.inat_n_rg) ? NA_() : rankBar(r.inat_pct, nf(r.inat_n_rg) + ' obs.')) +
          row('Described', isNA(r.desc_year) ? NA_() :
              rankBar(r.desc_pct, esc(isNA(r.desc_author) ? String(r.desc_year) : r.desc_author), 'Described earlier than')) + '</table>';
        h += '<div class="pf-h">IUCN Red List</div><table class="pf">' +
          row('Category', isNA(r.iucn_cat) ? NA_() : iucnBadge(r.iucn_cat) + ' ' + esc(IUCN_NAMES[r.iucn_cat] || '') +
              (isNA(r.iucn_year) ? '' : ' <span class="muted">(' + r.iucn_year + ')</span>')) +
          row('Population trend', txt(r.pop_trend)) +
          row('Threats', isNA(r.threat_n) ? NA_() : r.threat_n + (r.threats ? ' <span class="muted">&mdash; ' + esc(r.threats) + '</span>' : '')) +
          '</table>';
        h += '<div class="pf-h">Use &amp; trade (IUCN)</div><table class="pf">' +
          row('Uses', isNA(r.ut_n) ? NA_() : (+r.ut_n === 0 ? 'None recorded' :
              r.ut_n + (r.uses ? ' <span class="muted">&mdash; ' + esc(r.uses) + '</span>' : ''))) + '</table>';
        h += '<div class="pf-h">CITES trade</div><table class="pf">' + citesRows(r) + '</table>';
        h += '<div class="pf-h">Conservation funding</div><table class="pf">' +
          row('Gu&eacute;nard et al. 2025', isNA(r.guenard_projects) ? NA_() :
              r.guenard_projects + ' project' + (+r.guenard_projects > 1 ? 's' : '') +
              (isNA(r.guenard_usd) ? '' : ' &middot; US$ ' + money(r.guenard_usd))) + '</table>';
        h += '<div class="pf-h">Zoonoses (CLOVER 1.0)</div><table class="pf">' + zooRows(r) + '</table>';
        h += '<div class="pf-h">Zoos (ZIMS / DSKI)</div><table class="pf">' +
          row('Visitor hazard', hazardHTML(r)) +
          row('Held in zoos (ZIMS)', isNA(r.zims) ? NA_() : (r.zims ? 'Yes' : 'No')) +
          row('Demographic knowledge', isNA(r.dski_mort) && isNA(r.dski_fert) ? NA_() :
              'mortality ' + txt(r.dski_mort) + ' &middot; fertility ' + txt(r.dski_fert)) + '</table>';
        h += '<div class="pf-h">Body</div><table class="pf">' +
          row('Body mass', mass(r.bodymass_g)) + row('<span title="Tip of the nose to the base of the tail (tail excluded), COMBINE">Head&ndash;body length</span>', len(r.length_mm)) + '</table>';
        h += '<div class="pf-h">Life history</div><table class="pf">' +
          row('Max longevity', yrs(r.longevity_d)) +
          row('Litter size', num(r.litter_size, 1)) +
          row('Litters per year', num(r.litters_yr, 1)) +
          row('Gestation', isNA(r.gestation_d) ? NA_() : Math.round(+r.gestation_d) + ' days') +
          row('Generation length', yrs(r.generation_d)) + '</table>';
        h += '<div class="pf-h">Ecology</div><table class="pf">' +
          row('Realm', txt(r.realm)) +
          row('Habitats', isNA(r.n_habitats) ? NA_() : r.n_habitats + ' habitat type' + (+r.n_habitats > 1 ? 's' : '')) +
          row('Home range', isNA(r.home_range_km2) ? NA_() : sig(r.home_range_km2) + ' km&sup2;') +
          row('Density', isNA(r.density_km2) ? NA_() : sig(r.density_km2) + ' ind./km&sup2;') +
          row('Group size', num(r.group_size, 1)) +
          row('Hibernation / torpor', isNA(r.hibernation) ? NA_() : (+r.hibernation ? 'Yes' : 'No')) +
          row('Diet', txt(r.diet)) + row('Foraging stratum', txt(r.forstrat)) + row('Activity', txt(r.activity)) + '</table>';
        return h;
      }
      function showTab(t) {
        var p = t === 'profile';
        document.getElementById('tab-profile').style.display = p ? 'block' : 'none';
        document.getElementById('tab-comments').style.display = p ? 'none' : 'flex';
        document.getElementById('tb-profile').className = 'tab' + (p ? ' on' : '');
        document.getElementById('tb-comments').className = 'tab' + (p ? '' : ' on');
      }
  )---")),
  div(id = "big",
      span(class = "close", title = "Close (Esc)", onclick = "hideBig()", HTML("&times;")),
      span(id = "big-nav", class = "nav"),
      div(class = "big-wrap",
        div(tags$img(id = "big-img", src = ""), div(id = "big-credit", class = "credit"), div(id = "big-cap", class = "cap")),
        div(id = "big-side", class = "big-side",
          div(class = "tabs",
            tags$button(id = "tb-profile", type = "button", class = "tab on", onclick = "showTab('profile')", "Profile"),
            tags$button(id = "tb-comments", type = "button", class = "tab", onclick = "showTab('comments')", "Comments")),
          div(id = "tab-profile", class = "tabc"),
          div(id = "tab-comments", class = "tabc", style = "display:none",
          tags$h5(id = "com-sp", "Comments"),
          div(id = "com-list"),
          div(class = "com-form",
            tags$input(id = "com-name", type = "text", class = "form-control", placeholder = "Your name", maxlength = 80),
            tags$textarea(id = "com-text", class = "form-control", placeholder = "Your comment on this photo", maxlength = 2000),
            tags$button(id = "com-send", type = "button", class = "btn btn-primary btn-sm", onclick = "sendComment()", "Send"),
            tags$span(id = "com-status")))))),

  h2(class = "app-title", "CHARISMAM — image viewer"),
  div(class = "filters",
    div(class = "f-sel", selectizeInput("order",  "Order",  choices = ord_choices())),
    div(class = "f-sel", selectizeInput("family", "Family", choices = fam_choices(ALL))),
    div(class = "f-sp",  selectizeInput("species", "Species", choices = sp_choices(),
                                        options = list(placeholder = "Latin or common name..."))),
    div(class = "f-sort", selectInput("sort", "Sort species by",
                                      choices = c("Name" = "alpha", "Public attention" = "public", "Research attention" = "acad"))),
    if (comments_url != "") div(class = "f-chk", checkboxInput("only_com", "Only photos with comments", FALSE))),
  div(class = "sub", sprintf(if (comments_url != "") "%s species, %d families, %d orders, %s photos \u2014 click a photo to enlarge it and leave a comment" else "%s species, %d families, %d orders, %s photos \u2014 click a photo to enlarge it",
                             format(nrow(sp_tab), big.mark = ","), nrow(fam_tab), nrow(ord_tab), format(nrow(img), big.mark = ","))),
  uiOutput("gallery")
)

server <- function(input, output, session) {

  set_sel <- function(id, choices, selected) {
    updateSelectizeInput(session, id, choices = choices, selected = selected)
  }

  # Order -> restricts families and species
  observeEvent(input$order, {
    fam <- if (input$family %in% fam_choices(input$order)) input$family else ALL
    sp_ok <- input$species != ALL &&
             (input$order == ALL || identical(sp_tab$order[sp_tab$species == input$species][1], input$order))
    sp  <- if (sp_ok) input$species else ALL
    set_sel("family", fam_choices(input$order), fam)
    if (!identical(sp, input$species)) updateSelectizeInput(session, "species", selected = sp)
  }, ignoreInit = TRUE)

  # Family -> sets the order, restricts species
  observeEvent(input$family, {
    ord <- if (input$family == ALL) input$order else fam_tab$order[fam_tab$family == input$family][1]
    if (!identical(ord, input$order)) {
      updateSelectizeInput(session, "order", selected = ord)
      set_sel("family", fam_choices(ord), input$family)
    }
    sp_ok <- input$species != ALL &&
             (input$family == ALL || identical(sp_tab$family[sp_tab$species == input$species][1], input$family))
    sp <- if (sp_ok) input$species else ALL
    if (!identical(sp, input$species)) updateSelectizeInput(session, "species", selected = sp)
  }, ignoreInit = TRUE)

  # Species -> sets family and order
  observeEvent(input$species, {
    if (input$species == ALL) return()
    r <- sp_tab[sp_tab$species == input$species, ][1, ]
    if (!identical(r$order, input$order))   updateSelectizeInput(session, "order", selected = r$order)
    if (!identical(r$family, input$family)) set_sel("family", fam_choices(r$order), r$family)
  })

  # What is shown follows the menus: species > family > order
  mode <- reactive({
    if (!is.null(input$species) && input$species != ALL) "species"
    else if (!is.null(input$family) && input$family != ALL) "family"
    else if (!is.null(input$order) && input$order != ALL) "order"
    else "none"
  })

  # Photos with comments (keys "species|photo label", sent by the browser)
  only_com <- reactive(isTRUE(input$only_com))
  com_keys <- reactive(input$commented)
  keep_com <- function(d) if (!only_com()) d else d[paste(d$species, d$lab, sep = "|") %in% com_keys(), , drop = FALSE]

  # Species order (sort menu); within a species: all then head, photo 1 then 2
  sort_rows <- function(d) {
    k <- switch(input$sort %||% "alpha",
                public = -att_of(d$species, "public"), acad = -att_of(d$species, "acad"), rep(0, nrow(d)))
    d[order(k, d$species, d$n, d$type, na.last = TRUE), , drop = FALSE]
  }
  empty_com <- function() div(class = "hint", if (is.null(com_keys())) "Loading comments..." else "No commented photo here yet.")

  output$gallery <- renderUI({
    switch(mode(),

      none = {
        if (!only_com())
          return(div(class = "hint", "Pick an order, a family or a species (search by Latin or common name)."))
        d <- keep_com(img)
        if (!nrow(d)) return(empty_com())
        tagList(div(class = "head", h3("Photos with comments"),
                    span(class = "tax", sprintf("%d species, %d photos", length(unique(d$species)), nrow(d)))),
                gallery(sort_rows(d)))
      },

      species = {
        d <- keep_com(img[img$species == input$species, ])
        if (!nrow(d)) return(empty_com())
        d <- d[order(d$n, d$type), ]                 # all then head, photo 1 then 2
        cm <- d$common[1]
        tagList(div(class = "head", h3(tags$em(input$species), iucn_badge(iucn_of(input$species))), if (nzchar(cm)) div(class = "common", cm),
                    span(class = "tax", sprintf("%s — %s", d$family[1], d$order[1]))),
                gallery(d, big = TRUE))
      },

      family = {
        d <- keep_com(img[img$family == input$family, ])
        if (!nrow(d)) return(empty_com())
        tagList(div(class = "head", h3(input$family),
                    span(class = "tax", sprintf("%s — %d species, %d photos", d$order[1],
                                                length(unique(d$species)), nrow(d)))),
                gallery(sort_rows(d)))
      },

      order = {
        d <- keep_com(img[img$order == input$order, ])
        if (!nrow(d)) return(empty_com())
        tagList(div(class = "head", h3(input$order),
                    span(class = "tax", sprintf("%d families, %d species, %d photos",
                                                length(unique(d$family)), length(unique(d$species)), nrow(d)))),
                # sorted by name: grouped by family; sorted by attention: one ranking for the whole order
                if ((input$sort %||% "alpha") == "alpha")
                  lapply(sort(unique(d$family)), function(f)
                    tagList(div(class = "grp", f), gallery(sort_rows(d[d$family == f, ]))))
                else gallery(sort_rows(d)))
      })
  })
}

shinyApp(ui, server)
