# tools/zotero_to_qmd.R
library(jsonlite)
library(fs)
library(glue)
library(stringi)
library(xml2)
# library(readr) # no es necesario, puedes quitarlo

DATA_JSON <- "actividades/posts/presentaciones/data/presentaciones.json"
DATA_CSL  <- "actividades/posts/presentaciones/data/apa-es.csl"
OUT_DIR   <- "actividades/presentaciones"
dir_create(OUT_DIR)

stopifnot(file_exists(DATA_JSON))
json <- read_json(DATA_JSON, simplifyVector = FALSE)

# ---------------- utilidades ----------------
norm <- function(x) ifelse(is.null(x) || length(x) == 0, "", x)
yml_escape <- function(x) gsub('"','\\"',x,fixed=TRUE)

# === Reemplaza fmt_authors por esta versión robusta ===
fmt_authors <- function(auth) {
  if (is.null(auth) || length(auth) == 0) return(character(0))
  vec <- vapply(auth, function(a) {
    # si viene como 'literal' (muy común con presentaciones)
    if (!is.null(a$literal) && nzchar(a$literal)) return(trimws(a$literal))
    given  <- norm(a$given); family <- norm(a$family)
    nm <- trimws(paste(given, family))
    if (nm == "") NA_character__ else nm
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
  vec <- vec[!is.na(vec) & nzchar(vec)]
  unname(vec)
}

# Ayuda: toma la primera lista de personas disponible en estas claves
get_person_list <- function(it, keys = c("author","presenter","container-author","editor","composer","director")) {
  for (k in keys) {
    v <- it[[k]]
    if (!is.null(v) && length(v) > 0) return(v)
  }
  NULL
}
to_categoria <- function(t){
  switch(norm(t),
         "speech"="charla","paper-conference"="presentación","presentation"="taller",
         "webpage"="presentación","report"="presentación","presentación")
}
kv_from_note <- function(key, txt){
  m <- regmatches(txt, regexpr(paste0("(?i)",key,"\\s*:\\s*\\S+"), txt, perl=TRUE))
  if (length(m)==0) "" else sub("(?i).*:\\s*","",m,perl=TRUE)
}
parse_csl_date <- function(issued){
  if (!is.null(issued$`date-parts`) && length(issued$`date-parts`)){
    dp <- suppressWarnings(as.integer(as.numeric(unlist(issued$`date-parts`[[1]]))))
    dp <- dp[!is.na(dp)]
    if (length(dp)>=3) return(sprintf("%04d-%02d-%02d", dp[1], pmax(1,pmin(12,dp[2])), pmax(1,pmin(31,dp[3]))))
    if (length(dp)==2) return(sprintf("%04d-%02d-01", dp[1], pmax(1,pmin(12,dp[2]))))
    if (length(dp)==1) return(sprintf("%04d-01-01", dp[1]))
  }
  for (raw in c(issued$raw, issued$literal, issued$`date-time`, issued$date)){
    if (!is.null(raw)){ d <- suppressWarnings(as.Date(raw)); if (!is.na(d)) return(as.character(d)) }
  }
  format(Sys.Date(), "%Y-%m-%d")
}
is_valid_csl <- function(path){
  if (!file_exists(path)) return(FALSE)
  ok <- FALSE; try({ doc <- xml2::read_xml(path); ok <- identical(xml2::xml_name(xml2::xml_root(doc)),"style") }, silent=TRUE); ok
}

# ---------- helpers de EMBED (iframe simple como tu ejemplo) ----------
is_html     <- function(u) grepl("\\.html?(#|\\?|$)", u, ignore.case=TRUE)
is_pdf      <- function(u) grepl("\\.pdf(#|\\?|$)",  u, ignore.case=TRUE)
is_yt       <- function(u) grepl("youtube\\.com|youtu\\.be", u, ignore.case=TRUE)
is_vimeo    <- function(u) grepl("vimeo\\.com", u, ignore.case=TRUE)
is_gslides  <- function(u) grepl("docs\\.google\\.com/presentation", u, ignore.case=TRUE)

yt_embed <- function(u){
  u <- sub("https?://youtu\\.be/([A-Za-z0-9_-]+).*", "https://www.youtube.com/embed/\\1", u)
  sub("https?://www\\.youtube\\.com/watch\\?v=([A-Za-z0-9_-]+).*", "https://www.youtube.com/embed/\\1", u)
}
gslides_embed <- function(u){
  sub("/(edit|view).*", "/embed?start=false&loop=false&delayms=3000", u)
}

HEIGHT_DEFAULT <- 550  # YouTube/Vimeo (cámbialo si quieres)
HEIGHT_TALL    <- 900  # PDF / Google Slides / HTML slides

make_iframe <- function(u, tall = FALSE){
  h <- if (tall) HEIGHT_TALL else HEIGHT_DEFAULT
  sprintf('<iframe src="%s" width="100%%" height="%d" allowfullscreen="true" loading="lazy"></iframe>', u, h)
}

build_embed <- function(slides, url){
  cand <- if (nzchar(slides)) slides else url
  if (!nzchar(cand)) return("")
  if (is_yt(cand))      return(make_iframe(yt_embed(cand), tall = FALSE))
  if (is_gslides(cand)) return(make_iframe(gslides_embed(cand), tall = TRUE))
  if (is_pdf(cand))     return(make_iframe(cand, tall = TRUE))
  if (is_html(cand))    return(make_iframe(cand, tall = TRUE))
  make_iframe(cand, tall = FALSE)
}

# --------- generación de páginas ----------
csl_ok <- is_valid_csl(DATA_CSL)

for (it in json) {
  id     <- norm(it$id)
  title  <- norm(it$title)
  event  <- norm(it$event)
  place  <- norm(it$`event-place`)
  abstr  <- norm(it$abstract)
  url    <- norm(it$URL)
  notes  <- paste(norm(it$note), norm(it$`container-title`))
  date   <- parse_csl_date(it$issued)
  
  slides <- kv_from_note("slides", notes)
  video  <- kv_from_note("video",  notes)
  repo   <- kv_from_note("repo",   notes)
  
  authors <- fmt_authors(get_person_list(it))
  categoria <- to_categoria(it$type)
  year      <- substr(date, 1, 4)
  
  slug <- gsub("[^a-z0-9]+","-", stringi::stri_trans_general(tolower(title), "Latin-ASCII"))
  page_dir <- file.path(OUT_DIR, paste0(year,"-",slug)); dir_create(page_dir)
  
  bib_rel <- gsub("\\\\","/", path_rel(DATA_JSON, start = page_dir))
  csl_rel <- gsub("\\\\","/", path_rel(DATA_CSL,  start = page_dir))
  
  # descripción corta
  desc <- if (nzchar(abstr)) substr(gsub("\\s+"," ",abstr), 1, 260) else ""
  if (!nzchar(desc)) desc <- if (nzchar(event)) event else title
  if (nchar(desc) == 260) desc <- paste0(desc, "…")
  
  # --- YAML header ---
  header <- c(
    "---",
    "page-layout: full",
    'lang: es',  # para términos/locales en español (si tu CSL lo soporta)
    glue('title: "{yml_escape(title)}"'),
    glue('date: {date}'),
    # NO pongas author aquí
    glue('categories: ["{categoria}", "LISA"]'),
    glue('event: "{yml_escape(event)}"'),
    glue('location: "{yml_escape(place)}"'),
    glue('description: "{yml_escape(desc)}"'),
    glue('bibliography: "{bib_rel}"'),
    glue('nocite: "@{id}"')   # muestra la referencia en #refs sin cita en línea
  )
  if (length(authors) > 0) {
    header <- append(header, glue('author: [{paste0(\'"\', authors, \'"\', collapse = ", ")}]'), after = 5)
  }
  if (csl_ok) header <- c(header, glue('csl: "{csl_rel}"'))
  header <- c(header, "---")
  
  # --- EMBED automático ---
  embed_html <- build_embed(slides, url)
  
  # --- Body ---
  body <- c(
    "",
    if (nzchar(desc)) c(desc, "") else NULL,
    if (nzchar(embed_html)) c("### Presentación", embed_html, "") else NULL,
    "### Recursos",
    if (nzchar(url))    paste0("- **Enlace principal**: ", url) else "-",
    if (nzchar(slides)) paste0("- **Slides**: ",  slides) else NULL,
    if (nzchar(video))  paste0("- **Video**: ",   video)  else NULL,
    if (nzchar(repo))   paste0("- **Repositorio**: ", repo) else NULL,
    "",
    # === callout con la bibliografía ===
    '::: {.callout-note title="Cómo citar (APA)"}',
    "",
    "::: {#refs}",
    ":::",
    "",
    ":::"
  )
  
  qmd <- paste(c(header, body), collapse = "\n")
  writeLines(qmd, file.path(page_dir, "index.qmd"), useBytes = TRUE)
}

# sublistado informativo (por si navegan directo a /actividades/presentaciones/)
writeLines('
---
title: "Presentaciones, talleres y charlas"
page-layout: full
listing:
  contents: "*/*"
  sort: "date desc"
  type: default
  fields: [date, author, title, categories]
  filter-ui: [search, categories]
  categories: numbered
---
', file.path(OUT_DIR, "index.qmd"), useBytes = TRUE)


