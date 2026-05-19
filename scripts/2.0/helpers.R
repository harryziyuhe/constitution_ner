# helpers.R

library(htmltools)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

make_empty_spans <- function() {
  data.frame(
    uid = integer(),
    start = integer(),
    end   = integer(),
    label = character(),
    stringsAsFactors = FALSE
  )
}

add_span <- function(spans, uid, start, end, label) {
  cand <- data.frame(
    uid = as.integer(uid),
    start = as.integer(start),
    end   = as.integer(end),
    label = as.character(label),
    stringsAsFactors = FALSE
  )
  
  if (nrow(spans) == 0) {
    return(cand)
  }
  
  # reject overlaps
  for (i in seq_len(nrow(spans))) {
    s <- spans[i, ]
    overlap <- max(s$start, start) < min(s$end, end)
    if (overlap) return(spans)
  }
  
  rbind(spans, cand)
}

render_highlighted <- function(text, spans, labels) {
  if (is.null(text) || identical(text, "")) {
    return("")
  }
  
  if (nrow(spans) == 0) {
    return(text)
  }
  
  spans <- spans[order(spans$start), , drop = FALSE]
  idx <- 0L
  pieces <- list()
  tlen <- nchar(text)
  
  for (i in seq_len(nrow(spans))) {
    s <- spans[i, ]
    
    if (idx < s$start) {
      plain <- substr(text, idx + 1L, s$start)
      pieces[[length(pieces) + 1L]] <- span(class = "seg", plain)
    }
    
    seg <- substr(text, s$start + 1L, s$end)
    col <- labels[[s$label]] %||% "#7dd3fc"
    
    pieces[[length(pieces) + 1L]] <- span(
      class = "seg",
      style = sprintf(
        "background-color:%s33;border:1px solid %s;border-radius:8px;padding:0 2px;",
        col, col
      ),
      seg
    )
    
    idx <- s$end
  }
  
  if (idx < tlen) {
    rest <- substr(text, idx + 1L, tlen)
    pieces[[length(pieces) + 1L]] <- span(class = "seg", rest)
  }
  
  tagList(pieces)
}

default_labels <- list(
  PERSON = "#22c55e",
  ORG    = "#60a5fa",
  GPE    = "#f43f5e",
  LAW    = "#eab308"
)

# Build a deterministic palette (Okabe–Ito + a few extras)
label_palette <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#999999",
  "#F97316", "#A855F7", "#22C55E", "#06B6D4",
  "#EF4444", "#84CC16", "#F59E0B", "#3B82F6"
)

spans_from_record <- function(rec) {
  ents <- rec$entities %||% rec$spans
  if (is.null(ents) || !length(ents)) return(make_empty_spans())
  
  if (is.data.frame(ents)) {
    df <- ents
  } else {
    df <- as.data.frame(do.call(rbind, lapply(ents, as.data.frame)))
  }
  
  if (!("label" %in% names(df)) && ("type" %in% names(df))) df$label <- df$type
  if (!("uid" %in% names(df))) df$uid <- seq_len(nrow(df))
  
  df$start <- as.integer(df$start)
  df$end   <- as.integer(df$end)
  df$label <- as.character(df$label)
  df <- df[, c("uid", "start", "end", "label")]
  df[order(df$start), , drop = FALSE]
}

labels_from_jsonl <- function(parsed_records) {
  labs <- character(0)
  for (rec in parsed_records) {
    ents <- rec$entities %||% rec$spans
    if (is.null(ents) || !length(ents)) next
    if (is.data.frame(ents)) {
      labcol <- ents$label
      if (is.null(labcol) && "type" %in% names(ents)) labcol <- ents$type
      if (!is.null(labcol)) labs <- c(labs, as.character(labcol))
    } else {
      # list of entities
      for (e in ents) {
        lb <- e$label %||% e$type
        if (!is.null(lb)) labs <- c(labs, as.character(lb))
      }
    }
  }
  labs <- unique(labs[nzchar(labs)])
  sort(labs)
}

assign_colors <- function(label_names) {
  if (!length(label_names)) return(list())
  cols <- rep(label_palette, length.out = length(label_names))
  out <- as.list(cols)
  names(out) <- label_names
  out
}

