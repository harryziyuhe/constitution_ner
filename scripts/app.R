# app.R
# NER Labeler in Shiny with CSV dataset support + label save/load

library(shiny)
library(jsonlite)
library(htmltools)
library(colourpicker)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -------------------------
# Helpers
# -------------------------

make_empty_spans <- function() {
  data.frame(
    start = integer(),
    end   = integer(),
    label = character(),
    stringsAsFactors = FALSE
  )
}

add_span <- function(spans, start, end, label) {
  cand <- data.frame(
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
    return("")  # nothing inside the viewer yet
  }

  if (nrow(spans) == 0) {
    # Just plain text; outer div#viewer wraps this
    return(text)
  }

  spans <- spans[order(spans$start), , drop = FALSE]
  idx <- 0L
  pieces <- list()
  tlen <- nchar(text)

  for (i in seq_len(nrow(spans))) {
    s <- spans[i, ]

    # plain chunk
    if (idx < s$start) {
      plain <- substr(text, idx + 1L, s$start)
      pieces[[length(pieces) + 1L]] <- span(class = "seg", plain)
    }

    # labeled chunk
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

  # trailing text
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

# -------------------------
# UI
# -------------------------

ui <- fluidPage(
  tags$head(
    # Styling
    tags$style(HTML(
      r"(
      body {
        background: #0b1220;
        color: #e5e7eb;
        font-family: system-ui, -apple-system, "Segoe UI", Roboto, sans-serif;
      }
      .panel {
        background: #020617;
        border-radius: 12px;
        padding: 12px;
        border: 1px solid #1f2937;
        margin-bottom: 12px;
      }
      #viewer {
        white-space: pre-wrap;
        min-height: 260px;
        max-height: 540px;
        overflow: auto;
        border: 1px solid #1f2937;
        border-radius: 12px;
        padding: 12px;
        background: #020617;
      }
      .seg { display: inline; }
      .tag {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 2px 6px;
        border-radius: 999px;
        border: 1px solid #1f2937;
        font-size: 12px;
        margin: 3px;
      }
      .swatch {
        width: 12px;
        height: 12px;
        border-radius: 3px;
        border: 1px solid #1f2937;
        display: inline-block;
      }
      .label-row {
        display:flex;
        align-items:center;
        gap:8px;
        margin-bottom:6px;
      }
      )"
    )),
    # JS: global listener for selection + hotkeys
    tags$script(HTML(
      r"(
      (function () {
        // Mouse selection → send selected text
        document.addEventListener('mouseup', function () {
          var viewer = document.getElementById('viewer');
          if (!viewer) return;

          var sel = window.getSelection();
          if (!sel || sel.rangeCount === 0) return;

          var r = sel.getRangeAt(0);
          if (!viewer.contains(r.startContainer) || !viewer.contains(r.endContainer)) return;

          var txt = sel.toString();
          if (!txt || !txt.trim()) return;

          Shiny.setInputValue(
            'selection',
            { text: txt, nonce: Math.random() },
            { priority: 'event' }
          );

          try { sel.removeAllRanges(); } catch (e) {}
        });

        // hotkeys 1-9 for label selection
        document.addEventListener('keydown', function (e) {
          var tag = (document.activeElement && document.activeElement.tagName) || '';
          if (tag === 'INPUT' || tag === 'TEXTAREA') return;

          var n = parseInt(e.key, 10);
          if (!Number.isNaN(n) && n >= 1 && n <= 9) {
            Shiny.setInputValue(
              'label_hotkey',
              { index: n, nonce: Math.random() },
              { priority: 'event' }
            );
          }
        }, { passive: true });
      })();
      )"
    ))
  ),

  titlePanel("NER Labeler (Shiny, dataset mode + label presets)"),

  fluidRow(
    column(
      4,
      div(class = "panel",
          h4("Text, labels, dataset"),
          fileInput("csv_file", "CSV dataset", accept = c(".csv")),
          uiOutput("csv_ui"),
          hr(),
          textAreaInput(
            "text", "Current text", rows = 8,
            placeholder = "Paste or type text here..."
          ),
          actionButton("load_sample", "Load sample"),
          actionButton("apply_text", "Update text", class = "btn-primary"),
          actionButton("clear_text", "Clear text"),
          br(), br(),
          h5("Labels"),
          uiOutput("label_list"),
          hr(),
          textInput("new_label_name", "New label name", ""),
          colourInput("new_label_color", "Color", value = "#16a34a"),
          br(),
          actionButton("add_label", "Add label"),
          br(), br(),
          h6("Label set presets"),
          downloadButton("dl_labels", "Export labels"),
          fileInput("labels_file", "Import labels (.json)", accept = ".json")
      )
    ),
    column(
      8,
      div(class = "panel",
          h4("Annotate"),
          fluidRow(
            column(
              6,
              selectInput("label_select", "Current label", choices = names(default_labels))
            ),
            column(
              6,
              uiOutput("row_info")
            )
          ),
          # Static viewer element; inner content is rendered by uiOutput
          div(id = "viewer",
              uiOutput("viewer_html")
          ),
          br(),
          h5("Current spans (this row / document)"),
          uiOutput("spans_ui"),
          hr(),
          h5("Export / import"),
          downloadButton("dl_jsonl", "Export JSONL (all docs/rows)"),
          downloadButton("dl_brat", "Export BRAT .ann (current doc/row)"),
          fileInput("import_file", "Import JSON/JSONL/BRAT (single doc mode only)",
                    accept = c(".json", ".jsonl", ".ann", ".txt")),
          actionButton("clear_spans", "Clear spans for current doc/row", class = "btn-danger"),
          tags$small(class = "text-muted",
                     "JSONL export: one line per document/row, with 0-based [start, end) offsets.")
      )
    )
  )
)

# -------------------------
# Server
# -------------------------

server <- function(input, output, session) {
  rv <- reactiveValues(
    # single-document mode
    text   = "",
    spans  = make_empty_spans(),
    labels = default_labels,
    # dataset mode
    data      = NULL,   # data.frame
    text_col  = NULL,   # name of text column
    row_index = 1L,
    row_spans = list()  # list of spans per row, keyed by row_index as character
  )

  # helper: are we in dataset mode?
  in_dataset_mode <- reactive({
    !is.null(rv$data) && !is.null(rv$text_col)
  })

  # helper: get current text (single-doc or dataset row)
  get_current_text <- function() {
    if (!is.null(rv$data) && !is.null(rv$text_col)) {
      txt <- rv$data[[rv$text_col]][rv$row_index]
      txt %||% ""
    } else {
      rv$text %||% ""
    }
  }

  # save spans & edited text for current row (dataset mode)
  save_current_row <- function() {
    if (isTRUE(in_dataset_mode())) {
      key <- as.character(rv$row_index)
      rv$row_spans[[key]] <- rv$spans
      # also sync edited text back into the data
      txt_in <- isolate(input$text)
      if (!is.null(txt_in)) {
        rv$data[[rv$text_col]][rv$row_index] <- txt_in
      }
    }
  }

  # load row i (dataset mode)
  load_row <- function(i) {
    if (!isTRUE(in_dataset_mode())) return()
    n <- nrow(rv$data)
    if (i < 1L || i > n) return()
    rv$row_index <- i
    key <- as.character(rv$row_index)
    if (!is.null(rv$row_spans[[key]])) {
      rv$spans <- rv$row_spans[[key]]
    } else {
      rv$spans <- make_empty_spans()
    }
    updateTextAreaInput(session, "text", value = get_current_text())
  }

  # -------------------------
  # CSV dataset handling
  # -------------------------

  observeEvent(input$csv_file, {
    req(input$csv_file)
    df <- tryCatch(
      read.csv(input$csv_file$datapath, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    if (is.null(df)) {
      showNotification("Failed to read CSV.", type = "error")
      return()
    }
    rv$data <- df
    rv$text_col <- NULL
    rv$row_index <- 1L
    rv$row_spans <- list()
  })

  output$csv_ui <- renderUI({
    if (is.null(rv$data)) {
      return(tags$small("No dataset loaded. You can still work in single-document mode."))
    }
    cols <- names(rv$data)
    tagList(
      selectInput("text_col", "Text column", choices = cols, selected = rv$text_col %||% cols[1]),
      tags$small(sprintf("Dataset rows: %d", nrow(rv$data)))
    )
  })

  observeEvent(input$text_col, {
    req(rv$data)
    rv$text_col <- input$text_col
    rv$row_index <- 1L
    rv$row_spans <- list()
    rv$spans <- make_empty_spans()
    updateTextAreaInput(session, "text", value = get_current_text())
  })

  # Row info + navigation
  output$row_info <- renderUI({
    if (!isTRUE(in_dataset_mode())) {
      return(tags$small("Mode: single document"))
    }
    n <- nrow(rv$data)
    i <- rv$row_index
    tagList(
      tags$div(
        tags$strong(sprintf("Row %d of %d", i, n))
      ),
      br(),
      actionButton("prev_row", "Previous"),
      actionButton("next_row", "Next")
    )
  })

  observeEvent(input$prev_row, {
    if (!isTRUE(in_dataset_mode())) return()
    save_current_row()
    if (rv$row_index > 1L) {
      load_row(rv$row_index - 1L)
    }
  })

  observeEvent(input$next_row, {
    if (!isTRUE(in_dataset_mode())) return()
    save_current_row()
    n <- nrow(rv$data)
    if (rv$row_index < n) {
      load_row(rv$row_index + 1L)
    }
  })

  # -------------------------
  # Text controls
  # -------------------------

  # Sample text (single-doc or current row)
  observeEvent(input$load_sample, {
    sample_txt <- "President Alice met Bob at Acme Corp in New York to sign the Data Fairness Act."
    if (isTRUE(in_dataset_mode())) {
      rv$data[[rv$text_col]][rv$row_index] <- sample_txt
      rv$spans <- make_empty_spans()
      updateTextAreaInput(session, "text", value = sample_txt)
    } else {
      rv$text  <- sample_txt
      rv$spans <- make_empty_spans()
      updateTextAreaInput(session, "text", value = rv$text)
    }
  })

  # Apply text
  observeEvent(input$apply_text, {
    new_text <- input$text %||% ""
    if (isTRUE(in_dataset_mode())) {
      if (!identical(new_text, get_current_text())) {
        rv$data[[rv$text_col]][rv$row_index] <- new_text
        rv$spans <- rv$spans[rv$spans$end <= nchar(new_text), , drop = FALSE]
        key <- as.character(rv$row_index)
        rv$row_spans[[key]] <- rv$spans
      }
    } else {
      if (!identical(new_text, rv$text)) {
        rv$text <- new_text
        rv$spans <- rv$spans[rv$spans$end <= nchar(rv$text), , drop = FALSE]
      }
    }
  })

  # Clear text
  observeEvent(input$clear_text, {
    if (isTRUE(in_dataset_mode())) {
      rv$data[[rv$text_col]][rv$row_index] <- ""
      rv$spans <- make_empty_spans()
      key <- as.character(rv$row_index)
      rv$row_spans[[key]] <- rv$spans
      updateTextAreaInput(session, "text", value = "")
    } else {
      rv$text  <- ""
      rv$spans <- make_empty_spans()
      updateTextAreaInput(session, "text", value = "")
    }
  })

  # -------------------------
  # Labels (including save/load)
  # -------------------------

  output$label_list <- renderUI({
    labs <- rv$labels
    if (!length(labs)) return(tags$em("No labels defined yet."))

    tagList(
      lapply(seq_along(labs), function(i) {
        nm  <- names(labs)[i]
        col <- labs[[i]]
        div(class = "label-row",
            div(class = "swatch", style = paste0("background:", col, ";")),
            span(nm),
            tags$small(paste0("(shortcut: ", i, ")"))
        )
      })
    )
  })

  # Keep label dropdown in sync
  observe({
    labs <- rv$labels
    updateSelectInput(
      session, "label_select",
      choices = names(labs),
      selected = names(labs)[1] %||% ""
    )
  })

  # Add label
  observeEvent(input$add_label, {
    nm <- trimws(input$new_label_name)
    if (!nzchar(nm)) return()
    col <- input$new_label_color %||% "#16a34a"
    rv$labels[[nm]] <- col
    updateTextInput(session, "new_label_name", value = "")
  })

  # Hotkeys 1–9 to pick label
  observeEvent(input$label_hotkey, {
    idx  <- input$label_hotkey$index
    labs <- names(rv$labels)
    if (idx >= 1 && idx <= length(labs)) {
      updateSelectInput(session, "label_select", selected = labs[idx])
    }
  })

  # Export labels as JSON
  output$dl_labels <- downloadHandler(
    filename = function() "labels.json",
    content = function(file) {
      labs <- rv$labels
      if (!length(labs)) {
        writeLines("[]", file)
        return()
      }
      df <- data.frame(
        name  = names(labs),
        color = unname(unlist(labs)),
        stringsAsFactors = FALSE
      )
      writeLines(toJSON(df, auto_unbox = TRUE, pretty = TRUE), file)
    }
  )

  # Import labels from JSON
  observeEvent(input$labels_file, {
    req(input$labels_file)
    txt <- paste(readLines(input$labels_file$datapath, warn = FALSE), collapse = "\n")
    parsed <- tryCatch(
      fromJSON(txt),
      error = function(e) NULL
    )
    if (is.null(parsed)) {
      showNotification("Failed to parse labels JSON.", type = "error")
      return()
    }

    # Accept either a data.frame with name/color or a named list
    new_labels <- list()
    if (is.data.frame(parsed) && all(c("name", "color") %in% names(parsed))) {
      for (i in seq_len(nrow(parsed))) {
        nm <- as.character(parsed$name[i])
        col <- as.character(parsed$color[i])
        if (nzchar(nm) && nzchar(col)) {
          new_labels[[nm]] <- col
        }
      }
    } else if (is.list(parsed) && !is.null(names(parsed))) {
      # object like {"PERSON":"#22c55e", ...}
      for (nm in names(parsed)) {
        col <- as.character(parsed[[nm]])
        if (nzchar(nm) && nzchar(col)) {
          new_labels[[nm]] <- col
        }
      }
    } else {
      showNotification("Labels JSON must be either [{name,color},...] or {name: color, ...}.", type = "error")
      return()
    }

    if (!length(new_labels)) {
      showNotification("No valid labels found in JSON.", type = "warning")
      return()
    }

    rv$labels <- new_labels

    # Drop spans whose labels are no longer in the label set
    keep <- function(spans, labels) {
      if (!nrow(spans)) return(spans)
      spans[spans$label %in% names(labels), , drop = FALSE]
    }

    rv$spans <- keep(rv$spans, rv$labels)

    if (isTRUE(in_dataset_mode())) {
      # Clean spans for all rows
      for (k in names(rv$row_spans)) {
        rv$row_spans[[k]] <- keep(rv$row_spans[[k]], rv$labels)
      }
    }

    showNotification("Labels loaded.", type = "message")
  })

  # -------------------------
  # Selection from JS (substring-based; non-overlapping occurrences)
  # -------------------------

  observeEvent(input$selection, {
    sel <- input$selection
    raw <- sel$text
    if (is.null(raw) || !nzchar(raw)) return()

    # Trim leading/trailing whitespace for matching
    trimmed <- gsub("^\\s+|\\s+$", "", raw)
    if (!nzchar(trimmed)) return()

    doc_text <- get_current_text()
    if (!nzchar(doc_text)) return()

    # Find all occurrences of trimmed selection in the current text
    locs <- gregexpr(trimmed, doc_text, fixed = TRUE)[[1]]
    if (locs[1] == -1) {
      showNotification("Could not locate selected text in the document.", type = "error")
      return()
    }

    lab <- input$label_select
    if (is.null(lab) || !nzchar(lab)) {
      showNotification("No label selected.", type = "warning")
      return()
    }

    len <- nchar(trimmed)
    existing <- rv$spans

    chosen_start <- NULL
    chosen_end   <- NULL

    # Pick the first occurrence that does NOT overlap existing spans
    for (pos in locs) {
      start0 <- pos - 1L           # 0-based
      end0   <- start0 + len

      overlaps <- FALSE
      if (nrow(existing) > 0) {
        for (j in seq_len(nrow(existing))) {
          s <- existing[j, ]
          if (max(s$start, start0) < min(s$end, end0)) {
            overlaps <- TRUE
            break
          }
        }
      }

      if (!overlaps) {
        chosen_start <- start0
        chosen_end   <- end0
        break
      }
    }

    if (is.null(chosen_start)) {
      # all occurrences would overlap
      showNotification("All occurrences of this selection overlap existing spans.", type = "warning")
      return()
    }

    cand <- add_span(rv$spans, chosen_start, chosen_end, lab)
    if (nrow(cand) != nrow(rv$spans)) {
      rv$spans <- cand[order(cand$start), , drop = FALSE]
      # store back for this row if in dataset mode
      if (isTRUE(in_dataset_mode())) {
        key <- as.character(rv$row_index)
        rv$row_spans[[key]] <- rv$spans
      }
    } else {
      showNotification("Overlapping spans are not allowed.", type = "warning")
    }
  })

  # -------------------------
  # Viewer + spans
  # -------------------------

  output$viewer_html <- renderUI({
    render_highlighted(get_current_text(), rv$spans, rv$labels)
  })

  output$spans_ui <- renderUI({
    doc_text <- get_current_text()
    if (nrow(rv$spans) == 0) {
      return(tags$em("No spans yet. Select text in the viewer to create one."))
    }

    ord <- rv$spans[order(rv$spans$start), , drop = FALSE]

    tagList(
      lapply(seq_len(nrow(ord)), function(i) {
        s   <- ord[i, ]
        col <- rv$labels[[s$label]] %||% "#7dd3fc"
        txt <- substr(doc_text, s$start + 1L, s$end)

        div(
          class = "tag",
          style = paste0("border-color:", col, ";"),
          span(class = "swatch", style = paste0("background:", col, ";")),
          span(sprintf("%s [%d, %d) \"%s\"",
                       s$label, s$start, s$end, txt)),
          actionLink(paste0("del_span_", i), "\u2715")
        )
      })
    )
  })

  # Delete span handlers (current doc/row only)
  observe({
    doc_text <- get_current_text()  # for dependency
    ord <- rv$spans[order(rv$spans$start), , drop = FALSE]
    for (i in seq_len(nrow(ord))) {
      local({
        ii <- i
        observeEvent(input[[paste0("del_span_", ii)]], {
          ord2 <- rv$spans[order(rv$spans$start), , drop = FALSE]
          if (nrow(ord2) >= ii) {
            rv$spans <- ord2[-ii, , drop = FALSE]
            if (isTRUE(in_dataset_mode())) {
              key <- as.character(rv$row_index)
              rv$row_spans[[key]] <- rv$spans
            }
          }
        }, ignoreInit = TRUE)
      })
    }
  })

  # Clear spans for current doc/row
  observeEvent(input$clear_spans, {
    if (!nrow(rv$spans)) return()
    rv$spans <- make_empty_spans()
    if (isTRUE(in_dataset_mode())) {
      key <- as.character(rv$row_index)
      rv$row_spans[[key]] <- rv$spans
    }
  })

  # -------------------------
  # Export
  # -------------------------

  # JSONL: all docs
  output$dl_jsonl <- downloadHandler(
    filename = function() "annotations.jsonl",
    content = function(file) {
      if (isTRUE(in_dataset_mode())) {
        # Make sure current row's spans + text are saved
        save_current_row()
        n <- nrow(rv$data)
        recs <- vector("list", n)
        for (i in seq_len(n)) {
          txt_i <- rv$data[[rv$text_col]][i] %||% ""
          spans_i <- rv$row_spans[[as.character(i)]]
          if (is.null(spans_i)) spans_i <- make_empty_spans()
          ents <- lapply(seq_len(nrow(spans_i)), function(j) {
            as.list(spans_i[j, ])
          })
          recs[[i]] <- list(
            id       = paste0("row-", i),
            text     = txt_i,
            entities = ents
          )
        }
        lines <- vapply(recs, function(r) {
          toJSON(r, auto_unbox = TRUE)
        }, character(1L))
        writeLines(lines, file)
      } else {
        ents <- lapply(seq_len(nrow(rv$spans)), function(i) {
          as.list(rv$spans[i, ])
        })
        rec <- list(
          id       = "doc-1",
          text     = rv$text,
          entities = ents
        )
        line <- toJSON(rec, auto_unbox = TRUE)
        writeLines(line, file)
      }
    }
  )

  # BRAT: current doc/row only
  output$dl_brat <- downloadHandler(
    filename = function() {
      if (isTRUE(in_dataset_mode())) {
        sprintf("row-%d.ann", rv$row_index)
      } else {
        "annotations.ann"
      }
    },
    content = function(file) {
      doc_text <- get_current_text()
      if (!nrow(rv$spans)) {
        writeLines("", file); return()
      }
      lines <- vapply(seq_len(nrow(rv$spans)), function(i) {
        s   <- rv$spans[i, ]
        txt <- gsub("\n", " ", substr(doc_text, s$start + 1L, s$end))
        sprintf("T%d\t%s %d %d\t%s", i, s$label, s$start, s$end, txt)
      }, character(1L))
      writeLines(lines, file)
    }
  )

  # -------------------------
  # Import (single-doc mode only, to avoid chaos with dataset)
  # -------------------------

  observeEvent(input$import_file, {
    if (isTRUE(in_dataset_mode())) {
      showNotification("Import is only supported in single-document mode.", type = "warning")
      return()
    }

    req(input$import_file)
    lines <- readLines(input$import_file$datapath, warn = FALSE)
    lines <- lines[nzchar(trimws(lines))]
    if (!length(lines)) return()
    first <- trimws(lines[[1]])

    if (startsWith(first, "{")) {
      # JSON / JSONL: use first line
      j <- fromJSON(first)
      rv$text <- j$text %||% rv$text
      updateTextAreaInput(session, "text", value = rv$text)
      ents <- j$entities %||% j$spans
      if (!is.null(ents) && length(ents)) {
        df <- as.data.frame(do.call(rbind, lapply(ents, as.data.frame)))
        df$start <- as.integer(df$start)
        df$end   <- as.integer(df$end)
        df$label <- as.character(df$label %||% df$type)
        df <- df[, c("start", "end", "label")]
        rv$spans <- df[order(df$start), , drop = FALSE]
      }
    } else if (grepl("^T\\d+\\t", first)) {
      # BRAT .ann
      parsed <- lapply(lines, function(ln) {
        m <- regexec("^T\\d+\\t(\\S+)\\s+(\\d+)\\s+(\\d+)\\t(.+)$", ln)
        md <- regmatches(ln, m)[[1]]
        if (length(md) == 5) {
          list(
            label = md[2],
            start = as.integer(md[3]),
            end   = as.integer(md[4])
          )
        } else NULL
      })
      parsed <- Filter(Negate(is.null), parsed)
      if (length(parsed)) {
        df <- as.data.frame(do.call(rbind, lapply(parsed, as.data.frame)))
        df <- df[, c("start", "end", "label")]
        df$start <- as.integer(df$start)
        df$end   <- as.integer(df$end)
        df$label <- as.character(df$label)
        rv$spans <- df[order(df$start), , drop = FALSE]
      }
    } else {
      showNotification("Unrecognized file format.", type = "error")
    }
  })
}

shinyApp(ui, server)

