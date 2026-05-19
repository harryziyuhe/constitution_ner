# server.R

library(shiny)
library(jsonlite)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    # single-document mode
    text   = "",
    spans  = make_empty_spans(),
    next_uid = 1L,
    labels = default_labels,
    # dataset mode
    data      = NULL,
    text_col  = NULL,
    row_index = 1L,
    row_spans = list()
  )
  
  in_dataset_mode <- reactive({
    !is.null(rv$data) && !is.null(rv$text_col)
  })
  
  get_current_text <- function() {
    if (!is.null(rv$data) && !is.null(rv$text_col)) {
      txt <- rv$data[[rv$text_col]][rv$row_index]
      txt %||% ""
    } else {
      rv$text %||% ""
    }
  }
  
  save_current_row <- function() {
    if (isTRUE(in_dataset_mode())) {
      key <- as.character(rv$row_index)
      rv$row_spans[[key]] <- rv$spans
      txt_in <- isolate(input$text)
      if (!is.null(txt_in)) {
        rv$data[[rv$text_col]][rv$row_index] <- txt_in
      }
    }
  }
  
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
    #rv$row_index <- 1L
    #rv$row_spans <- list()
    #rv$spans <- make_empty_spans()
    updateTextAreaInput(session, "text", value = get_current_text())
  })
  
  output$row_info <- renderUI({
    if (!isTRUE(in_dataset_mode())) {
      return(tags$small("Mode: single document"))
    }
    n <- nrow(rv$data)
    i <- rv$row_index
    tagList(
      tags$div(tags$strong(sprintf("Row %d of %d", i, n))),
      br(),
      fluidRow(
        column(6, actionButton("prev_row", "Previous")),
        column(6, actionButton("next_row", "Next"))
      ),
      br(),
      fluidRow(
        column(
          7,
          numericInput(
            "goto_row",
            label = NULL,
            value = i,
            min = 1,
            max = n,
            step = 1
          )
        ),
        column(
          5,
          actionButton("go_row", "Go", class = "btn-primary")
        )
      ),
      tags$small("Tip: you can type a row number and press Go.")
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
  
  observeEvent(input$go_row, {
    if(!isTRUE(in_dataset_mode())) return()
    n <- nrow(rv$data)
    target <- as.integer(input$goto_row)
    
    if (is.na(target) || target < 1L || target > n) {
      showNotification(sprintf("Row must be between 1 and %d.", n), type = "error")
      return()
    }
    
    save_current_row()
    load_row(target)
  })
  
  # -------------------------
  # Text controls
  # -------------------------
  
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
  # Labels
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
  
  observe({
    labs <- rv$labels
    updateSelectInput(
      session, "label_select",
      choices = names(labs),
      selected = names(labs)[1] %||% ""
    )
  })
  
  observeEvent(input$add_label, {
    nm <- trimws(input$new_label_name)
    if (!nzchar(nm)) return()
    col <- input$new_label_color %||% "#16a34a"
    rv$labels[[nm]] <- col
    updateTextInput(session, "new_label_name", value = "")
  })
  
  observeEvent(input$label_hotkey, {
    idx  <- input$label_hotkey$index
    labs <- names(rv$labels)
    if (idx >= 1 && idx <= length(labs)) {
      updateSelectInput(session, "label_select", selected = labs[idx])
    }
  })
  
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
  
  observeEvent(input$labels_file, {
    req(input$labels_file)
    txt <- paste(readLines(input$labels_file$datapath, warn = FALSE), collapse = "\n")
    parsed <- tryCatch(fromJSON(txt), error = function(e) NULL)
    if (is.null(parsed)) {
      showNotification("Failed to parse labels JSON.", type = "error")
      return()
    }
    
    new_labels <- list()
    if (is.data.frame(parsed) && all(c("name", "color") %in% names(parsed))) {
      for (i in seq_len(nrow(parsed))) {
        nm <- as.character(parsed$name[i])
        col <- as.character(parsed$color[i])
        if (nzchar(nm) && nzchar(col)) new_labels[[nm]] <- col
      }
    } else if (is.list(parsed) && !is.null(names(parsed))) {
      for (nm in names(parsed)) {
        col <- as.character(parsed[[nm]])
        if (nzchar(nm) && nzchar(col)) new_labels[[nm]] <- col
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
    
    keep <- function(spans, labels) {
      if (!nrow(spans)) return(spans)
      spans[spans$label %in% names(labels), , drop = FALSE]
    }
    
    rv$spans <- keep(rv$spans, rv$labels)
    
    if (isTRUE(in_dataset_mode())) {
      for (k in names(rv$row_spans)) {
        rv$row_spans[[k]] <- keep(rv$row_spans[[k]], rv$labels)
      }
    }
    
    showNotification("Labels loaded.", type = "message")
  })
  
  # -------------------------
  # Selection handling
  # -------------------------
  
  observeEvent(input$selection, {
    sel <- input$selection
    
    doc_text <- get_current_text()
    if (!nzchar(doc_text)) return()
    
    start0 <- as.integer(sel$start)
    end0 <- as.integer(sel$end)
    
    if (is.na(start0) || is.na(end0) || start0 < 0L || end0 <= start0 || end0 > nchar(doc_text)) {
      showNotification("Invalid selection offsets.", type = "error")
      return()
    }
    
    lab <- input$label_select
    if (is.null(lab) || !nzchar(lab)) {
      showNotification("No label selected.", type = "warning")
      return()
    }
    
    cand <- add_span(rv$spans, rv$next_uid, start0, end0, lab)
    
    if (nrow(cand) != nrow(rv$spans)) {
      rv$next_uid <- rv$next_uid + 1L
      rv$spans <- cand[order(cand$start), , drop = FALSE]
      if (isTRUE(in_dataset_mode())) {
        key <- as.character(rv$row_index)
        rv$row_spans[[key]] <- rv$spans
      }
    } else {
      showNotification("Overlapping spans are not allowed.", type = "warning")
    }
  })
  
  # -------------------------
  # Viewer + spans UI
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
        
        span(
          class = "tag",
          style = paste0("border-color:", col, ";"),
          span(class = "swatch", style = paste0("background:", col, ";")),
          span(sprintf("%s [%d, %d) \"%s\"", s$label, s$start, s$end, txt)),
          tags$a(
            href = "#",
            onclick = sprintf(
              "Shiny.setInputValue('del_span_uid', %d, {priority: 'event'}); return false;",
              s$uid
            ),
            "\u2715"
          )
        )
      })
    )
  })
  
  observeEvent(input$del_span_uid, {
    uid <- as.integer(input$del_span_uid)
    if (is.na(uid)) return()
    
    rv$spans <- rv$spans[rv$spans$uid != uid, , drop = FALSE]
    
    if (isTRUE(in_dataset_mode())) {
      key <- as.character(rv$row_index)
      rv$row_spans[[key]] <- rv$spans
    }
  }, ignoreInit = TRUE)
  
  # observe({
  #   doc_text <- get_current_text()
  #   ord <- rv$spans[order(rv$spans$start), , drop = FALSE]
  #   for (i in seq_len(nrow(ord))) {
  #     local({
  #       ii <- i
  #       observeEvent(input[[paste0("del_span_", ii)]], {
  #         ord2 <- rv$spans[order(rv$spans$start), , drop = FALSE]
  #         if (nrow(ord2) >= ii) {
  #           rv$spans <- ord2[-ii, , drop = FALSE]
  #           if (isTRUE(in_dataset_mode())) {
  #             key <- as.character(rv$row_index)
  #             rv$row_spans[[key]] <- rv$spans
  #           }
  #         }
  #       }, ignoreInit = TRUE)
  #     })
  #   }
  # })
  
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
  
  output$dl_jsonl <- downloadHandler(
    filename = function() "annotations.jsonl",
    content = function(file) {
      if (isTRUE(in_dataset_mode())) {
        save_current_row()
        n <- nrow(rv$data)
        recs <- vector("list", n)
        for (i in seq_len(n)) {
          txt_i <- rv$data[[rv$text_col]][i] %||% ""
          spans_i <- rv$row_spans[[as.character(i)]]
          if (is.null(spans_i)) spans_i <- make_empty_spans()
          ents <- lapply(seq_len(nrow(spans_i)), function(j) as.list(spans_i[j, ]))
          recs[[i]] <- list(id = paste0("row-", i), text = txt_i, entities = ents)
        }
        lines <- vapply(recs, function(r) toJSON(r, auto_unbox = TRUE), character(1L))
        writeLines(lines, file)
      } else {
        ents <- lapply(seq_len(nrow(rv$spans)), function(i) as.list(rv$spans[i, ]))
        rec <- list(id = "doc-1", text = rv$text, entities = ents)
        writeLines(toJSON(rec, auto_unbox = TRUE), file)
      }
    }
  )
  
  # -------------------------
  # Import (JSON / JSONL restore)
  # -------------------------
  
  observeEvent(input$import_file, {
    req(input$import_file)
    
    lines <- readLines(input$import_file$datapath, warn = FALSE)
    lines <- lines[nzchar(trimws(lines))]
    if (!length(lines)) return()
    
    first <- trimws(lines[[1]])
    if (!startsWith(first, "{")) {
      showNotification("Unrecognized file format. Please import .json or .jsonl.", type = "error")
      return()
    }
    
    parsed <- lapply(lines, function(ln) {
      tryCatch(fromJSON(ln), error = function(e) NULL)
    })
    parsed <- Filter(Negate(is.null), parsed)
    
    if (!length(parsed)) {
      showNotification("Failed to parse JSON/JSONL.", type = "error")
      return()
    }
    
    if (length(parsed) >= 2) {
      jsonl_labels <- labels_from_jsonl(parsed)
      rv$labels <- assign_colors(jsonl_labels)
      # JSONL restore → dataset mode
      n <- length(parsed)
      texts <- vapply(parsed, function(r) as.character(r$text %||% ""), character(1L))
      ids   <- vapply(parsed, function(r) as.character(r$id %||% ""),  character(1L))
      
      rv$data <- data.frame(
        id   = ids,
        text = texts,
        stringsAsFactors = FALSE
      )
      rv$text_col  <- "text"
      rv$row_index <- 1L
      rv$row_spans <- vector("list", n)
      names(rv$row_spans) <- as.character(seq_len(n))
      
      for (i in seq_len(n)) {
        spans_i <- spans_from_record(parsed[[i]])
        if (nrow(spans_i)) {
          spans_i$uid <- seq_len(nrow(spans_i)) + rv$next_uid - 1L
          missing <- setdiff(unique(spans_i$label), names(rv$labels))
          if (length(missing)) {
            # extend with next colors
            existing_n <- length(rv$labels)
            new_cols <- rep(label_palette, length.out = existing_n + length(missing))
            new_cols <- new_cols[(existing_n + 1):(existing_n + length(missing))]
            for (k in seq_along(missing)) rv$labels[[missing[k]]] <- new_cols[k]
          }
        }
        rv$next_uid <- rv$next_uid + nrow(spans_i)
        rv$row_spans[[as.character(i)]] <- spans_i
      }
      
      rv$spans <- rv$row_spans[["1"]] %||% make_empty_spans()
      updateTextAreaInput(session, "text", value = rv$data[["text"]][1] %||% "")
      
      showNotification("JSONL imported: dataset state restored.", type = "message")
      return()
    }
    
    # Single JSON record → single-doc mode
    j <- parsed[[1]]
    
    rv$data <- NULL
    rv$text_col <- NULL
    rv$row_index <- 1L
    rv$row_spans <- list()
    
    rv$text <- as.character(j$text %||% rv$text %||% "")
    updateTextAreaInput(session, "text", value = rv$text)
    
    spans1 <- spans_from_record(j)
    ensure_labels_exist(spans1)
    rv$spans <- spans1
    
    showNotification("JSON imported (single document).", type = "message")
  })
}
