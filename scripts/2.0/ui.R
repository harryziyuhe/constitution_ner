# ui.R

library(shiny)
library(htmltools)
library(colourpicker)

ui <- fluidPage(
  tags$head(
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
        white-space: normal;
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
        display: inline-flex !important;
        align-items: center;
        gap: 6px;
        padding: 2px 6px;
        border-radius: 999px;
        border: 1px solid #1f2937;
        font-size: 12px;
        margin: 3px;
        vertical-align: middle;
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
    tags$script(HTML(
      r"(
      (function () {
        document.addEventListener('mouseup', function () {
          var viewer = document.getElementById('viewer');
          if (!viewer) return;
      
          var root = document.getElementById('viewer_html') || viewer;

          var sel = window.getSelection();
          if (!sel || sel.rangeCount === 0) return;

          var range = sel.getRangeAt(0);
          if (!root.contains(range.startContainer) || !root.contains(range.endContainer)) return;

          var txt = sel.toString();
          if (!txt || !txt.length || !txt.trim()) return;
      
          function closestSeg(node) {
            while (node && node !== root) {
              if (node.nodeType === 1 && node.classList && node.classList.contains('seg')) return node;
              node = node.parentNode;
            }
            return null;
          }
      
          function offsetWithinSeg(seg, container, offset) {
            var r = document.createRange();
            r.selectNodeContents(seg);
            r.setEnd(container, offset);
            return r.toString().length;
          }
      
          var startSeg = closestSeg(range.startContainer);
          var endSeg = closestSeg(range.endContainer);
      
          if (!startSeg || !endSeg) {
            var pre = range.cloneRange();
            pre.selectNodeContents(root);
            pre.setEnd(range.startContainer, range.startOffset);
            var startFallback = pre.toString().length;
      
            var pre2 = range.cloneRange();
            pre2.selectNodeContents(root);
            pre2.setEnd(range.endContainer, range.endOffset);
            var endFallback = pre2.toString().length;
      
            var leading = (txt.match(/^\s+/) || [''])[0].length;
            var trailing = (txt.match(/\s+$/) || [''])[0].length;

            var start2 = startFallback + leading;
            var end2 = endFallback - trailing;
            var txt2 = txt.trim();
      
            if (end2 <= start2) {
              try { sel.removeAllRanges(); } catch (e) {}
              return;
            }

            Shiny.setInputValue(
              'selection',
              { text: txt2, start: start2, end: end2, nonce: Math.random() },
              { priority: 'event' }
            );

            try { sel.removeAllRanges(); } catch (e) {}
            return;
          }
      
          var segs = root.querySelectorAll('.seg');
          var start = 0;
          var end = 0;
      
          for (var i = 0; i < segs.length; i++) {
            var s = segs[i];
            if (s === startSeg) {
              start += offsetWithinSeg(startSeg, range.startContainer, range.startOffset);
              break;
            }
            start += s.textContent.length;
          }

          for (var j = 0; j < segs.length; j++) {
            var s2 = segs[j];
            if (s2 === endSeg) {
              end += offsetWithinSeg(endSeg, range.endContainer, range.endOffset);
              break;
            }
            end += s2.textContent.length;
          }

          // Trim while keeping offsets aligned
          var leading2 = (txt.match(/^\s+/) || [''])[0].length;
          var trailing2 = (txt.match(/\s+$/) || [''])[0].length;

          var start2b = start + leading2;
          var end2b = end - trailing2;
          var txt2b = txt.trim();

          if (end2b <= start2b) {
            try { sel.removeAllRanges(); } catch (e) {}
            return;
          }

          Shiny.setInputValue(
            'selection',
            { text: txt2b, start: start2b, end: end2b, nonce: Math.random() },
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
          div(id = "viewer",
              uiOutput("viewer_html")
          ),
          br(),
          h5("Current spans (this row / document)"),
          uiOutput("spans_ui"),
          hr(),
          h5("Export / import"),
          downloadButton("dl_jsonl", "Export JSONL (all docs/rows)"),
          fileInput("import_file", "Import JSON/JSONL (restore saved state)",
                    accept = c(".json", ".jsonl")),
          actionButton("clear_spans", "Clear spans for current doc/row", class = "btn-danger"),
          tags$small(class = "text-muted",
                     "JSONL export: one line per document/row, with 0-based [start, end) offsets.")
      )
    )
  )
)
