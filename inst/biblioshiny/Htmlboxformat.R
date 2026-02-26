## Wrapper function to render bibliobox in Shiny
renderBibliobox <- function(df, title = "", ...) {
  renderUI({
    req(df)
    tryCatch(
      {
        data <- if (is.reactive(df)) df() else df
        HTML(htmlBoxFormat(data, title = title, ...))
      },
      error = function(e) {
        tags$div(
          class = "alert alert-danger",
          paste("Render error:", e$message)
        )
      }
    )
  })
}

#' HTML Box Format with Filter-Aware Excel Export
htmlBoxFormat <- function(
  df,
  nrow = 10,
  filename = "Table",
  pagelength = TRUE,
  left = NULL,
  right = NULL,
  numeric = NULL,
  dom = TRUE,
  size = "85%",
  filter = "top",
  columnShort = NULL,
  columnSmall = NULL,
  round = 2,
  title = "",
  button = FALSE,
  escape = FALSE,
  selection = FALSE,
  scrollX = FALSE,
  scrollY = FALSE,
  summary = FALSE
) {
  if (is.null(df) || (is.data.frame(df) && nrow(df) == 0)) {
    return(as.character(tags$div(
      class = "alert alert-info",
      "No data available"
    )))
  }

  # CORREZIONE: Gestisci pagelength prima di usare nrow nello sprintf
  if (is.logical(pagelength) && !pagelength) {
    nrow <- nrow(df) # Mostra tutte le righe
  } else if (is.numeric(pagelength)) {
    nrow <- as.integer(pagelength)
  }
  nrow <- as.integer(nrow) # Assicura che sia sempre un intero

  table_id <- sprintf(
    "htmlbox_%s",
    paste(sample(c(0:9, letters), 8, replace = TRUE), collapse = "")
  )
  col_names <- colnames(df)

  get_align <- function(i) {
    if (i %in% left) {
      return("text-align: left;")
    }
    if (i %in% right) {
      return("text-align: right;")
    }
    if (i %in% numeric) {
      return("text-align: right;")
    }
    return("text-align: left;")
  }

  # Determine if summary button column is needed
  has_summary <- is.character(summary) && summary == "documents"

  # Header
  header_cols <- paste0(
    sprintf(
      '<th class="sortable-header" data-col="%d" style="%s background-color: #f8f9fa; border-bottom: 2px solid #dee2e6; padding: 12px 25px 12px 10px; position: relative; white-space: nowrap;">%s <span class="sort-icon" style="color: #ccc; font-size: 0.9em; position: absolute; right: 8px; top: 50%%; transform: translateY(-50%%);">⇅</span></th>',
      0:(ncol(df) - 1),
      sapply(1:ncol(df), get_align),
      col_names
    ),
    collapse = ""
  )
  if (has_summary) {
    header_cols <- paste0(
      header_cols,
      '<th style="background-color: #f8f9fa; border-bottom: 2px solid #dee2e6; padding: 12px 10px; text-align: center; white-space: nowrap;">AI Summary</th>'
    )
  }
  header_html <- paste0("<thead><tr style='cursor: pointer;'>", header_cols, "</tr>")

  if (filter == "top") {
    filter_cells <- paste0(
      sprintf(
        '<td><input type="text" class="form-control table-filter-%s" data-col="%d" placeholder="Filter..." style="width:100%%; font-size:11px; height:24px; padding:2px 5px;"></td>',
        table_id,
        0:(ncol(df) - 1)
      ),
      collapse = ""
    )
    if (has_summary) {
      filter_cells <- paste0(filter_cells, "<td></td>")
    }
    filter_html <- paste0('<tr class="filter-row">', filter_cells, "</tr>")
    header_html <- paste0(header_html, filter_html)
  }
  header_html <- paste0(header_html, "</thead>")

  # Body: prepare data as JSON for client-side rendering (only visible page in DOM)
  n_cols <- ncol(df)
  aligns <- sapply(1:n_cols, get_align)

  # Build column-oriented vectors (fast R serialization, reconstruct rows in JS)
  sort_cols <- vector("list", n_cols)
  disp_cols <- vector("list", n_cols)
  for (j in 1:n_cols) {
    vals <- as.character(df[[j]])
    sort_vals <- gsub("<.*?>", "", vals)
    if (j %in% numeric) {
      num_vals <- as.numeric(sort_vals)
      disp_cols[[j]] <- format(round(num_vals, round), nsmall = round)
      sort_cols[[j]] <- as.character(num_vals)
    } else {
      disp_cols[[j]] <- vals
      sort_cols[[j]] <- sort_vals
    }
  }

  # Column-oriented JSON: {s: [[col1_vals], [col2_vals], ...], d: [[col1_vals], [col2_vals], ...], n: nrow}
  # jsonlite serializes character vectors very efficiently in C — no nested R list overhead
  data_json <- jsonlite::toJSON(
    list(s = sort_cols, d = disp_cols, n = nrow(df)),
    auto_unbox = TRUE
  )

  # UI Structure — empty tbody, data delivered via JSON script tag
  aligns_json <- jsonlite::toJSON(aligns, auto_unbox = TRUE)

  html_out <- tagList(
    tags$script(
      src = "https://cdn.jsdelivr.net/npm/xlsx@0.18.5/dist/xlsx.full.min.js"
    ),

    tags$div(
      id = paste0("container_", table_id),
      style = sprintf(
        "font-size: %s; background: white; border: 1px solid #ddd; border-radius: 4px; margin-bottom: 20px; font-family: sans-serif;",
        size
      ),

      tags$div(
        style = "padding: 10px 15px; background: #fdfdfd; border-bottom: 1px solid #eee; display: flex; justify-content: space-between; align-items: center;",
        tags$strong(title, style = "font-size: 1.1em; color: #333;"),
        if (button) {
          tags$button(
            "Excel",
            class = "btn btn-xs btn-success",
            onclick = sprintf("exportToExcel('%s', '%s')", table_id, filename)
          )
        } else {
          ""
        }
      ),

      tags$div(
        style = if (scrollX) "overflow-x: auto;" else "",
        tags$table(
          id = table_id,
          class = "table table-hover",
          style = "width: 100%; margin-bottom: 0; table-layout: auto;",
          HTML(header_html),
          HTML("<tbody></tbody>")
        )
      ),

      tags$div(
        style = "padding: 10px; border-top: 1px solid #eee; display: flex; justify-content: space-between; align-items: center;",
        tags$div(
          id = paste0("info_", table_id),
          style = "color: #777; font-size: 12px;"
        ),
        tags$div(class = "btn-group", id = paste0("pager_", table_id))
      )
    ),

    # Embed data as JSON in a script tag (not executed, just storage)
    tags$script(
      id = paste0("data_", table_id),
      type = "application/json",
      HTML(as.character(data_json))
    ),

    tags$script(HTML(sprintf(
      '
      if (typeof window.exportToExcel !== "function") {
        window.exportToExcel = function(tid, fname) {
          var state = window["__bb_" + tid];
          if (!state) return;

          var headers = [];
          var table = document.getElementById(tid);
          var headerCells = table.querySelectorAll("thead tr:first-child th");
          headerCells.forEach(function(th) { headers.push(th.innerText.replace("\\u21C5", "").trim()); });

          var data = [headers];
          var rows = state.filteredData;
          for (var i = 0; i < rows.length; i++) {
            var rowData = [];
            for (var j = 0; j < rows[i].length; j++) {
              var val = rows[i][j].s;
              if (!isNaN(val) && val !== "") val = parseFloat(val);
              rowData.push(val);
            }
            data.push(rowData);
          }

          var ws = XLSX.utils.aoa_to_sheet(data);
          var wb = XLSX.utils.book_new();
          XLSX.utils.book_append_sheet(wb, ws, "Data");
          XLSX.writeFile(wb, fname + "_" + new Date().toISOString().split("T")[0] + ".xlsx");
        };
      }

      (function() {
        var tid = "%s";
        var pageSize = %d;
        var hasSummary = %s;
        var aligns = %s;

        function init() {
          var table = document.getElementById(tid);
          if (!table) { setTimeout(init, 100); return; }

          var dataEl = document.getElementById("data_" + tid);
          var raw = JSON.parse(dataEl.textContent);
          // Transform column-oriented {s:[[col1],[col2],...], d:[[col1],[col2],...], n:nrow}
          // into row-oriented [{s:sortVal, d:dispVal}, ...] per row — fast in JS
          var nCols = raw.s.length;
          var allData = new Array(raw.n);
          for (var r = 0; r < raw.n; r++) {
            var row = new Array(nCols);
            for (var c = 0; c < nCols; c++) {
              row[c] = {s: raw.s[c][r], d: raw.d[c][r]};
            }
            allData[r] = row;
          }
          raw = null; // free column-oriented data
          var filteredData = allData.slice();
          var currentPage = 0;
          var sortCol = -1;
          var sortAsc = true;
          var tbody = table.querySelector("tbody");

          // Expose state for Excel export
          window["__bb_" + tid] = { filteredData: filteredData };

          function renderPage() {
            var start = currentPage * pageSize;
            var end = Math.min(start + pageSize, filteredData.length);
            tbody.innerHTML = "";
            for (var i = start; i < end; i++) {
              var row = filteredData[i];
              var tr = document.createElement("tr");
              for (var j = 0; j < row.length; j++) {
                var td = document.createElement("td");
                td.setAttribute("data-sort", row[j].s);
                td.style.cssText = aligns[j] + " padding: 8px; border-bottom: 1px solid #eee;";
                td.innerHTML = row[j].d;
                tr.appendChild(td);
              }
              if (hasSummary) {
                var tdS = document.createElement("td");
                tdS.style.cssText = "text-align: center; padding: 8px; border-bottom: 1px solid #eee;";
                var safeVal = row[0].s.replace(/\x27/g, "\\\x27");
                tdS.innerHTML = "<button class=\\"btn btn-xs btn-info\\" onclick=\\"Shiny.setInputValue(\\x27button_id\\x27, \\x27" + safeVal + "\\x27, {priority: \\x27event\\x27})\\" title=\\"AI Summary\\"><i class=\\"fa fa-robot\\"></i></button>";
                tr.appendChild(tdS);
              }
              tbody.appendChild(tr);
            }
            document.getElementById("info_" + tid).innerText =
              "Showing " + (filteredData.length > 0 ? start + 1 : 0) +
              "-" + end + " of " + filteredData.length;
            renderPager();
          }

          function renderPager() {
            var pager = document.getElementById("pager_" + tid);
            var pageCount = Math.ceil(filteredData.length / pageSize);
            pager.innerHTML = "";
            if (pageCount <= 1) return;
            var createBtn = function(label, target, active, disabled) {
              var b = document.createElement("button");
              b.className = "btn btn-default btn-xs" + (active ? " active" : "");
              b.innerText = label;
              b.disabled = disabled;
              b.onclick = function() { currentPage = target; renderPage(); };
              pager.appendChild(b);
            };
            createBtn("\\u00AB", Math.max(0, currentPage - 1), false, currentPage === 0);
            for (var i = 0; i < pageCount; i++) {
              if (i === 0 || i === pageCount - 1 || (i >= currentPage - 1 && i <= currentPage + 1)) {
                createBtn(i + 1, i, i === currentPage, false);
              } else if (i === currentPage - 2 || i === currentPage + 2) {
                var s = document.createElement("span");
                s.innerText = "...";
                s.style.padding = "0 5px";
                pager.appendChild(s);
              }
            }
            createBtn("\\u00BB", Math.min(pageCount - 1, currentPage + 1), false, currentPage === pageCount - 1);
          }

          table.querySelectorAll(".sortable-header").forEach(function(th) {
            th.onclick = function() {
              var col = parseInt(this.getAttribute("data-col"));
              sortAsc = (sortCol === col) ? !sortAsc : true;
              sortCol = col;
              table.querySelectorAll(".sort-icon").forEach(function(si) { si.innerText = "\\u21C5"; si.style.color = "#ccc"; });
              this.querySelector(".sort-icon").innerText = sortAsc ? "\\u25B2" : "\\u25BC";
              this.querySelector(".sort-icon").style.color = "#333";
              filteredData.sort(function(a, b) {
                var valA = a[col].s.trim();
                var valB = b[col].s.trim();
                if (!isNaN(valA) && !isNaN(valB) && valA !== "" && valB !== "") {
                  return sortAsc ? parseFloat(valA) - parseFloat(valB) : parseFloat(valB) - parseFloat(valA);
                }
                return sortAsc ? valA.localeCompare(valB) : valB.localeCompare(valA);
              });
              currentPage = 0;
              renderPage();
            };
          });

          var filters = document.querySelectorAll(".table-filter-" + tid);
          filters.forEach(function(input) {
            input.addEventListener("input", function() {
              var fVals = Array.from(filters).map(function(f) { return f.value.toUpperCase(); });
              filteredData = allData.filter(function(row) {
                return fVals.every(function(val, idx) {
                  if (!val) return true;
                  return row[idx].s.toUpperCase().indexOf(val) > -1;
                });
              });
              window["__bb_" + tid].filteredData = filteredData;
              currentPage = 0;
              renderPage();
            });
          });

          renderPage();
        }
        init();
      })();
      ',
      table_id,
      nrow,
      ifelse(has_summary, "true", "false"),
      as.character(aligns_json)
    )))
  )

  return(as.character(html_out))
}
