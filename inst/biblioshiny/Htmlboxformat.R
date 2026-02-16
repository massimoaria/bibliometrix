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

  # Body (vectorized column-by-column instead of row-by-row apply)
  n_cols <- ncol(df)
  aligns <- sapply(1:n_cols, get_align)
  cell_cols <- vector("list", n_cols)
  for (j in 1:n_cols) {
    vals <- as.character(df[[j]])
    sort_vals <- gsub("<.*?>", "", vals)
    if (j %in% numeric) {
      num_vals <- as.numeric(sort_vals)
      display_vals <- format(round(num_vals, round), nsmall = round)
      sort_vals <- as.character(num_vals)
    } else {
      display_vals <- vals
    }
    cell_cols[[j]] <- sprintf(
      '<td data-sort="%s" style="%s padding: 8px; border-bottom: 1px solid #eee;">%s</td>',
      sort_vals, aligns[j], display_vals
    )
  }
  row_contents <- do.call(paste0, cell_cols)

  if (has_summary) {
    sr_vals <- gsub("'", "\\\\'", as.character(df[[1]]))
    summary_cells <- sprintf(
      '<td style="text-align: center; padding: 8px; border-bottom: 1px solid #eee;"><button class="btn btn-xs btn-info" onclick="Shiny.setInputValue(\'button_id\', \'%s\', {priority: \'event\'})" title="AI Summary"><i class="fa fa-robot"></i></button></td>',
      sr_vals
    )
    row_contents <- paste0(row_contents, summary_cells)
  }

  body_rows <- paste0("<tr>", row_contents, "</tr>")
  tbody_html <- paste0("<tbody>", paste(body_rows, collapse = ""), "</tbody>")

  # UI Structure
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
          HTML(tbody_html)
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

    tags$script(HTML(sprintf(
      '
      if (typeof window.exportToExcel !== "function") {
        window.exportToExcel = function(tid, fname) {
          const table = document.getElementById(tid);
          if (!table) return;
          
          const data = [];
          const headers = [];
          
          // 1. Get headers (ignoring sorting icons)
          const headerCells = table.querySelectorAll("thead tr:first-child th");
          headerCells.forEach(th => headers.push(th.innerText.replace("⇅", "").trim()));
          data.push(headers);
          
          // 2. Get rows (exporting ALL or FILTERED rows)
          const allRows = table.querySelectorAll("tbody tr");
          allRows.forEach(row => {
            if (row.getAttribute("data-is-filtered") === "false") return;

            const rowData = [];
            Array.from(row.cells).forEach(cell => {
              let val = cell.getAttribute("data-sort") || cell.innerText;
              if (!isNaN(val) && val !== "") val = parseFloat(val);
              rowData.push(val);
            });
            data.push(rowData);
          });
          
          const ws = XLSX.utils.aoa_to_sheet(data);
          const wb = XLSX.utils.book_new();
          XLSX.utils.book_append_sheet(wb, ws, "Data");
          XLSX.writeFile(wb, fname + "_" + new Date().toISOString().split("T")[0] + ".xlsx");
        };
      }

      (function() {
        const tid = "%s";
        const pageSize = %d;
        let currentPage = 0;
        let visibleRows = [];
        let sortCol = -1;
        let sortAsc = true;

        function init() {
          const table = document.getElementById(tid);
          if (!table) { setTimeout(init, 100); return; }
          const tbody = table.querySelector("tbody");
          const allRows = Array.from(tbody.querySelectorAll("tr"));
          visibleRows = allRows;

          function updateTable() {
            const start = currentPage * pageSize;
            const end = start + pageSize;
            
            // Nascondi tutto fisicamente
            allRows.forEach(r => r.style.display = "none");
            
            // Mostra solo le righe della pagina corrente tra quelle filtrate
            visibleRows.slice(start, end).forEach(r => {
                r.style.display = "";
                tbody.appendChild(r); 
            });
            
            document.getElementById("info_" + tid).innerText = 
              "Showing " + (visibleRows.length > 0 ? start + 1 : 0) + 
              "-" + Math.min(end, visibleRows.length) + " of " + visibleRows.length;
            renderPager();
          }

          function renderPager() {
            const pager = document.getElementById("pager_" + tid);
            const pageCount = Math.ceil(visibleRows.length / pageSize);
            pager.innerHTML = "";
            if (pageCount <= 1 && visibleRows.length <= pageSize) return;
            const createBtn = (label, target, active = false, disabled = false) => {
              const b = document.createElement("button");
              b.className = "btn btn-default btn-xs" + (active ? " active" : "");
              b.innerText = label; b.disabled = disabled;
              b.onclick = () => { currentPage = target; updateTable(); };
              pager.appendChild(b);
            };
            createBtn("«", Math.max(0, currentPage - 1), false, currentPage === 0);
            for (let i = 0; i < pageCount; i++) {
              if (i === 0 || i === pageCount - 1 || (i >= currentPage - 1 && i <= currentPage + 1)) {
                createBtn(i + 1, i, i === currentPage);
              } else if (i === currentPage - 2 || i === currentPage + 2) {
                const s = document.createElement("span"); s.innerText = "..."; s.style.padding = "0 5px";
                pager.appendChild(s);
              }
            }
            createBtn("»", Math.min(pageCount - 1, currentPage + 1), false, currentPage === pageCount - 1);
          }

          table.querySelectorAll(".sortable-header").forEach(th => {
            th.onclick = function() {
              const col = parseInt(this.getAttribute("data-col"));
              sortAsc = (sortCol === col) ? !sortAsc : true;
              sortCol = col;
              table.querySelectorAll(".sort-icon").forEach(si => { si.innerText = "⇅"; si.style.color = "#ccc"; });
              this.querySelector(".sort-icon").innerText = sortAsc ? "▲" : "▼";
              this.querySelector(".sort-icon").style.color = "#333";
              visibleRows.sort((a, b) => {
                let valA = a.cells[col].getAttribute("data-sort").trim();
                let valB = b.cells[col].getAttribute("data-sort").trim();
                if (!isNaN(valA) && !isNaN(valB) && valA !== "" && valB !== "") {
                  return sortAsc ? parseFloat(valA) - parseFloat(valB) : parseFloat(valB) - parseFloat(valA);
                }
                return sortAsc ? valA.localeCompare(valB) : valB.localeCompare(valA);
              });
              currentPage = 0;
              updateTable();
            };
          });

          const filters = document.querySelectorAll(".table-filter-" + tid);
          filters.forEach(input => {
            input.addEventListener("input", () => {
              const fVals = Array.from(filters).map(f => f.value.toUpperCase());
              
              // Applica il filtro e segna le righe
              allRows.forEach(row => {
                const isMatch = fVals.every((val, idx) => {
                  if (!val) return true;
                  return row.cells[idx].getAttribute("data-sort").toUpperCase().includes(val);
                });
                row.setAttribute("data-is-filtered", isMatch ? "true" : "false");
              });

              visibleRows = allRows.filter(r => r.getAttribute("data-is-filtered") === "true");
              currentPage = 0;
              updateTable();
            });
          });
          
          // Segna inizialmente tutte le righe come "visibili" per export
          allRows.forEach(r => r.setAttribute("data-is-filtered", "true"));
          updateTable();
        }
        init();
      })();
      ',
      table_id,
      nrow
    )))
  )

  return(as.character(html_out))
}
