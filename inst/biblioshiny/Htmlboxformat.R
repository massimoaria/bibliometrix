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

  # Header
  header_html <- paste0(
    "<thead><tr style='cursor: pointer;'>",
    paste0(
      sprintf(
        '<th class="sortable-header" data-col="%d" style="%s background-color: #f8f9fa; border-bottom: 2px solid #dee2e6; padding: 12px 25px 12px 10px; position: relative; white-space: nowrap;">%s <span class="sort-icon" style="color: #ccc; font-size: 0.9em; position: absolute; right: 8px; top: 50%%; transform: translateY(-50%%);">⇅</span></th>',
        0:(ncol(df) - 1),
        sapply(1:ncol(df), get_align),
        col_names
      ),
      collapse = ""
    ),
    "</tr>"
  )

  if (filter == "top") {
    filter_html <- paste0(
      '<tr class="filter-row">',
      paste0(
        sprintf(
          '<td><input type="text" class="form-control table-filter-%s" data-col="%d" placeholder="Filter..." style="width:100%%; font-size:11px; height:24px; padding:2px 5px;"></td>',
          table_id,
          0:(ncol(df) - 1)
        ),
        collapse = ""
      ),
      "</tr>"
    )
    header_html <- paste0(header_html, filter_html)
  }
  header_html <- paste0(header_html, "</thead>")

  # Body
  body_rows <- apply(df, 1, function(row) {
    cells <- sapply(1:length(row), function(i) {
      val <- row[i]
      is_num <- i %in% numeric
      sort_val <- gsub("<.*?>", "", as.character(val))
      if (is_num) {
        sort_val <- as.numeric(sort_val)
      }
      display_val <- if (is_num) {
        format(round(as.numeric(sort_val), round), nsmall = round)
      } else {
        val
      }
      sprintf(
        '<td data-sort="%s" style="%s padding: 8px; border-bottom: 1px solid #eee;">%s</td>',
        sort_val,
        get_align(i),
        display_val
      )
    })
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  })
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
          // We look for rows that do NOT have the "filtered-out" logic applied by JS
          const allRows = table.querySelectorAll("tbody tr");
          allRows.forEach(row => {
            // Se la riga ha un attributo custom o classe che indica che è filtrata, la saltiamo.
            // Nella nostra logica di updateTable, le righe filtrate non hanno display "" ma rimangono nascoste.
            // Tuttavia, usiamo la variabile visibleRows definita nello scope della tabella se possibile, 
            // ma qui siamo in una funzione globale. Quindi controlliamo una proprietà della riga.
            
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


#' Crea HTML della tabella
#' @keywords internal
create_html_table <- function(
  df,
  table_id,
  col_align,
  size,
  columnShort,
  title,
  filter,
  button,
  filename,
  scrollX,
  scrollY
) {
  col_names <- names(df)
  n_cols <- length(col_names)

  # Header con titolo
  title_html <- if (nchar(title) > 0) {
    sprintf(
      '<div class="table-title" style="text-align: center; font-size: 140%%; font-weight: bold; margin-bottom: 10px;">%s</div>',
      title
    )
  } else {
    ""
  }

  # Toolbar con bottoni
  toolbar_html <- ""
  if (button || filter != "none") {
    toolbar_items <- c()

    if (button) {
      toolbar_items <- c(
        toolbar_items,
        sprintf(
          '<button class="btn-export" onclick="exportTableToExcel(\'%s\', \'%s\')">
          <i class="fa fa-file-excel-o"></i> Export Excel
        </button>',
          table_id,
          filename
        )
      )
    }

    toolbar_html <- sprintf(
      '<div class="table-toolbar">%s</div>',
      paste(toolbar_items, collapse = "\n")
    )
  }

  # Header della tabella
  header_html <- "<thead><tr>"

  # Filtri superiori
  if (filter == "top") {
    filter_html <- "<tr class='filter-row'>"
    for (i in seq_along(col_names)) {
      filter_html <- paste0(
        filter_html,
        sprintf(
          '<th><input type="text" class="column-filter" placeholder="Search %s" data-col="%d"></th>',
          col_names[i],
          i - 1
        )
      )
    }
    filter_html <- paste0(filter_html, "</tr>")
  } else {
    filter_html <- ""
  }

  # Nomi colonne
  for (col_name in col_names) {
    header_html <- paste0(
      header_html,
      sprintf(
        '<th class="sortable" style="cursor: pointer;">%s <span class="sort-icon"></span></th>',
        col_name
      )
    )
  }
  header_html <- paste0(header_html, "</tr>", filter_html, "</thead>")

  # Body della tabella
  body_html <- "<tbody>"
  for (i in 1:nrow(df)) {
    body_html <- paste0(body_html, "<tr>")
    for (j in 1:n_cols) {
      cell_value <- as.character(df[i, j])

      # Gestione valori NA
      if (is.na(cell_value) || cell_value == "NA") {
        cell_value <- ""
      }

      # Troncamento colonne lunghe
      if (!is.null(columnShort) && j %in% columnShort) {
        if (nchar(cell_value) > 500) {
          cell_value <- sprintf(
            '<span title="%s">%s...</span>',
            cell_value,
            substr(cell_value, 1, 500)
          )
        }
      }

      body_html <- paste0(
        body_html,
        sprintf('<td style="text-align: %s;">%s</td>', col_align[j], cell_value)
      )
    }
    body_html <- paste0(body_html, "</tr>")
  }
  body_html <- paste0(body_html, "</tbody>")

  # Pagination footer
  pagination_html <- sprintf(
    '<div class="table-pagination">
      <div class="pagination-info">
        Showing <span id="%s-start">1</span> to <span id="%s-end">10</span> of <span id="%s-total">%d</span> entries
      </div>
      <div class="pagination-controls">
        <button class="btn-page" onclick="changePage(\'%s\', \'first\')">First</button>
        <button class="btn-page" onclick="changePage(\'%s\', \'prev\')">Previous</button>
        <span id="%s-page-info">Page <span id="%s-current-page">1</span> of <span id="%s-total-pages">1</span></span>
        <button class="btn-page" onclick="changePage(\'%s\', \'next\')">Next</button>
        <button class="btn-page" onclick="changePage(\'%s\', \'last\')">Last</button>
      </div>
      <div class="page-length">
        Show 
        <select id="%s-page-length" onchange="changePageLength(\'%s\')">
          <option value="10">10</option>
          <option value="25">25</option>
          <option value="50">50</option>
          <option value="-1">All</option>
        </select>
        entries
      </div>
    </div>',
    table_id,
    table_id,
    table_id,
    nrow(df),
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id
  )

  # Filtri inferiori
  if (filter == "bottom") {
    filter_html <- "<tfoot><tr class='filter-row'>"
    for (i in seq_along(col_names)) {
      filter_html <- paste0(
        filter_html,
        sprintf(
          '<th><input type="text" class="column-filter" placeholder="Search %s" data-col="%d"></th>',
          col_names[i],
          i - 1
        )
      )
    }
    filter_html <- paste0(filter_html, "</tr></tfoot>")
  } else if (filter != "top") {
    filter_html <- ""
  }

  # Assembla tutto
  scroll_style <- ""
  if (scrollX) {
    scroll_style <- paste0(scroll_style, "overflow-x: auto; ")
  }
  if (scrollY) {
    scroll_style <- paste0(
      scroll_style,
      "max-height: 500px; overflow-y: auto; "
    )
  }

  full_html <- sprintf(
    '<div class="htmlbox-container" id="%s-container">
      %s
      %s
      <div class="table-wrapper" style="%s">
        <table id="%s" class="htmlbox-table">
          %s
          %s
          %s
        </table>
      </div>
      %s
    </div>',
    table_id,
    title_html,
    toolbar_html,
    scroll_style,
    table_id,
    header_html,
    body_html,
    if (filter == "bottom") filter_html else "",
    pagination_html
  )

  return(full_html)
}

#' Create CSS for the table
#' @keywords internal
create_table_css <- function(size) {
  css <- sprintf(
    '
<style>
.htmlbox-container {
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
  font-size: %s;
  margin: 20px 0;
}

.table-title {
  color: #333;
  padding: 10px 0;
}

.table-toolbar {
  display: flex;
  justify-content: flex-end;
  margin-bottom: 10px;
  gap: 10px;
}

.btn-export {
  padding: 6px 12px;
  background-color: #4CAF50;
  color: white;
  border: none;
  border-radius: 4px;
  cursor: pointer;
  font-size: 14px;
  transition: background-color 0.3s;
}

.btn-export:hover {
  background-color: #45a049;
}

.table-wrapper {
  background: white;
  border-radius: 4px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

.htmlbox-table {
  width: 100%%;
  border-collapse: collapse;
  background-color: white;
}

.htmlbox-table thead {
  background-color: #f8f9fa;
  position: sticky;
  top: 0;
  z-index: 10;
}

.htmlbox-table th {
  padding: 12px 8px;
  text-align: center;
  font-weight: 600;
  border-bottom: 2px solid #dee2e6;
  color: #495057;
  user-select: none;
}

.htmlbox-table th.sortable:hover {
  background-color: #e9ecef;
}

.sort-icon {
  font-size: 0.8em;
  margin-left: 5px;
  opacity: 0.5;
}

.sort-icon.asc::after {
  content: "â–²";
  opacity: 1;
}

.sort-icon.desc::after {
  content: "â–¼";
  opacity: 1;
}

.htmlbox-table td {
  padding: 10px 8px;
  border-bottom: 1px solid #dee2e6;
  color: #212529;
}

.htmlbox-table tbody tr:hover {
  background-color: #f8f9fa;
}

.htmlbox-table tbody tr:nth-child(even) {
  background-color: #f9f9f9;
}

.filter-row input {
  width: 100%%;
  padding: 4px 8px;
  border: 1px solid #ced4da;
  border-radius: 3px;
  font-size: 0.9em;
}

.filter-row input:focus {
  outline: none;
  border-color: #80bdff;
  box-shadow: 0 0 0 0.2rem rgba(0,123,255,.25);
}

.table-pagination {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 15px 10px;
  background-color: #f8f9fa;
  border-top: 1px solid #dee2e6;
  flex-wrap: wrap;
  gap: 10px;
}

.pagination-info {
  color: #6c757d;
  font-size: 0.9em;
}

.pagination-controls {
  display: flex;
  gap: 5px;
  align-items: center;
}

.pagination-controls span {
  margin: 0 10px;
  font-size: 0.9em;
  color: #6c757d;
}

.btn-page {
  padding: 5px 10px;
  background-color: white;
  border: 1px solid #dee2e6;
  border-radius: 3px;
  cursor: pointer;
  font-size: 0.9em;
  transition: all 0.2s;
}

.btn-page:hover:not(:disabled) {
  background-color: #007bff;
  color: white;
  border-color: #007bff;
}

.btn-page:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.page-length {
  display: flex;
  align-items: center;
  gap: 5px;
  font-size: 0.9em;
  color: #6c757d;
}

.page-length select {
  padding: 4px 8px;
  border: 1px solid #ced4da;
  border-radius: 3px;
  font-size: 0.9em;
}

/* Responsive */
@media (max-width: 768px) {
  .table-pagination {
    flex-direction: column;
  }
  
  .pagination-controls {
    order: 1;
  }
  
  .pagination-info {
    order: 2;
  }
  
  .page-length {
    order: 3;
  }
}
</style>
',
    size,
    "table_id"
  )

  return(css)
}

#' Create JavaScript for interactivity
#' @keywords internal
create_table_javascript <- function(
  table_id,
  nrow,
  pagelength,
  filter,
  scrollX,
  scrollY
) {
  js <- sprintf(
    '
<script>
(function() {
  let tableData_%s = {
    currentPage: 1,
    pageLength: %d,
    sortColumn: -1,
    sortDirection: "asc",
    filterValues: {},
    originalData: [],
    filteredData: []
  };
  
  // Inizializza la tabella
  function initTable_%s() {
    const table = document.getElementById("%s");
    if (!table) return;
    
    const rows = Array.from(table.querySelectorAll("tbody tr"));
    tableData_%s.originalData = rows.map(row => {
      return Array.from(row.querySelectorAll("td")).map(cell => cell.textContent.trim());
    });
    tableData_%s.filteredData = [...tableData_%s.originalData];
    
    setupSorting_%s();
    setupFiltering_%s();
    updateTable_%s();
  }
  
  // Setup ordinamento
  function setupSorting_%s() {
    const headers = document.querySelectorAll("#%s thead th.sortable");
    headers.forEach((header, index) => {
      header.addEventListener("click", function() {
        sortTable_%s(index);
      });
    });
  }
  
  // Ordinamento tabella
  function sortTable_%s(colIndex) {
    if (tableData_%s.sortColumn === colIndex) {
      tableData_%s.sortDirection = tableData_%s.sortDirection === "asc" ? "desc" : "asc";
    } else {
      tableData_%s.sortColumn = colIndex;
      tableData_%s.sortDirection = "asc";
    }
    
    tableData_%s.filteredData.sort((a, b) => {
      let aVal = a[colIndex];
      let bVal = b[colIndex];
      
      // Prova conversione numerica
      const aNum = parseFloat(aVal);
      const bNum = parseFloat(bVal);
      
      if (!isNaN(aNum) && !isNaN(bNum)) {
        return tableData_%s.sortDirection === "asc" ? aNum - bNum : bNum - aNum;
      }
      
      // Confronto stringa
      if (tableData_%s.sortDirection === "asc") {
        return aVal.localeCompare(bVal);
      } else {
        return bVal.localeCompare(aVal);
      }
    });
    
    // Aggiorna icone ordinamento
    const headers = document.querySelectorAll("#%s thead th.sortable .sort-icon");
    headers.forEach((icon, idx) => {
      icon.className = "sort-icon";
      if (idx === colIndex) {
        icon.classList.add(tableData_%s.sortDirection);
      }
    });
    
    tableData_%s.currentPage = 1;
    updateTable_%s();
  }
  
  // Setup filtri
  function setupFiltering_%s() {
    const filters = document.querySelectorAll("#%s .column-filter");
    filters.forEach(filter => {
      filter.addEventListener("input", function() {
        const colIndex = parseInt(this.getAttribute("data-col"));
        tableData_%s.filterValues[colIndex] = this.value.toLowerCase();
        applyFilters_%s();
      });
    });
  }
  
  // Applica filtri
  function applyFilters_%s() {
    tableData_%s.filteredData = tableData_%s.originalData.filter(row => {
      return Object.entries(tableData_%s.filterValues).every(([col, value]) => {
        if (!value) return true;
        return row[parseInt(col)].toLowerCase().includes(value);
      });
    });
    
    tableData_%s.currentPage = 1;
    updateTable_%s();
  }
  
  // Aggiorna visualizzazione tabella
  function updateTable_%s() {
    const table = document.getElementById("%s");
    const tbody = table.querySelector("tbody");
    const pageLength = tableData_%s.pageLength;
    const totalRows = tableData_%s.filteredData.length;
    const totalPages = pageLength === -1 ? 1 : Math.ceil(totalRows / pageLength);
    
    // Assicura che currentPage sia valida
    if (tableData_%s.currentPage > totalPages) {
      tableData_%s.currentPage = Math.max(1, totalPages);
    }
    
    const startIdx = pageLength === -1 ? 0 : (tableData_%s.currentPage - 1) * pageLength;
    const endIdx = pageLength === -1 ? totalRows : Math.min(startIdx + pageLength, totalRows);
    
    // Svuota tbody
    tbody.innerHTML = "";
    
    // Aggiungi righe visibili
    for (let i = startIdx; i < endIdx; i++) {
      const row = document.createElement("tr");
      tableData_%s.filteredData[i].forEach((cellData, colIdx) => {
        const cell = document.createElement("td");
        cell.innerHTML = cellData;
        // Applica allineamento dalla tabella originale
        const originalCell = table.querySelectorAll("tbody tr")[0]?.querySelectorAll("td")[colIdx];
        if (originalCell) {
          cell.style.textAlign = window.getComputedStyle(originalCell).textAlign;
        }
        row.appendChild(cell);
      });
      tbody.appendChild(row);
    }
    
    // Aggiorna info paginazione
    document.getElementById("%s-start").textContent = totalRows === 0 ? 0 : startIdx + 1;
    document.getElementById("%s-end").textContent = endIdx;
    document.getElementById("%s-total").textContent = totalRows;
    document.getElementById("%s-current-page").textContent = tableData_%s.currentPage;
    document.getElementById("%s-total-pages").textContent = totalPages;
    
    // Aggiorna stato bottoni
    const btnFirst = document.querySelector(".btn-page[onclick*=\'first\']");
    const btnPrev = document.querySelector(".btn-page[onclick*=\'prev\']");
    const btnNext = document.querySelector(".btn-page[onclick*=\'next\']");
    const btnLast = document.querySelector(".btn-page[onclick*=\'last\']");
    
    if (btnFirst) btnFirst.disabled = tableData_%s.currentPage === 1;
    if (btnPrev) btnPrev.disabled = tableData_%s.currentPage === 1;
    if (btnNext) btnNext.disabled = tableData_%s.currentPage === totalPages;
    if (btnLast) btnLast.disabled = tableData_%s.currentPage === totalPages;
  }
  
  // Cambia pagina
  window.changePage = function(tableId, direction) {
    if (tableId !== "%s") return;
    
    const pageLength = tableData_%s.pageLength;
    const totalRows = tableData_%s.filteredData.length;
    const totalPages = pageLength === -1 ? 1 : Math.ceil(totalRows / pageLength);
    
    switch(direction) {
      case "first":
        tableData_%s.currentPage = 1;
        break;
      case "prev":
        tableData_%s.currentPage = Math.max(1, tableData_%s.currentPage - 1);
        break;
      case "next":
        tableData_%s.currentPage = Math.min(totalPages, tableData_%s.currentPage + 1);
        break;
      case "last":
        tableData_%s.currentPage = totalPages;
        break;
    }
    
    updateTable_%s();
  };
  
  // Cambia lunghezza pagina
  window.changePageLength = function(tableId) {
    if (tableId !== "%s") return;
    
    const select = document.getElementById(tableId + "-page-length");
    tableData_%s.pageLength = parseInt(select.value);
    tableData_%s.currentPage = 1;
    updateTable_%s();
  };
  
  // Export Excel
  window.exportTableToExcel = function(tableId, filename) {
    const table = document.getElementById(tableId);
    const wb = XLSX.utils.table_to_book(table, {sheet: "Sheet1"});
    const wbout = XLSX.write(wb, {bookType: "xlsx", type: "binary"});
    
    function s2ab(s) {
      const buf = new ArrayBuffer(s.length);
      const view = new Uint8Array(buf);
      for (let i=0; i<s.length; i++) view[i] = s.charCodeAt(i) & 0xFF;
      return buf;
    }
    
    const blob = new Blob([s2ab(wbout)], {type: "application/octet-stream"});
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = filename + "_" + new Date().toISOString().split("T")[0] + ".xlsx";
    a.click();
    URL.revokeObjectURL(url);
  };
  
// Esecuzione immediata per Shiny renderUI
  setTimeout(function() {
    initTable_%s();
  }, 100);
})();
</script>
',
    table_id,
    nrow,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id,
    table_id
  )

  return(js)
}
