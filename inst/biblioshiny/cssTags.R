cssTags <- function(){
  ## workaround to solve visualization issues in Data Table
  tagList(
    tags$head(tags$style(HTML(".has-feedback .form-control { padding-right: 0px;}"))),
    ### animation for filter results box
    tags$head(tags$style(HTML("
  .fade-in {
    animation: fadeInAnim 0.8s ease-in-out;
  }

  @keyframes fadeInAnim {
    from { opacity: 0; transform: scale(0.98); }
    to { opacity: 1; transform: scale(1); }
  }
"))),
    ### css for citation matching
    tags$head(
      tags$style(HTML("
    .dataTables_wrapper .dataTable tbody tr.selected {
      background-color: #d4edda !important;
    }
    
    .dataTables_wrapper .dataTable tbody tr.selected td {
      font-weight: bold;
    }
    
    #refMatch_topCitations tbody tr:hover {
      background-color: #e9ecef;
      cursor: pointer;
    }
  "))
    ),
    tags$head(
      tags$style(HTML("
    /* Manual merge buttons styling */
    #refMatch_toggleSelection,
    #refMatch_clearSelection,
    #refMatch_confirmMerge {
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
    }
    
    /* Status inline box */
    #refMatch_selectionStatusInline {
      transition: background-color 0.3s ease;
    }
    
    /* Responsive adjustments */
    @media (max-width: 1200px) {
      #refMatch_toggleSelection,
      #refMatch_clearSelection,
      #refMatch_confirmMerge {
        font-size: 12px;
        padding: 6px 10px;
      }
    }
  "))
    ),
    tags$head(
      tags$style(HTML("
    /* Loading indicator animation */
    @keyframes pulse {
      0% { opacity: 1; }
      50% { opacity: 0.5; }
      100% { opacity: 1; }
    }
    
    #refMatch_loadingIndicator .box {
      animation: pulse 2s ease-in-out infinite;
    }
    
    #refMatch_loadingIndicator h4 {
      animation: pulse 1.5s ease-in-out infinite;
    }
  "))
    ),
    
    ### css for life cycle summary
    tags$style(shiny::HTML("
      .lifecycle-tabs .nav-tabs {
        border-bottom: 2px solid #dee2e6;
      }
      .lifecycle-tabs .nav-tabs .nav-link {
        color: #495057;
        font-weight: 500;
        padding: 12px 20px;
        border: none;
        border-bottom: 3px solid transparent;
        transition: all 0.3s ease;
      }
      .lifecycle-tabs .nav-tabs .nav-link:hover {
        color: #1f77b4;
        border-bottom-color: #1f77b4;
        background: transparent;
      }
      .lifecycle-tabs .nav-tabs .nav-link.active {
        color: #1f77b4;
        background: transparent;
        border-bottom-color: #1f77b4;
      }
      .lifecycle-plot-container {
        background: white;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        margin-bottom: 20px;
      }
    ")),
    shiny::tags$style(shiny::HTML("
      .lifecycle-summary {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
        color: #333;
      }
      .lifecycle-card {
        background: white;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        border-left: 4px solid #1f77b4;
      }
      .lifecycle-card.success {
        border-left-color: #2ca02c;
      }
      .lifecycle-card.warning {
        border-left-color: #ff7f0e;
      }
      .lifecycle-card.info {
        border-left-color: #9467bd;
      }
      .lifecycle-header {
        font-size: 18px;
        font-weight: 600;
        color: #1f77b4;
        margin-bottom: 15px;
        display: flex;
        align-items: center;
      }
      .lifecycle-header.success {
        color: #2ca02c;
      }
      .lifecycle-header.warning {
        color: #ff7f0e;
      }
      .lifecycle-header.info {
        color: #9467bd;
      }
      .lifecycle-header i {
        margin-right: 10px;
        font-size: 20px;
      }
      .lifecycle-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
        gap: 15px;
        margin-top: 15px;
      }
      .lifecycle-metric {
        background: #f8f9fa;
        padding: 12px;
        border-radius: 6px;
        text-align: center;
      }
      .lifecycle-metric-label {
        font-size: 12px;
        color: #666;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-bottom: 5px;
      }
      .lifecycle-metric-value {
        font-size: 24px;
        font-weight: 700;
        color: #1f77b4;
      }
      .lifecycle-metric-unit {
        font-size: 14px;
        color: #999;
        margin-left: 3px;
      }
      .lifecycle-row {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 10px 0;
        border-bottom: 1px solid #eee;
      }
      .lifecycle-row:last-child {
        border-bottom: none;
      }
      .lifecycle-row-label {
        font-weight: 500;
        color: #555;
      }
      .lifecycle-row-value {
        font-weight: 600;
        color: #1f77b4;
        font-size: 16px;
      }
      .lifecycle-badge {
        display: inline-block;
        padding: 4px 12px;
        border-radius: 12px;
        font-size: 12px;
        font-weight: 600;
        margin-left: 8px;
      }
      .lifecycle-badge.excellent {
        background: #d4edda;
        color: #155724;
      }
      .lifecycle-badge.good {
        background: #d1ecf1;
        color: #0c5460;
      }
      .lifecycle-badge.moderate {
        background: #fff3cd;
        color: #856404;
      }
      .lifecycle-progress {
        background: #e9ecef;
        border-radius: 10px;
        height: 20px;
        margin-top: 10px;
        overflow: hidden;
      }
      .lifecycle-progress-bar {
        background: linear-gradient(90deg, #1f77b4, #2ca02c);
        height: 100%;
        transition: width 0.3s ease;
        display: flex;
        align-items: center;
        justify-content: center;
        color: white;
        font-size: 12px;
        font-weight: 600;
      }
      .lifecycle-info-box {
        background: #e7f3ff;
        border-left: 3px solid #1f77b4;
        padding: 12px;
        border-radius: 4px;
        margin-top: 15px;
      }
      .lifecycle-info-box.warning {
        background: #fff4e6;
        border-left-color: #ff7f0e;
      }
    ")),
    
    ### css for author link
    tags$head(
      tags$style(HTML("
      .author-link {
        color: #337ab7;
        text-decoration: underline;
        cursor: pointer;
      }
      .author-link:hover {
        color: #23527c;
        font-weight: bold;
      }"
      ))
    ),
    ## script to open more times the same modal ####
    tags$script("
    Shiny.addCustomMessageHandler('button_id', function(value) {
    Shiny.setInputValue('button_id', value);
    });
  "),
    tags$script("
    Shiny.addCustomMessageHandler('selected_author', function(value) {
    Shiny.setInputValue('selected_author', value);
    });
  "),
    ## script to get the dimensions of the page ####
    ###
    tags$head(
      tags$style(".fa-cloud-arrow-down {font-size: 20px}"),
      tags$style(".fa-download {font-size: 20px}"),
      tags$style(".fa-envelope {color:#FF0000; font-size: 20px}"),
      tags$style(".fa-envelope-open {font-size: 20px}"),
      tags$style(".fa-cube {font-size: 20px}"),
      tags$style(".fa-question {font-size: 20px}"),
      tags$style(".fa-comment-dollar {font-size: 20px}"),
      tags$style(".fa-bars {font-size: 20px}"),
      tags$style(".sidebar-toggle {font-size: 15px}"),
      tags$script(
        'var dimension = [0, 0];
              $(document).on("shiny:connected", function(e) {
                  dimension[0] = window.innerWidth;
                  dimension[1] = window.innerHeight;
                  Shiny.onInputChange("dimension", dimension);
              });
              $(window).resize(function(e) {
                  dimension[0] = window.innerWidth;
                  dimension[1] = window.innerHeight;
                  Shiny.onInputChange("dimension", dimension);
              });
              $(document).ready(function(){
                  $("a[data-toggle=tab]").on("show.bs.tab", function(e){
                    Shiny.setInputValue("activeTab", $(this).attr("data-value"));
                   });
            });
      '
      )
    )
  )
}
