#' Create HTML Summary for Life Cycle Analysis Results
#' 
#' Generates a nicely formatted HTML output for lifeCycle results
#' suitable for Shiny renderUI
#' 
#' @param results Output from lifeCycle() function
#' 
#' @return HTML tag list for Shiny UI
#' 
lifeCycleSummaryUI <- function(results) {
  
 # Extract data
  params <- results$parameters_real_years
  metrics <- results$metrics
  df <- results$data
  forecast_df <- results$forecast
  
  # Calculate additional info
  last_obs_year <- max(df$PY)
  last_obs_annual <- tail(df$n, 1)
  last_obs_cumulative <- max(df$cumulative)
  current_pct <- (last_obs_cumulative / params$K) * 100
  
  # Years to reach milestones
  years_to_50 <- round(params$tm_year - last_obs_year,0)
  years_to_90 <- round(params$year_90_pct - last_obs_year,0)
  years_to_99 <- round(params$year_99_pct - last_obs_year,0)
  
  # Create the UI
  shiny::tagList(
    shiny::div(class = "lifecycle-summary",
               
               # === CARD 1: MODEL OVERVIEW ===
               shiny::div(class = "lifecycle-card",
                          shiny::div(class = "lifecycle-header",
                                     shiny::icon("chart-line"),
                                     "Model Overview"
                          ),
                          shiny::div(class = "lifecycle-grid",
                                     shiny::div(class = "lifecycle-metric",
                                                shiny::div(class = "lifecycle-metric-label", "Saturation (K)"),
                                                shiny::div(class = "lifecycle-metric-value",
                                                           format(round(params$K), big.mark = ","),
                                                           shiny::span(class = "lifecycle-metric-unit", "pubs")
                                                )
                                     ),
                                     shiny::div(class = "lifecycle-metric",
                                                shiny::div(class = "lifecycle-metric-label", "Peak Year (tₘ)"),
                                                shiny::div(class = "lifecycle-metric-value",
                                                           round(params$tm_year, 0)
                                                )
                                     ),
                                     shiny::div(class = "lifecycle-metric",
                                                shiny::div(class = "lifecycle-metric-label", "Peak Annual"),
                                                shiny::div(class = "lifecycle-metric-value",
                                                           format(round(params$peak_annual,0), big.mark = ","),
                                                           shiny::span(class = "lifecycle-metric-unit", "pubs/year")
                                                )
                                     ), # delta symbol for duration 
                                     
                                     shiny::div(class = "lifecycle-metric",
                                                shiny::div(class = "lifecycle-metric-label", "Growth Duration (Δₜ)"),
                                                shiny::div(class = "lifecycle-metric-value",
                                                           round(params$delta_t, 1),
                                                           shiny::span(class = "lifecycle-metric-unit", "years")
                                                )
                                     )
                          )
               ),
               
               # === CARD 2: MODEL FIT QUALITY ===
               shiny::div(class = "lifecycle-card success",
                          shiny::div(class = "lifecycle-header success",
                                     shiny::icon("check-circle"),
                                     "Model Fit Quality",
                                     if(metrics$R_squared >= 0.9) {
                                       shiny::span(class = "lifecycle-badge excellent", "Excellent")
                                     } else if(metrics$R_squared >= 0.75) {
                                       shiny::span(class = "lifecycle-badge good", "Good")
                                     } else {
                                       shiny::span(class = "lifecycle-badge moderate", "Moderate")
                                     }
                          ),
                          shiny::div(class = "lifecycle-grid",
                                     shiny::div(class = "lifecycle-metric",
                                                shiny::div(class = "lifecycle-metric-label", "R²"),
                                                shiny::div(class = "lifecycle-metric-value",
                                                           sprintf("%.3f", metrics$R_squared)
                                                )
                                     ),
                                     shiny::div(class = "lifecycle-metric",
                                                shiny::div(class = "lifecycle-metric-label", "RMSE"),
                                                shiny::div(class = "lifecycle-metric-value",
                                                           sprintf("%.2f", metrics$RMSE)
                                                )
                                     ),
                                     shiny::div(class = "lifecycle-metric",
                                                shiny::div(class = "lifecycle-metric-label", "AIC"),
                                                shiny::div(class = "lifecycle-metric-value",
                                                           sprintf("%.1f", metrics$AIC)
                                                )
                                     ),
                                     shiny::div(class = "lifecycle-metric",
                                                shiny::div(class = "lifecycle-metric-label", "BIC"),
                                                shiny::div(class = "lifecycle-metric-value",
                                                           sprintf("%.1f", metrics$BIC)
                                                )
                                     )
                          )
               ),
               
               # === CARD 3: CURRENT STATUS ===
               fluidRow(
                 column(4,
               
               shiny::div(class = "lifecycle-card info",
                          shiny::div(class = "lifecycle-header info",
                                     shiny::icon("info-circle"),
                                     "Current Status"
                          ),
                          shiny::div(
                            shiny::div(class = "lifecycle-row",
                                       shiny::div(class = "lifecycle-row-label", "Last Observed Year:"),
                                       shiny::div(class = "lifecycle-row-value", last_obs_year)
                            ),
                            shiny::div(class = "lifecycle-row",
                                       shiny::div(class = "lifecycle-row-label", "Annual Publications:"),
                                       shiny::div(class = "lifecycle-row-value", 
                                                  format(last_obs_annual, big.mark = ","))
                            ),
                            shiny::div(class = "lifecycle-row",
                                       shiny::div(class = "lifecycle-row-label", "Cumulative Total:"),
                                       shiny::div(class = "lifecycle-row-value", 
                                                  format(round(last_obs_cumulative), big.mark = ","))
                            ),
                            shiny::div(class = "lifecycle-row",
                                       shiny::div(class = "lifecycle-row-label", "Progress to Saturation:"),
                                       shiny::div(class = "lifecycle-row-value", 
                                                  sprintf("%.1f%%", current_pct))
                            )
                          ),
                          shiny::div(class = "lifecycle-progress",
                                     shiny::div(class = "lifecycle-progress-bar",
                                                style = sprintf("width: %.1f%%;", min(current_pct, 100)),
                                                sprintf("%.1f%%", current_pct)
                                     )
                          )
               )
               ),
               column(4,
               # === CARD 4: MILESTONE YEARS ===
               shiny::div(class = "lifecycle-card warning",
                          shiny::div(class = "lifecycle-header warning",
                                     shiny::icon("flag-checkered"),
                                     "Milestone Years"
                          ),
                          shiny::div(
                            shiny::div(class = "lifecycle-row",
                                       shiny::div(class = "lifecycle-row-label", "10% of K:"),
                                       shiny::div(class = "lifecycle-row-value", 
                                                  sprintf("%.1f", params$year_10_pct))
                            ),
                            shiny::div(class = "lifecycle-row",
                                       shiny::div(class = "lifecycle-row-label", "50% of K (Midpoint):"),
                                       shiny::div(class = "lifecycle-row-value", 
                                                  sprintf("%.1f", params$tm_year),
                                                  if(years_to_50 > 0) {
                                                    shiny::span(style = "font-size: 12px; color: #666; margin-left: 8px;",
                                                                sprintf("(%+.0f years)", years_to_50))
                                                  }
                                       )
                            ),
                            shiny::div(class = "lifecycle-row",
                                       shiny::div(class = "lifecycle-row-label", "90% of K:"),
                                       shiny::div(class = "lifecycle-row-value", 
                                                  sprintf("%.1f", params$year_90_pct),
                                                  if(years_to_90 > 0) {
                                                    shiny::span(style = "font-size: 12px; color: #666; margin-left: 8px;",
                                                                sprintf("(%+.0f years)", years_to_90))
                                                  }
                                       )
                            ),
                            shiny::div(class = "lifecycle-row",
                                       shiny::div(class = "lifecycle-row-label", "99% of K:"),
                                       shiny::div(class = "lifecycle-row-value", 
                                                  sprintf("%.1f", params$year_99_pct),
                                                  if(years_to_99 > 0) {
                                                    shiny::span(style = "font-size: 12px; color: #666; margin-left: 8px;",
                                                                sprintf("(%+.0f years)", years_to_99))
                                                  }
                                       )
                            )
                          ),
                          
                          # Info box about current phase
                          if(current_pct < 10) {
                            shiny::div(class = "lifecycle-info-box",
                                       shiny::icon("lightbulb"),
                                       " The topic is in the ",
                                       shiny::strong("early growth phase"),
                                       " (< 10% of saturation)."
                            )
                          } else if(current_pct < 50) {
                            shiny::div(class = "lifecycle-info-box",
                                       shiny::icon("lightbulb"),
                                       " The topic is in the ",
                                       shiny::strong("rapid growth phase"),
                                       " (10-50% of saturation)."
                            )
                          } else if(current_pct < 90) {
                            shiny::div(class = "lifecycle-info-box warning",
                                       shiny::icon("exclamation-triangle"),
                                       " The topic is in the ",
                                       shiny::strong("maturity phase"),
                                       " (50-90% of saturation). Growth is slowing."
                            )
                          } else {
                            shiny::div(class = "lifecycle-info-box warning",
                                       shiny::icon("exclamation-triangle"),
                                       " The topic has reached ",
                                       shiny::strong("saturation phase"),
                                       " (> 90%). Annual publications are declining."
                            )
                          }
               )
               ),
               column(4,
               # === CARD 5: FORECAST ===
               if(nrow(forecast_df) > 0) {
                 future_5y <- forecast_df[forecast_df$PY == (last_obs_year + 5), ]
                 future_10y <- forecast_df[forecast_df$PY == (last_obs_year + 10), ]
                 
                 shiny::div(class = "lifecycle-card",
                            shiny::div(class = "lifecycle-header",
                                       icon("rocket"),
                                       "Forecast"
                            ),
                            shiny::div(
                              shiny::div(class = "lifecycle-row",
                                         shiny::div(class = "lifecycle-row-label", "Forecast Period:"),
                                         shiny::div(class = "lifecycle-row-value", 
                                                    sprintf("%d - %d", last_obs_year + 1, max(forecast_df$PY)))
                              ),
                              if(nrow(future_5y) > 0) {
                                shiny::div(class = "lifecycle-row",
                                           shiny::div(class = "lifecycle-row-label", 
                                                      sprintf("Projection for %d:", last_obs_year + 5)),
                                           shiny::div(class = "lifecycle-row-value", 
                                                      sprintf("%.0f cumulative", future_5y$predicted_cumulative[1]),
                                                      shiny::span(style = "font-size: 12px; color: #666; margin-left: 8px;",
                                                                  sprintf("(%.0f annual)", future_5y$predicted_annual[1]))
                                           )
                                )
                              },
                              if(nrow(future_10y) > 0) {
                                shiny::div(class = "lifecycle-row",
                                           shiny::div(class = "lifecycle-row-label", 
                                                      sprintf("Projection for %d:", last_obs_year + 10)),
                                           shiny::div(class = "lifecycle-row-value", 
                                                      sprintf("%.0f cumulative", future_10y$predicted_cumulative[1]),
                                                      shiny::span(style = "font-size: 12px; color: #666; margin-left: 8px;",
                                                                  sprintf("(%.0f annual)", future_10y$predicted_annual[1]))
                                           )
                                )
                              }
                            )
                 )
               }
               )
               )
    )
  )
}


