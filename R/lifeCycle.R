#' Life Cycle Analysis with Logistic Growth Model
#' 
#' Estimates logistic growth model for annual (non-cumulative) publications
#' following Meyer et al. (1999) methodology 
#' 
#' @param data Data frame with columns: year (PY) and number of publications (n)
#' @param forecast_years Number of years to forecast beyond saturation
#' @param plot Logical, if TRUE produces plots
#' @param verbose Logical, if TRUE prints detailed output
#' 
#' @return List containing parameters, forecasts and metrics
#' 
#' @export
lifeCycle <- function(data, 
                      forecast_years = 5,
                      plot = TRUE,
                      verbose = FALSE) {
  
  # Prepare data
  df <- data[order(data$PY), ]
  df$year_index <- df$PY - min(df$PY) + 1
  
  t <- df$year_index
  N_annual <- df$n  # Annual publications (NOT cumulative)
  base_year <- min(df$PY)
  
  # Logistic cumulative function
  logistic <- function(t, K, tm, delta_t) {
    exponent <- -(log(81)/delta_t) * (t - tm)
    exponent <- pmax(pmin(exponent, 700), -700)
    K / (1 + exp(exponent))
  }
  
  # Derivative: annual publications (rate of change)
  logistic_derivative <- function(t, K, tm, delta_t) {
    exponent <- -(log(81)/delta_t) * (t - tm)
    exponent <- pmax(pmin(exponent, 700), -700)
    constant <- log(81)/delta_t
    K * constant * exp(exponent) / (1 + exp(exponent))^2
  }
  
  # === ESTIMATE INITIAL PARAMETERS ===
  # Use cumulative for initial parameter estimation
  cumulative <- cumsum(N_annual)
  
  estimate_initial_params <- function(t, cum_N, annual_N) {
    # Estimate K from cumulative
    K_est <- max(cum_N) * 2.5
    
    # Find approximate midpoint (where annual publications peak)
    peak_idx <- which.max(annual_N)
    tm_est <- t[peak_idx]
    
    # Estimate delta_t from spread of data
    # Use the range where we have significant growth
    threshold <- max(annual_N) * 0.1
    growth_range <- t[annual_N > threshold]
    if(length(growth_range) > 1) {
      delta_t_est <- (max(growth_range) - min(growth_range)) / 2
    } else {
      delta_t_est <- length(t) / 3
    }
    
    list(K = K_est, tm = tm_est, delta_t = max(1, delta_t_est))
  }
  
  init <- estimate_initial_params(t, cumulative, N_annual)
  
  # === OPTIMIZATION: Fit on ANNUAL publications ===
  objective <- function(params) {
    K <- params[1]
    tm <- params[2]
    delta_t <- params[3]
    
    if (K <= max(cumulative) || delta_t <= 0) {
      return(1e10)
    }
    
    # Predict annual publications using derivative
    predicted <- logistic_derivative(t, K, tm, delta_t)
    
    if (any(!is.finite(predicted))) {
      return(1e10)
    }
    
    # Sum of squared errors on ANNUAL data
    sum((N_annual - predicted)^2)
  }
  
  result <- optim(
    par = c(init$K, init$tm, init$delta_t),
    fn = objective,
    method = "L-BFGS-B",
    lower = c(max(cumulative) * 1.01, min(t) * 0.5, 0.1),
    upper = c(max(cumulative) * 10, max(t) * 3, length(t) * 5),
    control = list(maxit = 2000)
  )
  
  if (result$convergence != 0) {
    result <- optim(
      par = c(init$K, init$tm, init$delta_t),
      fn = objective,
      method = "Nelder-Mead",
      control = list(maxit = 10000)
    )
  }
  
  # Extract parameters
  K <- as.numeric(result$par[1])
  tm <- as.numeric(result$par[2])
  delta_t <- as.numeric(result$par[3])
  
  params <- setNames(c(K, tm, delta_t), c("K", "tm", "delta_t"))
  
  # Fitted values for annual publications
  df$fitted_annual <- logistic_derivative(t, K, tm, delta_t)
  df$fitted_cumulative <- logistic(t, K, tm, delta_t)
  df$cumulative <- cumulative
  
  # Calculate actual years for interpretation
  tm_year <- base_year + tm - 1
  year_10_pct <- base_year + (tm - delta_t/2) - 1
  year_90_pct <- base_year + (tm + delta_t/2) - 1
  year_99_pct <- base_year + (tm + log(99) * delta_t / log(81)) - 1
  
  # === FORECAST ===
  years_to_99pct <- ceiling(year_99_pct - max(df$PY)) + forecast_years
  
  future_t <- seq(max(t) + 1, length.out = years_to_99pct, by = 1)
  future_years <- seq(max(df$PY) + 1, length.out = years_to_99pct, by = 1)
  
  forecast_annual <- logistic_derivative(future_t, K, tm, delta_t)
  forecast_cumulative <- logistic(future_t, K, tm, delta_t)
  
  forecast_df <- data.frame(
    PY = future_years,
    year_index = future_t,
    predicted_annual = forecast_annual,
    predicted_cumulative = forecast_cumulative
  )
  
  # Complete time series for plotting
  all_years <- c(df$PY, forecast_df$PY)
  all_t <- c(df$year_index, forecast_df$year_index)
  all_annual <- logistic_derivative(all_t, K, tm, delta_t)
  all_cumulative <- logistic(all_t, K, tm, delta_t)
  
  # === METRICS ===
  residuals <- N_annual - df$fitted_annual
  R2 <- 1 - sum(residuals^2) / sum((N_annual - mean(N_annual))^2)
  RMSE <- sqrt(mean(residuals^2))
  n_params <- length(params)
  n_obs <- length(N_annual)
  AIC_val <- n_obs * log(sum(residuals^2)/n_obs) + 2 * n_params
  BIC_val <- n_obs * log(sum(residuals^2)/n_obs) + n_params * log(n_obs)
  
  metrics <- data.frame(
    R_squared = R2,
    RMSE = RMSE,
    AIC = AIC_val,
    BIC = BIC_val
  )
  
  # === PLOTS ===
  if (plot) {
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
    
    # Plot 1: Annual Publications (MAIN - like Loglet Lab)
    max_annual <- max(c(N_annual, all_annual), na.rm = TRUE)
    
    plot(all_years, all_annual, type = "l", col = "blue", lwd = 2,
         xlab = "Year", ylab = "Publications (Annual)",
         main = "Life Cycle - Annual Publications",
         ylim = c(0, max_annual * 1.1))
    
    # Add observed points
    points(df$PY, N_annual, pch = 19, col = "blue", cex = 1.2)
    
    # Mark the peak
    abline(v = tm_year, lty = 2, col = "red")
    text(tm_year, max_annual * 0.95, 
         sprintf("Peak: %.1f", tm_year), 
         pos = 4, cex = 0.8, col = "red")
    
    legend("topleft", 
           legend = c("Observed", "Logistic fit", "Peak year"),
           col = c("blue", "blue", "red"), 
           pch = c(19, NA, NA),
           lty = c(NA, 1, 2),
           lwd = c(NA, 2, 1),
           bty = "n")
    
    text(max(all_years), max_annual * 1.05, 
         sprintf("R\u00b2 = %.3f", R2), 
         adj = 1, cex = 0.9)
    
    # Plot 2: Cumulative Publications
    plot(all_years, all_cumulative, type = "l", col = "darkgreen", lwd = 2,
         xlab = "Year", ylab = "Cumulative Publications",
         main = "Cumulative Growth Curve",
         ylim = c(0, K * 1.05))
    
    points(df$PY, df$cumulative, pch = 19, col = "darkgreen", cex = 1.2)
    
    # Add reference lines
    abline(h = K * 0.5, lty = 3, col = "gray50")
    abline(h = K * 0.9, lty = 3, col = "gray50")
    abline(h = K * 0.99, lty = 3, col = "gray50")
    
    text(min(all_years), K * 0.5, "50%", pos = 4, cex = 0.7, col = "gray50")
    text(min(all_years), K * 0.9, "90%", pos = 4, cex = 0.7, col = "gray50")
    text(min(all_years), K * 0.99, "99%", pos = 4, cex = 0.7, col = "gray50")
    
    legend("topleft", 
           legend = c("Observed", "Logistic fit"),
           col = c("darkgreen", "darkgreen"), 
           pch = c(19, NA),
           lty = c(NA, 1),
           lwd = c(NA, 2),
           bty = "n")
    
    # Plot 3: Fisher-Pry Transform
    ratio_all <- all_cumulative / (K - all_cumulative)
    ratio_all[ratio_all <= 0] <- NA
    fisher_pry_all <- log10(ratio_all)
    
    valid_fp <- is.finite(fisher_pry_all) & all_cumulative < K * 0.99
    
    if (sum(valid_fp) > 2) {
      plot(all_years[valid_fp], fisher_pry_all[valid_fp], 
           type = "l", col = "blue", lwd = 2,
           xlab = "Year", 
           ylab = expression(log[10]*"[F/(1-F)]"),
           main = "Fisher-Pry Transform",
           ylim = c(-2, 2))
      
      valid_obs <- is.finite(fisher_pry_all[1:nrow(df)])
      points(df$PY[valid_obs], fisher_pry_all[1:nrow(df)][valid_obs], 
             pch = 19, col = "blue", cex = 1.2)
      
      axis(4, at = log10(c(1/99, 1/9, 1, 9, 99)), 
           labels = c("1%", "10%", "50%", "90%", "99%"),
           las = 1, cex.axis = 0.8)
      
      abline(h = log10(c(1/99, 1/9, 1, 9, 99)), lty = 3, col = "gray70")
      
      valid_obs_fp <- is.finite(fisher_pry_all[1:nrow(df)])
      if(sum(valid_obs_fp) > 2) {
        fit_lm <- lm(fisher_pry_all[1:nrow(df)][valid_obs_fp] ~ df$PY[valid_obs_fp])
        r2_fp <- summary(fit_lm)$r.squared
        text(min(df$PY), max(fisher_pry_all[valid_fp], na.rm = TRUE), 
             sprintf("R\u00b2 = %.3f", r2_fp), adj = 0, cex = 0.9)
                 
      }
    }
    
    # Plot 4: Residuals
    plot(df$PY, residuals, type = "h", col = "darkgray", lwd = 2,
         xlab = "Year", ylab = "Residuals (Annual)", 
         main = "Model Residuals")
    abline(h = 0, col = "red", lty = 2)
    points(df$PY, residuals, pch = 19, col = "steelblue", cex = 1.2)
    
    text(max(df$PY), max(abs(residuals)) * 0.9,
         sprintf("RMSE = %.2f", RMSE),
         adj = 1, cex = 0.9)
    
    par(mfrow = c(1, 1))
  }
  
  if (verbose){
    # === REPORT ===
    cat("\n=== LIFE CYCLE ANALYSIS (LOGISTIC MODEL) ===\n")
    cat("Model: Logistic (fitted on annual publications)\n\n")
    cat("Estimated Parameters:\n")
    cat(sprintf("  K (saturation limit): %.2f\n", params["K"]))
    cat(sprintf("  tm (midpoint index): %.2f\n", params["tm"]))
    cat(sprintf("  delta_t (duration 10-90%%): %.2f\n", params["delta_t"]))
    cat("\nFit Metrics:\n")
    print(round(metrics, 3))
    
    cat("\nInterpretation:\n")
    cat(sprintf("- Estimated saturation (K): %.0f cumulative publications\n", params["K"]))
    cat(sprintf("- Peak year (tm): %.1f\n", tm_year))
    cat(sprintf("- Peak annual publications: %.0f\n", 
                logistic_derivative(tm, K, tm, delta_t)))
    cat(sprintf("- Growth duration 10-90%% (delta_t): %.1f years\n", params["delta_t"]))
    cat(sprintf("- Year reaching 50%% of K: %.1f\n", tm_year))
    cat(sprintf("- Year reaching 90%% of K: %.1f\n", year_90_pct))
    cat(sprintf("- Year reaching 99%% of K: %.1f\n", year_99_pct))
    
    # Forecast summary
    cat("\n--- Forecast Summary ---\n")
    cat(sprintf("Last observed year: %d (%.0f annual / %.0f cumulative)\n", 
                max(df$PY), tail(N_annual, 1), max(df$cumulative)))
    cat(sprintf("Forecast extends to: %d\n", max(forecast_df$PY)))
  }
  
  next_5_year <- max(df$PY) + 5
  if(next_5_year <= max(forecast_df$PY)) {
    next_5_val <- forecast_df$predicted_cumulative[forecast_df$PY == next_5_year]
    if (verbose) cat(sprintf("\nProjection for %d: %.0f cumulative publications\n", 
                next_5_year, next_5_val))
  }
  
  
  results <- list(
    parameters = params,
    parameters_real_years = data.frame(
      K = K,
      tm_year = tm_year,
      delta_t = delta_t,
      peak_annual = logistic_derivative(tm, K, tm, delta_t),
      year_10_pct = year_10_pct,
      year_90_pct = year_90_pct,
      year_99_pct = year_99_pct
    ),
    data = df,
    forecast = forecast_df,
    complete_curve = data.frame(
      year = all_years,
      year_index = all_t,
      annual = all_annual,
      cumulative = all_cumulative
    ),
    metrics = metrics,
    residuals = residuals,
    base_year = base_year
  )
  
  return(invisible(results))
}





# Example usage:
# data <- data.frame(PY = 2000:2024, n = c(5, 8, 12, 20, 35, 55, 80, ...))
# results <- lifeCycle(data, forecast_years = 20, plot = FALSE)
# 
# # For biblioshiny
# p1 <- plotLifeCycle(results, plot_type = "annual")
# p2 <- plotLifeCycle(results, plot_type = "cumulative")
# 
# # In Shiny app:
# output$lifeCyclePlot1 <- plotly::renderPlotly({ p1 })
# output$lifeCyclePlot2 <- plotly::renderPlotly({ p2 })