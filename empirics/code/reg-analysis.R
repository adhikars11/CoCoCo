# Load required libraries
library(plm)          # Panel data analysis
library(dplyr)        # Data manipulation
library(Metrics)      # Performance metrics
library(nlme)         # Generalized Least Squares

# Step 1: Load and clean dataset
final_data <- read.csv("final_data.csv") %>%
  mutate(
    hhi_index = as.numeric(hhi_index),
    log_patents = log1p(as.numeric(total_patents)),
    log_enablingpatents = log1p(as.numeric(enabling_tech_patents)),
    country = as.factor(country),
    year = as.factor(year)
  ) %>%
  filter(!is.na(hhi_index) & !is.na(log_patents) & !is.na(hc) & 
           !is.na(rgdpe) & !is.na(kaopen) & !is.na(ctfp) & !is.na(xr)) %>%
  mutate(hhi_index_centered = hhi_index - mean(hhi_index, na.rm = TRUE),
         hhi_index_sq = hhi_index_centered^2)

# Step 2: Convert to Panel Data
pdata <- pdata.frame(final_data, index = c("country", "year"))

# Function to calculate RMSE and extract R²
evaluate_model <- function(model, data, actual_values) {
  fitted_values <- as.numeric(fitted(model))
  valid_rows <- !is.na(actual_values) & !is.na(fitted_values)
  actual_values <- actual_values[valid_rows]
  fitted_values <- fitted_values[valid_rows]
  
  r2 <- 1 - sum((actual_values - fitted_values)^2) / sum((actual_values - mean(actual_values))^2)
  rmse_value <- rmse(actual_values, fitted_values)
  return(list(R2 = r2, RMSE = rmse_value))
}

evaluate_model_fixed <- function(model, data, actual_values) {
  residuals <- resid(model)
  fitted_values <- actual_values - residuals
  mean_within <- mean(actual_values, na.rm = TRUE)
  tss <- sum((actual_values - mean_within)^2, na.rm = TRUE)
  rss <- sum(residuals^2, na.rm = TRUE)
  r2 <- 1 - (rss / tss)
  rmse_value <- sqrt(mean(residuals^2, na.rm = TRUE))
  return(list(R2 = r2, RMSE = rmse_value))
}

# Initialize results table
results_table <- data.frame(
  Model = character(),
  R2 = numeric(),
  RMSE = numeric(),
  stringsAsFactors = FALSE
)

# --- Model 1: Pooled OLS ---
# This model assumes no heterogeneity across entities or time.
pooled_model <- plm(
  log_patents ~ hhi_index_centered + hhi_index_sq + hc + rgdpe + kaopen + ctfp + xr,
  data = pdata,
  model = "pooling"
)
pooled_eval <- evaluate_model(pooled_model, pdata, pdata$log_patents)
results_table <- rbind(results_table, data.frame(
  Model = "Pooled OLS", R2 = pooled_eval$R2, RMSE = pooled_eval$RMSE
))

# --- Model 2: Fixed Effects (Individual) ---
# Controls for country-specific fixed effects.
fe_individual <- plm(
  log_patents ~ hhi_index_centered + hhi_index_sq + hc + rgdpe + kaopen + ctfp + xr,
  data = pdata,
  model = "within",
  effect = "individual"
)
fe_ind_eval <- evaluate_model_fixed(fe_individual, pdata, pdata$log_patents)
results_table <- rbind(results_table, data.frame(
  Model = "Fixed Effects (Individual)", R2 = fe_ind_eval$R2, RMSE = fe_ind_eval$RMSE
))

# --- Model 3: Fixed Effects (Time) ---
# Controls for time-specific fixed effects.
fe_time <- plm(
  log_patents ~ hhi_index_centered + hhi_index_sq + hc + rgdpe + kaopen + ctfp + xr,
  data = pdata,
  model = "within",
  effect = "time"
)
fe_time_eval <- evaluate_model_fixed(fe_time, pdata, pdata$log_patents)
results_table <- rbind(results_table, data.frame(
  Model = "Fixed Effects (Time)", R2 = fe_time_eval$R2, RMSE = fe_time_eval$RMSE
))

# --- Model 4: Fixed Effects (Two-Way) ---
# Controls for both country and time fixed effects.
fe_twoway <- plm(
  log_patents ~ hhi_index_centered + hhi_index_sq + hc + rgdpe + kaopen + ctfp + xr,
  data = pdata,
  model = "within",
  effect = "twoways"
)
fe_tw_eval <- evaluate_model_fixed(fe_twoway, pdata, pdata$log_patents)
results_table <- rbind(results_table, data.frame(
  Model = "Fixed Effects (Two-Way)", R2 = fe_tw_eval$R2, RMSE = fe_tw_eval$RMSE
))
summary(fe_twoway)

# --- Model 5: Generalized Least Squares (GLS) ---
# Accounts for serial correlation and heteroskedasticity.
gls_model <- gls(
  log_patents ~ hhi_index_centered + hhi_index_sq + hc + rgdpe + kaopen + ctfp + xr,
  correlation = corAR1(), 
  data = final_data
)
gls_pred <- predict(gls_model)
gls_r2 <- 1 - sum((final_data$log_patents - gls_pred)^2) / 
  sum((final_data$log_patents - mean(final_data$log_patents))^2)
gls_rmse <- rmse(final_data$log_patents, gls_pred)

results_table <- rbind(results_table, data.frame(
  Model = "Generalized Least Squares", R2 = gls_r2, RMSE = gls_rmse
))

# Step 3: Compare Results
print("Model Performance Comparison:")
print(results_table)
