# Load required libraries
if (!require("pdp")) install.packages("pdp", dependencies = TRUE)
if (!require("caret")) install.packages("caret", dependencies = TRUE)
if (!require("nnet")) install.packages("nnet", dependencies = TRUE)

library(pdp)         # Partial dependence plots
library(caret)       # Cross-validation and hyperparameter tuning
library(nnet)        # Neural network
library(dplyr)       # Data manipulation
library(ggplot2)     # Visualization

# ------------------------
# Step 1: Load and Prepare the Data
# ------------------------

# Set working directory and load data
setwd("/Users/shishamadhikari/Desktop/Research/Co-Co-Co/empirics/data")
final_data <- read.csv("final_data.csv")

# Prepare the dataset with feature engineering
final_data_nn <- final_data %>%
  mutate(log_patents = log1p(total_patents),  # Log-transform of patents
         hhi_index_sq = hhi_index^2) %>%      # Add quadratic term
  filter(!is.na(hhi_index) & !is.na(log_patents) & !is.na(hhi_index_sq) &
           !is.na(hc) & !is.na(rgdpe) & !is.na(kaopen) & !is.na(ctfp) & !is.na(xr)) %>%
  mutate(
    hhi_index = as.numeric(scale(hhi_index)),       # Scale predictors
    hhi_index_sq = as.numeric(scale(hhi_index_sq)), # Scale quadratic term
    hc = as.numeric(scale(hc)),
    rgdpe = as.numeric(scale(rgdpe)),
    kaopen = as.numeric(scale(kaopen)),
    ctfp = as.numeric(scale(ctfp)),
    xr = as.numeric(scale(xr)),
    log_patents = as.numeric(scale(log_patents))
  )

print("Data preparation complete.")

# ------------------------
# Step 2: Train-Test Split
# ------------------------

set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(final_data_nn$log_patents, p = 0.8, list = FALSE)
train_data <- final_data_nn[trainIndex, ] %>% as.data.frame()
test_data <- final_data_nn[-trainIndex, ] %>% as.data.frame()

# ------------------------
# Step 3: Train a Deeper Neural Network
# ------------------------

# Define control for cross-validation
train_control <- trainControl(
  method = "cv",        # Cross-validation
  number = 5,           # 5-fold CV
  verboseIter = TRUE    # Show progress
)

# Grid of hyperparameters
tune_grid <- expand.grid(size = c(15, 30, 50), decay = c(0.001, 0.01, 0.1))

# Train the model
set.seed(123)
deep_nn_model <- train(
  log_patents ~ hhi_index + hhi_index_sq + hc + rgdpe + kaopen + ctfp + xr,
  data = train_data,
  method = "nnet",
  tuneGrid = tune_grid,
  trControl = train_control,
  linout = TRUE,   # Linear output for regression
  trace = FALSE,   # Suppress training output
  maxit = 5000     # Maximum iterations
)

print("Cross-validated deeper neural network training complete!")
print("Best hyperparameters:")
print(deep_nn_model$bestTune)

# ------------------------
# Step 4: Make Predictions and Evaluate
# ------------------------

# Predict on test data
test_data$nn_pred <- predict(deep_nn_model, test_data)

# R-squared and RMSE
r2 <- R2(test_data$nn_pred, test_data$log_patents)
rmse <- RMSE(test_data$nn_pred, test_data$log_patents)
print(paste("R-squared:", round(r2, 4)))
print(paste("Root Mean Squared Error (RMSE):", round(rmse, 4)))

# ------------------------
# Step 5: Partial Dependence Plot for HHI Index
# ------------------------

# Generate Partial Dependence Plot for hhi_index
pdp_hhi <- partial(
  object = deep_nn_model,
  pred.var = "hhi_index",
  train = as.data.frame(train_data),
  grid.resolution = 500,  # Increase the number of grid points for smoother curve
  ice = FALSE             # Disable ICE curves for cleaner PDP
)

# Plot the Partial Dependence
plot_pdp <- ggplot(pdp_hhi, aes(x = hhi_index, y = yhat)) +
  geom_line(color = "blue", size = 1.2) +
  labs(title = "PDP of Log Patents on HHI Index (Deeper Neural Network)",
       x = "HHI Index (Market Competition)",
       y = "Predicted Log of Patents") +
  theme_minimal()

print(plot_pdp)

# Save the PDP Plot
pdp_plot_path <- "pdp_hhi_index_deep_nn.png"
ggsave(pdp_plot_path, plot = plot_pdp)
print(paste("PDP Plot saved:", pdp_plot_path))

# ------------------------
# Step 6: Save Results
# ------------------------

output_file <- "deep_nn_results.csv"
write.csv(test_data, output_file, row.names = FALSE)
print(paste("Results saved:", output_file))
