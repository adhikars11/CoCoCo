# Load required libraries
library(plm)
library(dplyr)
library(Metrics)

# Load required libraries
library(plm)
library(dplyr)
library(Metrics)

# Step 1: Load and clean dataset
final_data <- read.csv("final_data.csv") %>%
  mutate(
    hhi_index = as.numeric(hhi_index),
    log_patents = log1p(as.numeric(total_patents)),
    log_enablingpatents = log1p(as.numeric(enabling_tech_patents)),
    country = as.factor(country),
    year = as.factor(year)
  ) %>%
  filter(!is.na(hhi_index) & !is.na(log_patents) & !is.na(hc) & !is.na(rgdpe) & !is.na(kaopen) & !is.na(ctfp) & !is.na(xr))

# Step 2: EDA
# Overview of the dataset
summary(final_data)
str(final_data)

# Check unique entities and time periods
num_countries <- length(unique(final_data$country))
num_years <- length(unique(final_data$year))
print(paste("Number of countries:", num_countries))
print(paste("Number of years:", num_years))

# Check for balance
table(final_data$country, final_data$year)  # Creates a matrix for presence of data

# Check for missing values
na_counts <- colSums(is.na(final_data))
print("Missing values per column:")
print(na_counts)

# Histograms for continuous variables
hist(final_data$hhi_index, main = "Distribution of HHI Index", xlab = "HHI Index")
hist(final_data$log_patents, main = "Distribution of Log Patents", xlab = "Log Patents")

# Boxplot by country
ggplot(final_data, aes(x = country, y = log_patents)) +
  geom_boxplot() +
  labs(title = "Log Patents by Country") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Average trend of log_patents over time
# Ensure 'year' is numeric
final_data$year <- as.numeric(as.character(final_data$year))
avg_trend <- final_data %>%
  group_by(year) %>%
  summarize(mean_log_patents = mean(log_patents, na.rm = TRUE))  # Use log_patents

# Plot the average log_patents over time
ggplot(avg_trend, aes(x = year, y = mean_log_patents)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Average Log Patents Over Time",
    x = "Year",
    y = "Average Log Patents"
  ) +
  theme_minimal()

# Average HHI Index by Country
country_avg <- final_data %>%
  group_by(country) %>%
  summarize(avg_hhi = mean(hhi_index, na.rm = TRUE))

ggplot(country_avg, aes(x = reorder(country, avg_hhi), y = avg_hhi)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average HHI Index by Country", x = "Country", y = "HHI Index") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Average Innovation by Country
country_avg1 <- final_data %>%
  group_by(country) %>%
  summarize(avg_patents = mean(total_patents, na.rm = TRUE))

ggplot(country_avg1, aes(x = reorder(country, avg_patents), y = avg_patents)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Green Patents by Country", x = "Country", y = "Total Patents") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Correlations between variables 
# Select numeric columns
numeric_vars <- final_data %>% select(where(is.numeric))
cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")

# Heatmap for correlation matrix
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black")

# Visualize the relationship using a scatter plot with a linear trend
ggplot(final_data, aes(x = hhi_index, y = log_patents)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship between HHI Index and Innovation",
       x = "HHI Index (Market Concentration)",
       y = "Log of Total Patents") +
  theme_minimal()

# Panel Data Exploration
# Trends of log_patents for top-5 countries country
# Step 2: Identify the top 5 innovating countries by average total patents
top_countries <- final_data %>%
  group_by(country) %>%
  summarise(mean_patents = mean(total_patents, na.rm = TRUE)) %>%
  arrange(desc(mean_patents)) %>%
  slice_head(n = 5) %>%  # Select top 5 countries
  pull(country)

final_data %>%
  filter(country %in% top_countries) %>%  # Filter for top countries
  ggplot(aes(x = year, y = log_patents, group = country, color = country)) +
  geom_line(size = 0.5) +
  labs(
    title = "Trends in Log Patents for Top 5 Innovating Countries",
    x = "Year",
    y = "Log of Total Patents"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")