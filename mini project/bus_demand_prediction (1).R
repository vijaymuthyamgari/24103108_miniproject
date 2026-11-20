# ============================================================
# BUS / TRANSPORT DEMAND ANALYSIS AND PREDICTION USING R
# Single dataset: Dataset D - Chennai Hub
# Input: Dataset_D_Chennai_Hub_combined_data.csv
# ============================================================

# ---------------------------
# 0. PROJECT SETUP
# ---------------------------

# Change this path only if your dataset is stored elsewhere.
project_folder <- getwd()

input_file <- file.path(
  project_folder,
  "Dataset_D_Chennai_Hub_combined_data.csv"
)



if (!file.exists(input_file)) {
  stop(
    paste0(
      "Dataset file not found: ", input_file,
      "\nCheck the filename and project_folder."
    )
  )
}

# Install only packages that are not already installed.
required_packages <- c("dplyr", "ggplot2", "tidyr", "rpart", "randomForest")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(dplyr)
library(ggplot2)
library(tidyr)
library(rpart)
library(randomForest)

set.seed(123)

output_folder <- file.path(project_folder, "results_single_dataset")
graph_folder <- file.path(output_folder, "graphs")
dir.create(graph_folder, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 1. LOAD AND INSPECT DATA
# ---------------------------

raw_data <- read.csv(input_file, check.names = FALSE)

cat("Dataset dimensions:\n")
print(dim(raw_data))

cat("\nColumn names:\n")
print(names(raw_data))

cat("\nFirst six records:\n")
print(head(raw_data))

cat("\nStructure:\n")
str(raw_data)

cat("\nSummary:\n")
print(summary(raw_data))

# Confirm the required columns are present.
required_columns <- c(
  "final_booked_seats",
  "MONTH(TRAVEL_DATE)",
  "YEAR(TRAVEL_DATE)"
)
missing_columns <- setdiff(required_columns, names(raw_data))
if (length(missing_columns) > 0) {
  stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
}

# ---------------------------
# 2. DATA PREPROCESSING
# ---------------------------

# Check missing values and duplicate rows before cleaning.
missing_summary <- data.frame(
  column = names(raw_data),
  missing_values = sapply(raw_data, function(x) sum(is.na(x)))
)

cat("\nMissing values by column:\n")
print(missing_summary)
cat("\nExact duplicate rows:\n")
print(sum(duplicated(raw_data)))

# Standardize the relevant columns and create a date variable.
bus_data <- raw_data %>%
  transmute(
    serial = if ("serial" %in% names(raw_data)) as.integer(serial) else row_number(),
    demand = suppressWarnings(as.numeric(.data[["final_booked_seats"]])),
    month = suppressWarnings(as.integer(.data[["MONTH(TRAVEL_DATE)"]])),
    year = suppressWarnings(as.integer(.data[["YEAR(TRAVEL_DATE)"]]))
  ) %>%
  filter(
    !is.na(month), !is.na(year), !is.na(demand),
    month >= 1, month <= 12,
    demand >= 0
  ) %>%
  mutate(
    date = as.Date(sprintf("%04d-%02d-01", year, month))
  ) %>%
  arrange(date)

# Check whether more than one record exists for a month.
duplicate_months <- bus_data %>%
  count(date, name = "records_in_month") %>%
  filter(records_in_month > 1)

cat("\nMonths with multiple records (inspect before aggregating):\n")
print(duplicate_months)

# This supplied dataset is expected to have one row per month.
# Stop rather than silently discarding or summing repeated months.
if (nrow(duplicate_months) > 0) {
  stop("Repeated month/year records found. Inspect duplicate_months before proceeding.")
}

bus_data <- bus_data %>% distinct(date, .keep_all = TRUE) %>% arrange(date)

cat("\nCleaned dataset dimensions:\n")
print(dim(bus_data))
cat("\nDate range:\n")
print(range(bus_data$date))

write.csv(bus_data, file.path(output_folder, "cleaned_chennai_hub_data.csv"), row.names = FALSE)
write.csv(missing_summary, file.path(output_folder, "missing_value_summary.csv"), row.names = FALSE)

# ---------------------------
# 3. DESCRIPTIVE STATISTICS
# ---------------------------

descriptive_stats <- bus_data %>%
  summarise(
    observations = n(),
    mean_demand = mean(demand),
    median_demand = median(demand),
    standard_deviation = sd(demand),
    minimum_demand = min(demand),
    maximum_demand = max(demand)
  )

cat("\nDescriptive statistics:\n")
print(descriptive_stats)
write.csv(descriptive_stats, file.path(output_folder, "descriptive_statistics.csv"), row.names = FALSE)

# ---------------------------
# 4. EXPLORATORY DATA ANALYSIS
# ---------------------------

# 4.1 Monthly demand trend.
p_monthly_trend <- ggplot(bus_data, aes(x = date, y = demand)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  labs(
    title = "Monthly Bus Passenger Demand - Chennai Hub",
    x = "Travel month",
    y = "Booked seats"
  ) +
  theme_minimal()
print(p_monthly_trend)
ggsave(file.path(graph_folder, "monthly_demand_trend.png"), p_monthly_trend,
       width = 10, height = 6)

# 4.2 Average demand by calendar month (monthly/seasonal pattern).
monthly_summary <- bus_data %>%
  group_by(month) %>%
  summarise(
    average_demand = mean(demand),
    observations = n(),
    .groups = "drop"
  ) %>%
  arrange(month)

print(monthly_summary)
write.csv(monthly_summary, file.path(output_folder, "monthly_demand_summary.csv"), row.names = FALSE)

p_monthly_average <- ggplot(monthly_summary, aes(x = factor(month), y = average_demand)) +
  geom_col() +
  labs(
    title = "Average Booked Seats by Month",
    x = "Month (1 = January)",
    y = "Average booked seats"
  ) +
  theme_minimal()
print(p_monthly_average)
ggsave(file.path(graph_folder, "average_demand_by_month.png"), p_monthly_average,
       width = 9, height = 6)

# 4.3 Yearly average. Also show number of months observed in each year.
# Note: 2015 contains only January in this file, so it is a partial year.
yearly_summary <- bus_data %>%
  group_by(year) %>%
  summarise(
    average_monthly_demand = mean(demand),
    total_observed_demand = sum(demand),
    months_observed = n(),
    .groups = "drop"
  ) %>%
  arrange(year)

print(yearly_summary)
write.csv(yearly_summary, file.path(output_folder, "yearly_demand_summary.csv"), row.names = FALSE)

p_yearly <- ggplot(yearly_summary, aes(x = year, y = average_monthly_demand)) +
  geom_line() +
  geom_point(size = 2) +
  geom_text(aes(label = paste0("n=", months_observed)), vjust = -0.8, size = 3) +
  labs(
    title = "Yearly Average Monthly Demand",
    subtitle = "2015 is a partial year in this dataset",
    x = "Year",
    y = "Average booked seats"
  ) +
  theme_minimal()
print(p_yearly)
ggsave(file.path(graph_folder, "yearly_average_demand.png"), p_yearly,
       width = 9, height = 6)

# 4.4 Boxplot of demand distribution by month.
p_month_box <- ggplot(bus_data, aes(x = factor(month), y = demand)) +
  geom_boxplot() +
  labs(
    title = "Demand Distribution by Calendar Month",
    x = "Month (1 = January)",
    y = "Booked seats"
  ) +
  theme_minimal()
print(p_month_box)
ggsave(file.path(graph_folder, "demand_distribution_by_month.png"), p_month_box,
       width = 9, height = 6)

# ---------------------------
# 5. CORRELATION ANALYSIS
# ---------------------------

# Represent calendar month cyclically: December is near January.
bus_data <- bus_data %>%
  mutate(
    time_index = row_number(),
    month_sin = sin(2 * pi * month / 12),
    month_cos = cos(2 * pi * month / 12),
    lag1 = dplyr::lag(demand, 1)
  )

correlation_data <- bus_data %>%
  select(demand, year, time_index, month_sin, month_cos, lag1) %>%
  drop_na()

correlation_matrix <- cor(correlation_data, use = "complete.obs", method = "pearson")
cat("\nCorrelation matrix:\n")
print(round(correlation_matrix, 3))
write.csv(correlation_matrix, file.path(output_folder, "correlation_matrix.csv"))

correlation_long <- as.data.frame(as.table(correlation_matrix)) %>%
  rename(variable_x = Var1, variable_y = Var2, correlation = Freq)

p_correlation <- ggplot(correlation_long, aes(x = variable_x, y = variable_y, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", correlation)), size = 3) +
  labs(title = "Correlation Matrix", x = NULL, y = NULL, fill = "Pearson r") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_correlation)
ggsave(file.path(graph_folder, "correlation_heatmap.png"), p_correlation,
       width = 8, height = 6)

# ---------------------------
# 6. K-MEANS CLUSTERING
# ---------------------------

# Cluster months based on demand only. Labels are ordered by cluster mean.
if (length(unique(bus_data$demand)) < 3) {
  stop("K-Means with 3 clusters requires at least 3 distinct demand values.")
}

set.seed(123)
km <- kmeans(scale(bus_data$demand), centers = 3, nstart = 25)
cluster_means <- tapply(bus_data$demand, km$cluster, mean)
ordered_cluster_ids <- names(sort(cluster_means))
cluster_labels <- setNames(c("Low", "Medium", "High"), ordered_cluster_ids)

cluster_data <- bus_data %>%
  mutate(
    cluster_id = km$cluster,
    demand_cluster = unname(cluster_labels[as.character(km$cluster)])
  )

cat("\nK-Means cluster summary:\n")
print(cluster_data %>% group_by(demand_cluster) %>%
        summarise(months = n(), average_demand = mean(demand), .groups = "drop"))
write.csv(cluster_data, file.path(output_folder, "kmeans_clustering_results.csv"), row.names = FALSE)

p_clusters <- ggplot(cluster_data, aes(x = date, y = demand, color = demand_cluster)) +
  geom_point(size = 2.5) +
  labs(
    title = "K-Means Clustering of Monthly Demand",
    x = "Travel month",
    y = "Booked seats",
    color = "Demand group"
  ) +
  theme_minimal()
print(p_clusters)
ggsave(file.path(graph_folder, "kmeans_demand_clusters.png"), p_clusters,
       width = 10, height = 6)

# ---------------------------
# 7. FEATURE ENGINEERING FOR PREDICTION
# ---------------------------

# Lag 1 means the previous month's observed booked-seat demand.
model_data <- bus_data %>%
  arrange(date) %>%
  mutate(lag1 = lag(demand, 1)) %>%
  drop_na(lag1)

cat("\nUsable model observations after creating lag1:\n")
print(nrow(model_data))

# With 49 original rows, this should leave 48 supervised observations.
if (nrow(model_data) < 20) {
  stop("Too few observations remain for a meaningful train/test comparison.")
}

# ---------------------------
# 8. CHRONOLOGICAL TRAIN / TEST SPLIT
# ---------------------------

n <- nrow(model_data)
split_point <- floor(0.75 * n)
train_data <- model_data[seq_len(split_point), ]
test_data <- model_data[(split_point + 1):n, ]

cat("\nTraining observations:", nrow(train_data), "\n")
cat("Testing observations:", nrow(test_data), "\n")
cat("Training period:", as.character(min(train_data$date)), "to", as.character(max(train_data$date)), "\n")
cat("Testing period:", as.character(min(test_data$date)), "to", as.character(max(test_data$date)), "\n")

# The test uses the actual previous month's demand for each test month.
# This is a rolling one-step-ahead evaluation, not a multi-month forecast made all at once.

# ---------------------------
# 9. TRAIN THREE MODELS
# ---------------------------

model_formula <- demand ~ time_index + month_sin + month_cos + lag1

# 9.1 Linear Regression
model_lm <- lm(model_formula, data = train_data)
pred_lm <- as.numeric(predict(model_lm, newdata = test_data))

# 9.2 Decision Tree
model_tree <- rpart(
  model_formula,
  data = train_data,
  method = "anova",
  control = rpart.control(maxdepth = 3, minsplit = 8, cp = 0.01)
)
pred_tree <- as.numeric(predict(model_tree, newdata = test_data))

# 9.3 Random Forest
set.seed(123)
model_rf <- randomForest(
  model_formula,
  data = train_data,
  ntree = 300,
  mtry = 2,
  importance = TRUE
)
pred_rf <- as.numeric(predict(model_rf, newdata = test_data))

# Previous-month naive baseline for reference.
pred_naive <- test_data$lag1

# ---------------------------
# 10. MODEL EVALUATION
# ---------------------------

mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

r2_score <- function(actual, predicted) {
  if (length(actual) < 2 || is.na(var(actual)) || var(actual) == 0) {
    return(NA_real_)
  }
  1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
}

prediction_list <- list(
  Previous_Month_Baseline = pred_naive,
  Linear_Regression = pred_lm,
  Decision_Tree = pred_tree,
  Random_Forest = pred_rf
)

model_comparison <- bind_rows(lapply(names(prediction_list), function(model_name) {
  predicted <- prediction_list[[model_name]]
  data.frame(
    model = model_name,
    MAE = mae(test_data$demand, predicted),
    RMSE = rmse(test_data$demand, predicted),
    R2 = r2_score(test_data$demand, predicted)
  )
})) %>% arrange(RMSE)

cat("\nModel comparison (lower MAE/RMSE is better; higher R-squared is generally better):\n")
print(model_comparison)
write.csv(model_comparison, file.path(output_folder, "model_comparison.csv"), row.names = FALSE)

prediction_details <- bind_rows(lapply(names(prediction_list), function(model_name) {
  data.frame(
    date = test_data$date,
    actual = test_data$demand,
    predicted = prediction_list[[model_name]],
    model = model_name
  )
}))

write.csv(prediction_details, file.path(output_folder, "prediction_details.csv"), row.names = FALSE)

# ---------------------------
# 11. ACTUAL VS PREDICTED GRAPH
# ---------------------------

p_predictions <- ggplot(prediction_details, aes(x = date)) +
  geom_line(aes(y = actual), color = "black", linewidth = 0.9) +
  geom_line(aes(y = predicted, color = model), linewidth = 0.7) +
  labs(
    title = "Actual vs Predicted Monthly Bus Demand",
    x = "Travel month",
    y = "Booked seats",
    color = "Model"
  ) +
  theme_minimal()
print(p_predictions)
ggsave(file.path(graph_folder, "actual_vs_predicted.png"), p_predictions,
       width = 10, height = 6)

# ---------------------------
# 12. NEXT-MONTH ESTIMATE USING BEST TEST-RMSE MODEL
# ---------------------------

best_model_name <- model_comparison$model[1]
last_date <- max(bus_data$date)
next_date <- as.Date(seq(last_date, by = "month", length.out = 2)[2])
next_month <- as.integer(format(next_date, "%m"))
next_time_index <- max(bus_data$time_index) + 1
last_demand <- tail(bus_data$demand, 1)

next_input <- data.frame(
  time_index = next_time_index,
  month_sin = sin(2 * pi * next_month / 12),
  month_cos = cos(2 * pi * next_month / 12),
  lag1 = last_demand
)

# Refit the selected model on all available supervised observations.
if (best_model_name == "Previous_Month_Baseline") {
  next_prediction <- last_demand
} else if (best_model_name == "Linear_Regression") {
  final_model <- lm(model_formula, data = model_data)
  next_prediction <- as.numeric(predict(final_model, newdata = next_input))
} else if (best_model_name == "Decision_Tree") {
  final_model <- rpart(
    model_formula,
    data = model_data,
    method = "anova",
    control = rpart.control(maxdepth = 3, minsplit = 8, cp = 0.01)
  )
  next_prediction <- as.numeric(predict(final_model, newdata = next_input))
} else {
  set.seed(123)
  final_model <- randomForest(
    model_formula,
    data = model_data,
    ntree = 300,
    mtry = 2,
    importance = TRUE
  )
  next_prediction <- as.numeric(predict(final_model, newdata = next_input))
}

next_month_forecast <- data.frame(
  forecast_month = next_date,
  selected_model = best_model_name,
  predicted_booked_seats = max(0, next_prediction)
)

cat("\nNext-month estimate (experimental):\n")
print(next_month_forecast)
write.csv(next_month_forecast, file.path(output_folder, "next_month_forecast.csv"), row.names = FALSE)

# ---------------------------
# 13. RANDOM FOREST VARIABLE IMPORTANCE
# ---------------------------

if (exists("model_rf")) {
  importance_table <- as.data.frame(importance(model_rf))
  importance_table$feature <- rownames(importance_table)
  rownames(importance_table) <- NULL
  print(importance_table)
  write.csv(importance_table, file.path(output_folder, "random_forest_variable_importance.csv"), row.names = FALSE)
}

# ---------------------------
# 14. FINAL CONSOLE SUMMARY
# ---------------------------

cat("\n====================================================\n")
cat("PROJECT COMPLETED\n")
cat("Dataset used: Chennai Hub (Dataset D) only\n")
cat("Original observations:", nrow(raw_data), "\n")
cat("Cleaned observations:", nrow(bus_data), "\n")
cat("Model observations:", nrow(model_data), "\n")
cat("Best model by test RMSE:", best_model_name, "\n")
cat("Results saved in:", output_folder, "\n")
cat("====================================================\n")

# IMPORTANT INTERPRETATION NOTES:
# 1. This dataset has monthly booked-seat demand, month, and year only.
# 2. It does not include weather, holidays, fares, capacity, or passenger profiles.
# 3. It has 49 monthly records (2011-Jan to 2015-Jan); 2015 is only a partial year.
# 4. K-Means labels Low/Medium/High are relative clusters within this dataset.
# 5. With so few observations, tree/forest metrics are unstable and should be
#    presented as a classroom comparison, not proof of strong real-world accuracy.
# 6. The test is rolling one-step-ahead: each test month's lag1 is the actual
#    previous month's demand. This differs from a multi-month forecast.
