# This R code can be used to perform detailed exploratory data analysis
# on a dataset.


# Function to load the dataset
load_dataset <- function(file_path) {
  dataset <- read.csv(file_path)
  return(dataset)
}

# Function to display the head of the dataset
display_head <- function(dataset, n = 5) {
  head(dataset, n)
}

# Function to display the summary statistics of the dataset
summary_stats <- function(dataset) {
  summary(dataset)
}

# Function to display the dimensions of the dataset
get_dimensions <- function(dataset) {
  dimensions <- dim(dataset)
  return(dimensions)
}

# Function to display the column names of the dataset
get_column_names <- function(dataset) {
  col_names <- colnames(dataset)
  return(col_names)
}

# Function to check for missing values in the dataset
check_missing_values <- function(dataset) {
  missing_values <- sum(is.na(dataset))
  if (missing_values > 0) {
    message("Missing values found:", missing_values)
  } else {
    message("No missing values found.")
  }
}

# Function to generate descriptive statistics for a numeric variable
numeric_stats <- function(dataset, column_name) {
  stats <- summary(dataset[[column_name]])
  return(stats)
}

# Function to generate a histogram for a numeric variable
plot_histogram <- function(dataset, column_name) {
  hist(dataset[[column_name]],
       main = paste("Histogram of", column_name),
    xlab=column_name)
}

# Function to generate a boxplot for a numeric variable
plot_boxplot <- function(dataset, column_name) {
  boxplot(dataset[[column_name]], main = paste("Boxplot of", column_name))
}

# Function to generate a bar plot for a categorical variable
plot_barplot <- function(dataset, column_name) {
  barplot(table(dataset[[column_name]]), main = paste("Barplot of", column_name))
}

# Function to generate a scatter plot for two numeric variables
plot_scatter <- function(dataset, x_column, y_column) {
  plot(dataset[[x_column]], dataset[[y_column]], main = "Scatter Plot", 
       xlab = y_column, ylab = x_column)
}

# Function to generate a correlation matrix for numeric variables
correlation_matrix <- function(dataset) {
  corr_matrix <- cor(dataset, use = "pairwise.complete.obs")
  return(corr_matrix)
}

# Function to generate a correlation plot for numeric variables
plot_correlation <- function(dataset) {
  corr_matrix <- correlation_matrix(dataset)
  corrplot(corr_matrix, method = "color")
}

# Function to generate a scatter plot matrix for numeric variables
scatter_plot_matrix <- function(dataset) {
  pairs(dataset)
}

# Example usage:
# Load the dataset
data <- load_dataset("healthcare-dataset-stroke-data.csv")

# Display the head of the dataset
display_head(data)

# Display summary statistics
summary_stats(data)

# Get the dimensions of the dataset
get_dimensions(data)

# Get the column names of the dataset
get_column_names(data)

# Check for missing values
check_missing_values(data)

# Generate descriptive statistics for a numeric variable
numeric_stats(data, "age")

# Generate a histogram for a numeric variable
plot_histogram(data, "age")

# Generate a boxplot for a numeric variable
plot_boxplot(data, "age")

# Generate a bar plot for a categorical variable
plot_barplot(data, "gender")

# Generate a scatter plot for two numeric variables
plot_scatter(data, "age", "income")

# Generate a correlation matrix for numeric variables
correlation_matrix(data)

# Generate a correlation plot
# Function to generate a correlation plot for numeric variables
plot_correlation <- function(dataset) {
  corr_matrix <- correlation_matrix(dataset)
  corrplot(corr_matrix, method = "color")
}

# Function to generate a scatter plot matrix for numeric variables
scatter_plot_matrix <- function(dataset) {
  pairs(dataset)
}

# Example usage:
# Load the dataset
data <- load_dataset("path/to/dataset.csv")

# Display the head of the dataset
display_head(data)

# Display summary statistics
summary_stats(data)

# Get the dimensions of the dataset
get_dimensions(data)

# Get the column names of the dataset
get_column_names(data)

# Check for missing values
check_missing_values(data)

# Generate descriptive statistics for a numeric variable
numeric_stats(data, "age")

# Generate a histogram for a numeric variable
plot_histogram(data, "age")

# Generate a boxplot for a numeric variable
plot_boxplot(data, "age")

# Generate a bar plot for a categorical variable
plot_barplot(data, "category")

# Generate a scatter plot for two numeric variables
plot_scatter(data, "age", "income")

# Generate a correlation matrix for numeric variables
correlation_matrix(data)

# Generate a correlation plot
plot_correlation(data)

# Generate a scatter plot matrix for numeric variables
scatter_plot_matrix(data)



# _____________________ EDA 2 ___________________________

library(tidyverse)
library(reshape2)
library(corrplot)

# Function to load the data
load_data <- function(file_path) {
  data <- read.csv(file_path)
  return(data)
}

# Function to display the head of the data
display_head <- function(data, n = 5) {
  cat("First", n, "rows of the data:\n")
  head(data, n)
}

# Function to get the shape of the data
get_shape <- function(data) {
  cat("Shape of the data:\n")
  dim(data)
}

# Function to check for missing values
check_missing_values <- function(data) {
  cat("Number of missing values:\n")
  colSums(is.na(data))
}

# Function to drop missing values
drop_missing_values <- function(data) {
  data <- na.omit(data)
  return(data)
}

# Function to get the data types of the columns
get_data_types <- function(data) {
  cat("Data types of the columns:\n")
  sapply(data, class)
}

# Function to get the summary statistics of the numerical columns
summary_stats_numeric <- function(data) {
  cat("Summary statistics of the numerical columns:\n")
  summary(data %>% select_if(is.numeric))
}

# Function to get the unique values and their frequencies for categorical columns
unique_values_categorical <- function(data) {
  cat("Unique values and their frequencies for categorical columns:\n")
  for (column in names(data)[sapply(data, is.character)]) {
    cat(column, ":\n")
    print(table(data[, column]))
    cat("\n")
  }
}

# Function to plot histograms of the numerical columns
plot_histograms <- function(data) {
  cat("Histograms of the numerical columns:\n")
  data %>% select_if(is.numeric) %>% 
    gather() %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 20, fill = "lightblue", color = "white") +
    facet_wrap(~key, scales = "free") +
    theme_classic() +
    labs(x = "Value", y = "Count")
}

# Function to plot box plots of the numerical columns
plot_boxplots <- function(data) {
  cat("Box plots of the numerical columns:\n")
  data %>% select_if(is.numeric) %>% 
    gather() %>% 
    ggplot(aes(x = key, y = value)) +
    geom_boxplot(fill = "lightblue", color = "white") +
    facet_wrap(~key, scales = "free") +
    theme_classic() +
    labs(x = "", y = "Value")
}

# Function to plot correlation matrix of the numerical columns
plot_correlation_matrix <- function(data) {
  cat("Correlation matrix of the numerical columns:\n")
  corr_matrix <- cor(data %>% select_if(is.numeric))
  corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black", addCoef.col = "black", diag = FALSE)
}

# Function to plot scatter plots of the numerical columns against the target variable
plot_scatter_plots <- function(data, target_column) {
  cat("Scatter plots of the numerical columns against the target variable:\n")
  numerical_columns <- names(data)[sapply(data, is.numeric) & names(data) != target_column]
  data %>% select(c(numerical_columns, target_column)) %>% 
    gather(-target) %>% 
    ggplot(aes_string(x = target_column, y = "value", color = "key")) +
    geom_point() +
    facet_wrap(~key, scales = "free") +
    theme_classic() +
    labs(x = target_column, y = "Value")
}

# Example usage:
# Load the data
data <- load_data("data.csv")

# Display the head of the data
display_head(data)

# Get the shape of the data
get_shape(data)

# Check for missing values
check_missing_values(data)

# Drop missing values
data <- drop_missing_values(data)

# Get the data types of the columns
get_data_types(data)

# Get the summary statistics of the numerical columns
summary_stats_numeric(data)

# Get the unique values and their frequencies for categorical columns
unique_values_categorical(data)

# Plot histograms of the numerical columns
plot_histograms(data)

# Plot box plots of the numerical columns
plot_boxplots(data)

# Plot correlation matrix of the numerical columns
plot_correlation_matrix(data)

# Plot scatter plots of the numerical columns against the target variable
plot_scatter_plots(data, "target")
























