library(readr)
library(tidyverse)

read_data <- function(filename) {
  return(read_csv(filename))
}

preview_data <- function(data) {
  print("Preview of the data:")
  print(head(data))
}

data_summary <- function(data) {
  summary_data <- summary(data)
  print("Summary of the data:")
  print(summary_data)
}

plot_structure <- function(data) {
  print("Data structure overview:")
  plot_str(data)
}

data_overview <- function(data) {
  overview_data <- ExpData(data)
  print("Data overview:")
  print(overview_data)
}

introduce_data <- function(data) {
  introduction_data <- introduce(data)
  print("Introduction to the data:")
  print(introduction_data)
}

visualize_introduction <- function(data) {
  print("Visualizing data introduction:")
  plot_intro(data)
  plot_intro(data)
}

visualize_missing_profiles <- function(data) {
  print("Visualizing missing profiles:")
  plot_missing(data)
}

plot_bar_chart <- function(column) {
  plot_bar(column)
}

plot_histogram <- function(column) {
  plot_histogram(column)
}

get_selected_variables <- function(data) {
  selected_columns <- data[, c("Type", "Days for shipping (real)", "Days for shipment (scheduled)",
                              "Benefit per order", "Sales", "Order Region", "Order Status",
                              "Shipping Mode")]
  return(selected_columns)
}

summary_selected_variables <- function(data) {
  summary_data <- summary(data)
  print("Summary of the selected variables:")
  print(summary_data)
}

descriptive_visualization <- function(data) {
  ggplot(data = data, mapping = aes(x = `Days for shipping (real)`, y = `Days for shipment (scheduled)`)) +
    geom_point()
  plot_bar(data$Type, title = 'Payment type')
  plot_bar(data$`Order Region`, title = 'Order Region')
  plot_bar(data$`Order Status`, title = 'Order Status')
  plot_bar(data$`Shipping Mode`, title = 'Shipping mode')
  plot_histogram(data$`Days for shipping (real)`, ggtheme = theme_linedraw(), title = "Days for shipping (real)")
  plot_histogram(data$`Days for shipment (scheduled)`, ggtheme = theme_linedraw(), title = "Days for shipment")
  plot(factor(data$Type), data$`Days for shipping (real)`, xlab = "Payment Type", ylab = "Days for shipping")
  ggplot(data) + geom_boxplot(aes(x = Type, y = Sales))
  ggplot(data) + geom_point(aes(x = Sales, y = `Days for shipping (real)`, color = Type))
  ggplot(data) + geom_point(aes(x = `Type`, y = Sales, color = `Shipping Mode`)) +
    scale_x_discrete(guide = guide_axis(angle = 90))
  ggplot(data) + geom_line(aes(x = `Shipping Mode`, y = `Days for shipping (real)`, color = `Order Status`)) +
    scale_x_discrete(guide = guide_axis(angle = 90))
}

visualize_delivery_delay <- function(data) {
  delay_days <- data$`Days for shipment (scheduled)` - data$`Days for shipping (real)`
  print("Visualizing delivery delay:")
  plot_histogram(delay_days)
}

group_and_summarize <- function(data, group_by_column, summarise_columns) {
  grouped_data <- data %>% group_by({{ group_by_column }}) %>%
    summarise(across(all_of(summarise_columns), sum),
              .groups = 'drop')
  return(grouped_data)
}

visualize_grouped_data <- function(grouped_data, x_column, y_column) {
  ggplot(grouped_data) + geom_point(aes(x = {{ x_column }}, y = {{ y_column }})) +
    scale_x_discrete(guide = guide_axis(angle = 90))
}

# Read the data
df2 <- read_data('DataCoSupplyChainDataset.csv')

# Preview the data
preview_data(df2)

# Find the summary of the data
data_summary(df2)

# Display the data structure overview
plot_structure(df2)

# Overview of the data
df2_overv <- data_overview(df2)

# Exploratory data analysis - Introduction
df2_intro <- introduce_data(df2)
df2_intro

# Visualizing df_intro
visualize_introduction(df2_intro)

# Visualize missing profiles
visualize_missing_profiles(df2)

# Bar chart
plot_bar_chart(df2$`Days for shipping (real)`)
plot_bar_chart(df2$`Shipping Mode`)
plot_bar_chart(df2$`Delivery Status`)

# Get selected variables for analysis
df2_col <- get_selected_variables(df2)
View(df2_col)

# Summary of the new data
summary_selected_variables(df2_col)

# Descriptive visualization
descriptive_visualization(df2_col)

# Visualize delivery delay
visualize_delivery_delay(df2_col)

# Grouping the data by region then summarise by sales
df2_col_grp_region <- group_and_summarize(df2_col, `Order Region`, c("Sales", "Benefit per order"))
View(df2_col_grp_region)

# Visualize the grouped data by region
visualize_grouped_data(df2_col_grp_region, `Order Region`, "total_sales")

# Grouping the data by shipping mode then summarise by sales
df2_col_grp_mod <- group_and_summarize(df2_col, `Shipping Mode`, c("Sales", "Benefit per order"))
View(df2_col_grp_mod)

# Visualize the grouped data by shipping mode
visualize_grouped_data(df2_col_grp_mod, `Shipping Mode`, "total_sales")
