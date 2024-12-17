#Clear Workspace
cat("\014")
rm(list=ls())
set.seed(18552)

setwd("C:/Users/audre/Desktop/Tinder Data Set")

data<-read.csv("profiles_2021-11-10.csv")


library(dplyr)
library(lubridate)
library(stargazer)
library(ggplot2)
library(tidyr)
library(gganimate)



age_conversation_summary <- data_cleaned %>%
  group_by(age) %>%
  summarise(
    num_single_message = mean(conversationsMeta.percentOfOneMessageConversations, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 3: Create the Line Graph
ggplot(age_conversation_summary, aes(x = age, y = num_single_message)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Average Percentage of One Message Conversations Length by User Age",
    x = "User Age",
    y = "Percent of one-message Conversations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )







# Step 1: Clean and Prepare Data
data_cleaned <- data_cleaned %>%
  mutate(
    user.birthDate_clean = as.Date(substr(user.birthDate, 1, 10)),
    age = floor(as.numeric(difftime(as.Date("2021-11-10"), user.birthDate_clean, units = "days")) / 365.25),
    one_message_count = (conversationsMeta.percentOfOneMessageConversations / 100) * conversationsMeta.nrOfConversations
  ) %>%
  filter(
    !is.na(age),
    !is.na(one_message_count),
    !is.na(conversationsMeta.nrOfConversations),
    age >= 18 & age <= 70
  )

# Step 2: Summarize Data to Get True Percentages
age_conversation_summary <- data_cleaned %>%
  group_by(age) %>%
  summarise(
    total_one_message_conversations = sum(one_message_count, na.rm = TRUE),
    total_conversations = sum(conversationsMeta.nrOfConversations, na.rm = TRUE),
    percent_one_message = (total_one_message_conversations / total_conversations) * 100
  ) %>%
  ungroup()

# Step 3: Create the Line Graph
ggplot(age_conversation_summary, aes(x = age, y = percent_one_message)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Percentage of One-Message Conversations by User Age",
    x = "User Age",
    y = "Percent of One-Message Conversations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )








# Step 1: Clean and Prepare Data
data_cleaned <- data %>%
  mutate(user.createDate = as.Date(substr(user.createDate, 1, 10), format = "%Y-%m-%d"))

# Step 2: Process Matches Columns into Long Format
matches_long <- data_cleaned %>%
  select(X_id, user.createDate, starts_with("matches")) %>%
  pivot_longer(
    cols = starts_with("matches"), 
    names_to = "match_date", 
    values_to = "matches"
  ) %>%
  mutate(
    # Extract the date from column names
    match_date = gsub("matches\\.", "", match_date),
    match_date = as.Date(match_date, format = "%Y.%m.%d"),
    # Calculate months since account creation
    months_since_create = interval(user.createDate, match_date) %/% months(1)
  ) %>%
  filter(
    !is.na(matches),           # Keep valid matches
    months_since_create >= 0   # Only post-account creation months
  )

# Step 3: Summarize Total Matches Per Month
matches_summary <- matches_long %>%
  group_by(months_since_create) %>%
  summarise(total_matches = sum(matches, na.rm = TRUE), .groups = "drop")

# Step 4: Create Line Graph
p <- ggplot(matches_summary, aes(x = months_since_create, y = total_matches)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Total Matches Per Month Since Account Creation",
    x = "Months Since Account Creation",
    y = "Total Matches"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

# Display the plot
print(p)

#### Transformations on all the data

data_cleaned <- data %>%
  mutate(
    # Remove the timestamp and keep only the date part
    user.birthDate = as.Date(substr(user.birthDate, 1, 10))
  )

# Step 1: Function to process a specific metric
aggregate_monthly <- function(data, metric_prefix) {
  data %>%
    select(X_id, starts_with(metric_prefix)) %>%  # Filter relevant columns
    pivot_longer(
      cols = starts_with(metric_prefix),
      names_to = "metric_date",
      values_to = "metric_value"
    ) %>%
    mutate(
      # Extract date portion using base R gsub (removing the prefix)
      metric_date = gsub(paste0("^", metric_prefix, "\\."), "", metric_date),
      year_month = format(as.Date(metric_date, format = "%Y.%m.%d"), "%Y-%m")  # Convert to YYYY-MM
    ) %>%
    filter(!is.na(metric_value)) %>%  # Remove rows with NA values
    group_by(X_id, year_month) %>%  # Aggregate by user and month
    summarise(total_value = sum(metric_value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = year_month,
      values_from = total_value,
      names_prefix = paste0(metric_prefix, ".")
    )
}

# Step 2: Process swipes.likes, messages.sent, and matches
swipes_summary <- aggregate_monthly(data, "swipes.likes")
messages_summary <- aggregate_monthly(data, "messages.sent")
matches_summary <- aggregate_monthly(data, "matches")

# Step 3: Combine all summaries into the original dataset
data_with_summary <- data %>%
  select(X_id) %>%  # Keep only user ID to start
  left_join(swipes_summary, by = "X_id") %>%
  left_join(messages_summary, by = "X_id") %>%
  left_join(matches_summary, by = "X_id")

# Step 4: View the final dataset
head(data_with_summary)




# Step 1: Clean the user.createDate column
data_cleaned <- data_with_summary %>%
  mutate(user.createDate = as.Date(substr(user.createDate, 1, 10), format = "%Y-%m-%d"))
























#Step 1: Filter swipes.likes columns
swipes_data <- data %>%
  select(X_id, starts_with("swipes.likes"))  # Include user ID and swipes.likes.* columns

# Step 2: Reshape to long format and extract YY-MM
swipes_long <- swipes_data %>%
  pivot_longer(
    cols = starts_with("swipes.likes"),
    names_to = "swipe_date",
    values_to = "swipes"
  ) %>%
  mutate(
    swipe_date = str_extract(swipe_date, "\\d{4}\\.\\d{2}\\.\\d{2}"),  # Extract date from column name
    year_month = format(as.Date(swipe_date, format = "%Y.%m.%d"), "%Y-%m")  # Convert to YY-MM
  ) %>%
  filter(!is.na(swipes))  # Remove rows with NA values for swipes

# Step 3: Aggregate swipes by user and year-month
swipes_aggregated <- swipes_long %>%
  group_by(X_id, year_month) %>%
  summarise(total_swipes = sum(swipes, na.rm = TRUE), .groups = "drop")

# Step 4: Reshape back to wide format
swipes_wide <- swipes_aggregated %>%
  pivot_wider(
    names_from = year_month,
    values_from = total_swipes,
    names_prefix = "swipes.likes."
  )

# Step 5: Join back to the original data
data_with_swipes_summary <- data %>%
  select(-starts_with("swipes.likes")) %>%  # Remove daily swipes.likes to avoid duplication
  left_join(swipes_wide, by = "X_id")

# View the final dataset
head(data_with_swipes_summary)





















































swipe_column_names <- names(data)[grepl("swipes.", names(data))]
print(swipe_column_names)
swipes_long <- data %>%
  select(X_id, starts_with("swipes.likes.")) %>%  # Filter only swipes.likes columns
  pivot_longer(
    cols = starts_with("swipes.likes."),
    names_to = "date",
    values_to = "swipes"
  ) %>%
  mutate(
    date = gsub("swipes\\.likes\\.", "", date),  # Remove "swipes.likes." prefix
    date = as.Date(date, format = "%Y.%m.%d"),  # Convert cleaned string to Date format
    month_year = format(date, "%Y-%m")          # Extract month and year
  )

# Check outputs
head(swipes_long)
summary(swipes_long$date)

# Step 2: Aggregate total swipes per user and month-year combination
swipes_summary <- swipes_long %>%
  group_by(X_id, month_year) %>%
  summarise(total_swipes = sum(swipes, na.rm = TRUE), .groups = "drop")

# Step 3: Identify the most active month-year for each user
most_active_month <- swipes_summary %>%
  group_by(X_id) %>%
  filter(total_swipes == max(total_swipes)) %>%
  slice(1) %>%  # In case of ties, take the first
  ungroup() %>%
  select(X_id, most_active_month = month_year)

# Step 4: Add the most active month-year back to the original dataset
data_with_active_month <- data %>%
  left_join(most_active_month, by = "X_id")

# View the updated dataset
head(data_with_active_month$most_active_month)


head(data_with_active_month$user.createDate)









age_filter_trends <- data %>%
  group_by(user.age) %>%
  summarise(
    avg_ageFilterMin = mean(user.ageFilterMin, na.rm = TRUE),
    avg_ageFilterMax = mean(user.ageFilterMax, na.rm = TRUE)
  )

# Step 2: Create the line chart
ggplot(age_filter_trends, aes(x = user.age)) +
  geom_line(aes(y = avg_ageFilterMin, color = "Age Filter Min"), size = 1) +
  geom_line(aes(y = avg_ageFilterMax, color = "Age Filter Max"), size = 1) +
  labs(
    title = "Age Filter Trends by User Age",
    x = "User Age",
    y = "Average Age Filter",
    color = "Filter Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )



# Step 1: Extract month-year from swipes columns and aggregate total swipes
swipes_by_month <- swipes_long %>%
  group_by(month_year) %>%
  summarise(total_swipes = sum(swipes, na.rm = TRUE))

# Step 2: Create the heatmap
ggplot(swipes_by_month, aes(x = as.factor(month_year), y = 1, fill = total_swipes)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Swipe Distribution by Month",
    x = "Month-Year",
    y = "",
    fill = "Total Swipes"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank()
  )










# Step 1: Clean user.createDate
data_cleaned <- data %>%
  mutate(
    user.createDate = as.Date(substr(user.createDate, 1, 10), format = "%Y-%m-%d")  # Extract date only
  )

# Step 2: Reshape swipe data into long format
swipes_long <- data_cleaned %>%
  select(X_id, user.createDate, starts_with("swipes.")) %>%
  pivot_longer(
    cols = starts_with("swipes."),
    names_to = "swipe_date",
    values_to = "swipes"
  ) %>%
  mutate(
    swipe_date = as.Date(gsub("swipes\\.", "", swipe_date), format = "%Y.%m.%d"),  # Clean swipe date
    months_since_create = interval(user.createDate, swipe_date) %/% months(1)     # Calculate months since creation
  ) %>%
  filter(
    !is.na(swipes),
    months_since_create >= 0  # Only keep valid months after creation
  )

# Step 3: Calculate average swipes per month since account creation
swipes_summary <- swipes_long %>%
  group_by(months_since_create) %>%
  summarise(avg_swipes = mean(swipes, na.rm = TRUE), .groups = "drop")

# Step 4: Create the line plot
ggplot(swipes_summary, aes(x = months_since_create, y = avg_swipes)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Average Swipes Per Month Since Account Creation",
    x = "Months Since Account Creation",
    y = "Average Swipes Per Month"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )



################################################################################


# 30 + 365 + 366 + 3 * 365 + 366 + 102 = 2324
appOpens2014 = subset(data, select=names(data)[grepl("appOpens.2014", names(data))])
appOpens2015 = subset(data, select=names(data)[grepl("appOpens.2015", names(data))])
appOpens2016 = subset(data, select=names(data)[grepl("appOpens.2016", names(data))])
appOpens2017 = subset(data, select=names(data)[grepl("appOpens.2017", names(data))])
appOpens2018 = subset(data, select=names(data)[grepl("appOpens.2018", names(data))])
appOpens2019 = subset(data, select=names(data)[grepl("appOpens.2019", names(data))])
appOpens2020 = subset(data, select=names(data)[grepl("appOpens.2020", names(data))])
appOpens2021 = subset(data, select=names(data)[grepl("appOpens.2021", names(data))])

# checking how many of these are actually NA
na_percentage <- (sum(is.na(appOpens2014$appOpens.2014.11.12)) / nrow(appOpens2014)) * 100

cols_without_appOpens <- names(data)[!grepl("appOpens|matches|messages|swipe", names(data))]
cols_without_appOpens

matches = subset(data, select=names(data)[grepl("matches", names(data))])

messages = subset(data, select=names(data)[grepl("messages", names(data))])
swipes = subset(data, select=names(data)[grepl("swipes", names(data))])



head(matches)
# Extract the column names from the subset
matches_columns <- colnames(matches)
swipes_columns <- colnames(swipes)
messages_columns <- colnames(messages)

# Remove the last 10 characters (the dates) from each column name
matches_columns_clean <- substr(matches_columns, 1, nchar(matches_columns) - 10)
swipes_columns_clean <- substr(swipes_columns, 1, nchar(swipes_columns) - 10)
messages_columns_clean <- substr(messages_columns, 1, nchar(messages_columns) - 10)

# Find unique column names
unique_columns <- unique(matches_columns_clean)
unique_columns <- unique(swipes_columns_clean)
unique_columns <- unique(messages_columns_clean)

unique_columns

library(tidyverse)
library(gganimate)
library(lubridate)






################################################################################


# Step 1: Clean and reshape the data
data_cleaned <- data %>%
  mutate(
    user.createDate = as.Date(substr(user.createDate, 1, 10), format = "%Y-%m-%d")  # Extract date only
  )

# Combine swipes, matches, and messages into long format
activity_long <- data_cleaned %>%
  select(X_id, user.createDate, starts_with("swipes.likes."), starts_with("matches."), starts_with("messages.sent.")) %>%
  pivot_longer(
    cols = starts_with("swipes.likes.") | starts_with("matches.") | starts_with("messages.sent."),
    names_to = "activity_date",
    values_to = "activity_value"
  ) %>%
  mutate(
    # Extract the date from column names and calculate months since account creation
    activity_type = case_when(
      grepl("swipes.likes.", activity_date) ~ "Swipes",
      grepl("matches", activity_date) ~ "Matches",
      grepl("messages.sent", activity_date) ~ "Messages"
    ),
    activity_date = as.Date(gsub(".*\\.", "", activity_date), format = "%Y.%m.%d"),  # Clean activity date
    months_since_create = interval(user.createDate, activity_date) %/% months(1)    # Months since account creation
  ) %>%
  filter(
    !is.na(activity_value),
    months_since_create >= 0  # Only keep valid months post-account creation
  )




# Step 2: Summarize average activity by months since account creation
activity_summary <- activity_long %>%
  group_by(months_since_create, activity_type) %>%
  summarise(avg_activity = mean(activity_value, na.rm = TRUE), .groups = "drop")

# Step 3: Create the line plot
ggplot(activity_summary, aes(x = months_since_create, y = avg_activity, color = activity_type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "User Activity Over Time Post-Account Creation",
    x = "Months Since Account Creation",
    y = "Average Activity",
    color = "Activity Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
