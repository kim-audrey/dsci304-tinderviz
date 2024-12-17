required_packages <- c("shiny", "dplyr", "tidyr", "ggplot2", "gganimate", "plotly", "lubridate")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

# Load libraries
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(plotly)
library(lubridate)
# Data Preparation ------------------------------------------------------------
# Clean up date fields

data_cleaned <- readRDS("data_cleaned.rds")
data_no_outliers <- readRDS("data_no_outliers.rds")



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

# UI ---------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .container {
        margin-left: auto;
        margin-right: auto;
        max-width: 80%; /* 10% margins on each side */
      }
      h3, h6, .shiny-output-error { /* Optional: Center headers */
        text-align: center;
      }
      img {
        display: block; 
        margin-left: auto;
        margin-right: auto;
      }
    "))
  ),
  
  div(class="container",
      titlePanel("Tinder Dat(a)ing Trends"),
      
      # Section 1: Average Ghostings by Horoscope
      h3("Filter Out Pisces: Ghosting Behaviour by Horoscope?"),
      h6("\"Ghosting\" is defined in the dataset as when the match starts the conversation and the user doesn't reply"),
      plotlyOutput("horoscope_plot"),
      
      # Section App Opens Over Time
      h3("Taking off in Covid: Amount of Times Users Opened Tinder per Month from 2015 - 2020"),
      tags$img(src = "https://media2.giphy.com/media/v1.Y2lkPTc5MGI3NjExMzdicnh0YmZiOXhpcnlqNXhrNm8wZXFrazczcTIyYWg5b3k5YXBqYyZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/uYAl0rbVLYYH9ZARQq/giphy.gif"),
      
      # Section 3: Boxplot of Age Differences
      h3("What Ages Do People Of Different Ages Want to Date?"),
      plotlyOutput("age_boxplot", height = "900px"),
      
      # Section 4: Matches per User by Sexuality
      h3("Who's Texting? Median Conversation Length by Total Matches (By Sexuality)"),
      selectInput(
        inputId = "selected_sexuality",
        label = "Select Sexuality Group:",
        choices = c("All", "Male for Male", "Female for Female", "Male for Female", 
                    "Female for Male", "Bi Male", "Bi Female"),
        selected = "All"
      ),
      plotlyOutput("conversation_length_plot", height = "500px"),
      
      
      h3("Who's Texting? Percentage of One-Message Conversations by Age"),
      plotlyOutput("one_message_plot", height = "500px")
  )
)

# Server -----------------------------------------------------------------------
server <- function(input, output) {
  # 1. Average Ghostings by Horoscope
  output$horoscope_plot <- renderPlotly({
    horoscope_averages <- aggregate(
      conversationsMeta.nrOfGhostingsAfterInitialMessage ~ horoscope,
      data = data_cleaned,
      FUN = mean,
      na.rm = TRUE
    )
    colnames(horoscope_averages) <- c("horoscope", "avg_ghostings")
    p <- ggplot(horoscope_averages, aes(x = reorder(horoscope, -avg_ghostings), y = avg_ghostings)) +
      geom_bar(stat = "identity", fill = "lightblue", color = "black") +
      labs(title = "Average Ghostings by Horoscope", x = "Horoscope", y = "Average Ghostings") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  # 3. Boxplot of Age Differences
  output$age_boxplot <- renderPlotly({
    ggplot_object <- ggplot(data_no_outliers, aes(x = as.factor(age), y = mean_age_difference)) +
      geom_boxplot(fill = "lightblue", outlier.shape = NA) +
      geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 1) +
      facet_wrap(~ age_group, scales = "free_x", ncol = 1) +
      labs(
        title = "Age Filter Preferences Relative to User Age by Age Group (No Outliers)",
        x = "User Age",
        y = "Mean Age Difference"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12, face = "bold")
      )
    
    ggplotly(ggplot_object)  # Make it interactive
  })
  
  output$sexuality_plot <- renderPlot({
    # Step 1: Filter and Clean Data
    data_with_sexuality <- data_cleaned %>%
      filter(
        !is.na(user.gender), 
        user.gender != "", 
        !is.na(user.interestedIn), 
        user.interestedIn != ""
      ) %>%
      mutate(
        sexuality = case_when(
          user.gender == "M" & user.interestedIn == "M" ~ "Male for Male",
          user.gender == "F" & user.interestedIn == "F" ~ "Female for Female",
          user.gender == "M" & user.interestedIn == "F" ~ "Male for Female",
          user.gender == "F" & user.interestedIn == "M" ~ "Female for Male",
          user.gender == "M" & user.interestedIn == "M and F" ~ "Bi Male",
          user.gender == "F" & user.interestedIn == "M and F" ~ "Bi Female",
          TRUE ~ "Unknown"
        )
      )
    # Step 2: Calculate Total Matches per Group
    data_with_matches <- data_with_sexuality %>%
      mutate(
        total_matches = rowSums(across(starts_with("matches.")), na.rm = TRUE)
      )
    
    # Step 3: Summarize Total Matches and Group Sizes
    matches_summary <- data_with_matches %>%
      group_by(sexuality) %>%
      summarise(
        total_matches = sum(total_matches, na.rm = TRUE),   # Sum of all matches
        group_size = n(),                                  # Count of users in the group
        matches_per_user = total_matches / group_size      # Normalize matches per user
      ) %>%
      ungroup()
    
    # Step 4: Plot Matches Per User by Sexuality
    ggplot(matches_summary, aes(x = reorder(sexuality, -matches_per_user), y = matches_per_user, fill = sexuality)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Matches Per User by Sexuality Group (Normalized)",
        x = "Sexuality",
        y = "Matches Per User",
        fill = "Sexuality"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)
      )
  })
  
  # Median Conversation Length Scatter Plot by Sexuality
  output$conversation_length_plot <- renderPlotly({
    # Step 1: Clean and Prepare Data
    data_with_sexuality <- data_cleaned %>%
      filter(
        !is.na(conversationsMeta.medianConversationLength),
        !is.na(user.gender),
        !is.na(user.interestedIn)
      ) %>%
      mutate(
        sexuality = case_when(
          user.gender == "M" & user.interestedIn == "M" ~ "Male for Male",
          user.gender == "F" & user.interestedIn == "F" ~ "Female for Female",
          user.gender == "M" & user.interestedIn == "F" ~ "Male for Female",
          user.gender == "F" & user.interestedIn == "M" ~ "Female for Male",
          user.gender == "M" & user.interestedIn == "M and F" ~ "Bi Male",
          user.gender == "F" & user.interestedIn == "M and F" ~ "Bi Female",
          TRUE ~ "Unknown"
        ),
        total_matches = rowSums(across(starts_with("matches.")), na.rm = TRUE)
      ) %>%
      filter(total_matches > 0, sexuality != "Unknown")
    
    # Step 2: Filter Data Based on User Input
    filtered_data <- if (input$selected_sexuality == "All") {
      data_with_sexuality
    } else {
      data_with_sexuality %>% filter(sexuality == input$selected_sexuality)
    }
    
    # Step 3: Create Scatter Plot
    scatter_plot <- ggplot(filtered_data, aes(
      x = total_matches,
      y = conversationsMeta.medianConversationLength
    )) +
      geom_point(alpha = 0.7, color = "steelblue") +
      labs(
        title = paste("Median Conversation Length vs. Total Matches\nSexuality Group:", input$selected_sexuality),
        x = "Total Matches",
        y = "Median Conversation Length"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Step 4: Make Interactive with Plotly
    ggplotly(scatter_plot)
  })
  
  output$one_message_plot <- renderPlotly({
    p <- ggplot(age_conversation_summary, aes(x = age, y = percent_one_message)) +
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
    ggplotly(p)  # Make it interactive with plotly
  })
  
}

# Run the Shiny App ------------------------------------------------------------
shinyApp(ui = ui, server = server)
