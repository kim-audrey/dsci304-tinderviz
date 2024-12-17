library(ggplot2)
library(gganimate)
library(dplyr)
library(lubridate)

appOpens = subset(data_cleaned, select=names(data_cleaned)[grepl("appOpens", names(data_cleaned))])

# Convert to long format
appOpens_long <- appOpens %>%
  pivot_longer(cols = everything(), names_to = "date", values_to = "app_opens") %>%
  mutate(
    date = gsub("appOpens\\.", "", date),  # Remove "appOpens." prefix
    date = as.Date(date, format = "%Y.%m.%d")  # Convert to proper Date format
  ) %>%
  filter(year(date) > 2014 & year(date) < 2021) # we don't have all the months for these years, so take them out


appOpens_monthly <- appOpens_long %>%
  mutate(
    year = year(date),
    month = month(date, label = TRUE, abbr = TRUE)  # Extract month name
  ) %>%
  group_by(year, month) %>%
  summarise(total_app_opens = sum(app_opens, na.rm = TRUE)) %>%
  ungroup() 

# Create the animated bar chart
animated_plot <- ggplot(appOpens_monthly, aes(x = month, y = total_app_opens, fill = month)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Monthly App Opens Over Time ({closest_state})",
    x = "Month",
    y = "Total App Opens",
    fill = "Month"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  transition_states(year, transition_length = 2, state_length = 1) +
  ease_aes('linear')  # Smooth transitions

# Render the animation
animate(animated_plot, nframes = 100, fps = 10)

anim_save("app_opens_animation.gif", animate(animated_plot, nframes = 100, fps = 10))
