# Load libraries
library(dplyr)
library(lubridate)

# Load the raw data
data <- read.csv("profiles_2021-11-10.csv")

# Pre-process and clean data
data_cleaned <- data %>%
  mutate(
    user.birthDate = as.Date(substr(user.birthDate, 1, 10)),
    user.createDate = as.Date(substr(user.createDate, 1, 10), format = "%Y-%m-%d"),
    age = floor(as.numeric(difftime(as.Date("2021-11-10"), user.birthDate, units = "days")) / 365.25),
    mean_age_difference = ((user.ageFilterMax + user.ageFilterMin) / 2) - age,
    age_group = case_when(
      age >= 20 & age <= 30 ~ "Young Adults",
      age >= 30 & age <= 40 ~ "Middle-Aged Adults",
      age >= 40 & age <= 54 ~ "Older Adults",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(age),
    !is.na(mean_age_difference),
    user.ageFilterMax < 150,
    user.ageFilterMin > 0,
    !is.na(age_group)
  )


# Function to determine zodiac sign
get_zodiac <- function(date) {
  month <- month(date)
  day <- day(date)
  if ((month == 1 & day >= 20) | (month == 2 & day <= 18)) return("Aquarius")
  if ((month == 2 & day >= 19) | (month == 3 & day <= 20)) return("Pisces")
  if ((month == 3 & day >= 21) | (month == 4 & day <= 19)) return("Aries")
  if ((month == 4 & day >= 20) | (month == 5 & day <= 20)) return("Taurus")
  if ((month == 5 & day >= 21) | (month == 6 & day <= 20)) return("Gemini")
  if ((month == 6 & day >= 21) | (month == 7 & day <= 22)) return("Cancer")
  if ((month == 7 & day >= 23) | (month == 8 & day <= 22)) return("Leo")
  if ((month == 8 & day >= 23) | (month == 9 & day <= 22)) return("Virgo")
  if ((month == 9 & day >= 23) | (month == 10 & day <= 22)) return("Libra")
  if ((month == 10 & day >= 23) | (month == 11 & day <= 21)) return("Scorpio")
  if ((month == 11 & day >= 22) | (month == 12 & day <= 21)) return("Sagittarius")
  if ((month == 12 & day >= 22) | (month == 1 & day <= 19)) return("Capricorn")
}
data_cleaned <- data_cleaned %>% mutate(horoscope = sapply(user.birthDate, get_zodiac))

# Remove outliers
data_no_outliers <- data_cleaned %>%
  group_by(age_group, age) %>%
  mutate(
    Q1 = quantile(mean_age_difference, 0.25, na.rm = TRUE),
    Q3 = quantile(mean_age_difference, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  ) %>%
  filter(
    mean_age_difference >= (Q1 - 1.5 * IQR) & mean_age_difference <= (Q3 + 1.5 * IQR)
  ) %>%
  ungroup()

# Save the cleaned data
saveRDS(data_cleaned, "data_cleaned.rds")
saveRDS(data_no_outliers, "data_no_outliers.rds")