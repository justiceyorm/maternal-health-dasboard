#Installing packages
install.packages(c(
  "tidyverse",
  "janitor",
  "skimr",
  "lubridate",
  "ggthemes",
  "sf",
  "shiny",
  "DT"
))

#Loading packages
library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)
library(ggthemes)
library(sf)
library(shiny)
library(DT)
library(ggplot2)

#Importing file
maternal_data <- read_csv("maternal_data.csv")

#Checking if file is loaded correctly
glimpse(maternal_data)
View(maternal_data)
head(maternal_data)
tail(maternal_data)

#checking data quality and missing variables
skim(maternal_data)

#Count missing values explicitly
colSums(is.na(maternal_data))

#Fix data types
maternal_data <- maternal_data %>%
  mutate(
    year = as.integer(year),
    region = as.factor(region),
    live_births = as.numeric(live_births),
    maternal_deaths = as.numeric(maternal_deaths),
    skilled_birth_attendance_percent = as.numeric(skilled_birth_attendance_percent),
    antenatal_4plus_visits_percent = as.numeric(antenatal_4plus_visits_percent),
    facility_delivery_percent = as.numeric(facility_delivery_percent)
  )

#fixing missing values with median and saved as maternal_cleaned
maternal_cleaned <- maternal_data %>%
  group_by(region) %>%
  mutate(
    live_births =
      ifelse(is.na(live_births),
             median(live_births, na.rm = TRUE),
             live_births),
    
    maternal_deaths =
      ifelse(is.na(maternal_deaths),
             median(maternal_deaths, na.rm = TRUE),
             maternal_deaths),
    
    skilled_birth_attendance_percent =
      ifelse(is.na(skilled_birth_attendance_percent),
             median(skilled_birth_attendance_percent, na.rm = TRUE),
             skilled_birth_attendance_percent),
    
    antenatal_4plus_visits_percent =
      ifelse(is.na(antenatal_4plus_visits_percent),
             median(antenatal_4plus_visits_percent, na.rm = TRUE),
             antenatal_4plus_visits_percent)
  ) %>%
  ungroup()

#rechecking the cleaned data
colSums(is.na(maternal_cleaned))
View(maternal_cleaned)

#creating maternal mortality ratio mmr
maternal_cleaned <- maternal_data %>%
  mutate(
    mmr = (maternal_deaths / live_births) * 100000
  )

#national trends over time
maternal_cleaned %>%
  group_by(year) %>%
  summarise(
    mean_mmr = mean(mmr, na.rm = TRUE),
    mean_sba = mean(skilled_birth_attendance_percent, na.rm = TRUE)
  )

#regional summary
maternal_cleaned %>%
  group_by(region) %>%
  summarise(
    avg_mmr = mean(mmr, na.rm = TRUE),
    avg_sba = mean(skilled_birth_attendance_percent, na.rm = TRUE),
    avg_facility_delivery = mean(facility_delivery_percent, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_mmr))

#mmr trend over time data visualization
ggplot(maternal_cleaned, aes(x = year, y = mmr)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point") +
  labs(
    title = "Maternal Mortality Ratio Over Time",
    x = "Year",
    y = "MMR per 100,000 live births"
  ) +
  theme_minimal()

#mmr by region data visualization
ggplot(maternal_cleaned, aes(x = region, y = mmr)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Distribution of Maternal Mortality Ratio by Region",
    x = "Region",
    y = "MMR"
  ) +
  theme_minimal()

#service coverage Vs mmr
ggplot(maternal_cleaned,
       aes(x = skilled_birth_attendance_percent, y = mmr)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Skilled Birth Attendance and Maternal Mortality",
    x = "Skilled Birth Attendance (%)",
    y = "MMR"
  ) +
  theme_minimal()
