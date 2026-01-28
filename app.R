# Load required packages
library(shiny)
library(tidyverse)
library(janitor)
library(DT)

# -----------------------------
# DATA IMPORT AND CLEANING
# -----------------------------

maternal_data <- read_csv("maternal_data.csv") %>%
  clean_names() %>%
  mutate(
    year = as.integer(year),
    region = as.factor(region),
    live_births = as.numeric(live_births),
    maternal_deaths = as.numeric(maternal_deaths),
    skilled_birth_attendance_percent = as.numeric(skilled_birth_attendance_percent),
    antenatal_4plus_visits_percent = as.numeric(antenatal_4plus_visits_percent),
    facility_delivery_percent = as.numeric(facility_delivery_percent)
  )

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
  ungroup() %>%
  mutate(
    mmr = (maternal_deaths / live_births) * 100000
  )

# -----------------------------
# USER INTERFACE
# -----------------------------

ui <- fluidPage(
  
  titlePanel("Maternal Health Indicators Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "region",
        label = "Select Region:",
        choices = c("All", levels(maternal_cleaned$region)),
        selected = "All"
      ),
      
      sliderInput(
        inputId = "year",
        label = "Select Year Range:",
        min = min(maternal_cleaned$year, na.rm = TRUE),
        max = max(maternal_cleaned$year, na.rm = TRUE),
        value = c(
          min(maternal_cleaned$year, na.rm = TRUE),
          max(maternal_cleaned$year, na.rm = TRUE)
        ),
        sep = ""
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("MMR Trend", plotOutput("mmr_trend")),
        tabPanel("SBA vs MMR", plotOutput("sba_mmr")),
        tabPanel("Data Table", DTOutput("table"))
      )
    )
  )
)

# -----------------------------
# SERVER LOGIC
# -----------------------------

server <- function(input, output) {
  
  filtered_data <- reactive({
    maternal_cleaned %>%
      filter(
        year >= input$year[1],
        year <= input$year[2],
        if (input$region == "All") TRUE else region == input$region
      )
  })
  
  # MMR trend plot
  output$mmr_trend <- renderPlot({
    ggplot(filtered_data(), aes(x = year, y = mmr)) +
      stat_summary(fun = mean, geom = "line", linewidth = 1) +
      stat_summary(fun = mean, geom = "point") +
      labs(
        title = "Maternal Mortality Ratio Over Time",
        x = "Year",
        y = "MMR per 100,000 live births"
      ) +
      theme_minimal()
  })
  
  # SBA vs MMR plot
  output$sba_mmr <- renderPlot({
    ggplot(filtered_data(),
           aes(x = skilled_birth_attendance_percent, y = mmr)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        title = "Skilled Birth Attendance vs Maternal Mortality",
        x = "Skilled Birth Attendance (%)",
        y = "MMR"
      ) +
      theme_minimal()
  })
  
  # Data table
  output$table <- renderDT({
    datatable(
      filtered_data(),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
}

# -----------------------------
# RUN APP
# -----------------------------

shinyApp(ui = ui, server = server)

#publication link
https://malaria1111.shinyapps.io/maternal_health_analysis/