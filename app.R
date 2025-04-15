# Shiny_App
# Author: Ben Laufer
# Date: 2023-04-19

# ---- Setup ----
knitr::opts_chunk$set(echo = TRUE)

# ---- Libraries ----
library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(geojsonio)
library(plotly)
library(maps)
library(readr)
library(here)

# ---- Read in Data ----
survey <- read_csv(here::here("data/survey.csv"))

# ---- View Data ----
Gender <- survey |> 
  select(Gender) |> 
  filter(Gender != "Male" & Gender != "Female") |> 
  filter(!duplicated(Gender))

Age <- survey |> 
  select(Age) |> 
  filter(Age < 5 | Age > 122)

# ---- Cleaning ----
clean_data <- survey %>%
  mutate(
    Gender = case_when(
      Gender %in% c("Male", "M", "male", "m", "maile", "Cis Male", "Mal", "Male (CIS)", "Make", "Man", "msle", "Mail", "cis male", "Malr", "Cis Man") ~ "Male",
      Gender %in% c("Female", "female", "F", "Woman", "f", "Femake", "woman", "cis-female/femme", "Female (cis)", "femail") ~ "Female",
      Gender %in% c("Male-ish", "something kinda male?", "male leaning androgynous", "Guy (-ish) ^_^", "ostensibly male, unsure what that really means") ~ "Leaning Male",
      Gender == "queer/she/they" ~ "Leaning Female",
      Gender %in% c("Trans-female", "Trans woman", "Female (trans)") ~ "Trans Female", 
      Gender %in% c("non-binary", "All", "Enby", "fluid", "Genderqueer", "Androgyne", "Agender", "queer") ~ "Nonbinary",
      TRUE ~ "Did Not Specify")) |> 
  mutate(Age = ifelse(Age < 18 | Age > 100, NA, Age),
         across(.cols = self_employed:obs_consequence, .fns = ~ as.factor(.x))) |> 
  select(Age:obs_consequence)

#saveRDS(clean_data, file = "clean_data.RDS")

# ---- Load Clean Data ----
clean_data <- readRDS(here::here("clean_data.RDS"))

survey_questions <- data.frame(
  Variable_Name = c("Age", "Gender", "Country", "state", "self_employed", "family_history", 
                    "treatment", "work_interfere", "no_employees", "remote_work", 
                    "tech_company", "benefits", "care_options", "wellness_program", 
                    "seek_help", "anonymity", "leave", "mental_health_consequence", 
                    "phys_health_consequence", "coworkers", "supervisor", 
                    "mental_health_interview", "phys_health_interview", 
                    "mental_vs_physical", "obs_consequence"),
  Variable_Explanation = c(NA, NA, NA, NA, "Are you self-employed?", 
                           "Do you have a family history of mental illness?", 
                           "Have you sought treatment for a mental health condition?", 
                           "If you have a mental health condition, do you feel that it interferes with your work?", 
                           "How many employees does your company or organization have?", 
                           "Do you work remotely (outside of an office) at least 50% of the time?", 
                           "Is your employer primarily a tech company/organization?", 
                           "Does your employer provide mental health benefits?", 
                           "Do you know the options for mental health care your employer provides?", 
                           "Has your employer ever discussed mental health as part of an employee wellness program?", 
                           "Does your employer provide resources to learn more about mental health issues and how to seek help?", 
                           "Is your anonymity protected if you choose to take advantage of mental health or substance abuse treatment resources?", 
                           "How easy is it for you to take medical leave for a mental health condition?", 
                           "Do you think that discussing a mental health issue with your employer would have negative consequences?", 
                           "Do you think that discussing a physical health issue with your employer would have negative consequences?", 
                           "Would you be willing to discuss a mental health issue with your coworkers?", 
                           "Would you be willing to discuss a mental health issue with your direct supervisor(s)?", 
                           "Would you bring up a mental health issue with a potential employer in an interview?", 
                           "Would you bring up a physical health issue with a potential employer in an interview?", 
                           "Do you feel that your employer takes mental health as seriously as physical health?", 
                           "Have you heard of or observed negative consequences for coworkers with mental health conditions in your workplace?"))

# ---- Cleaning for Map ----
state_lookup <- setNames(state.name, state.abb)
clean_data_states <- clean_data
clean_data_states$state_full <- state_lookup[as.character(clean_data$state)]
clean_data_states <- clean_data_states[complete.cases(clean_data_states$state_full), ]

state.center <- as.data.frame(state.center)
state.center$state_full <- state.name
clean_data_merged <- merge(state.center, clean_data_states, by = "state_full")

# ---- UI ----
ui <- fluidPage(
  titlePanel("My Shiny App"),
  navbarPage("Header Selections",
             tabPanel("ggplot",
                      sidebarPanel(
                        selectInput("country", "Select country:", choices = c("All", unique(clean_data$Country))),
                        selectInput("gender", "Select gender", choices = c("All", unique(clean_data$Gender))),
                        selectInput("variable", "Select variable:", choices = unique(names(clean_data)[4:ncol(clean_data)])),
                        selectInput("plot", "Select plot type:", choices = c("dot plot", "bar graph")),
                        dataTableOutput("questions_table")
                      ),
                      mainPanel(
                        dataTableOutput("table"),
                        plotOutput("ggplot")
                      )),
             tabPanel("USA Map",
                      sidebarPanel(
                        selectInput("map_gender", "Select gender:", choices = c("All", unique(clean_data$Gender))),
                        selectInput("map_variable", "Select variable:", choices = unique(names(clean_data)[3:ncol(clean_data)]))
                      ),
                      mainPanel(leafletOutput("map"))
             ),
             tabPanel("Plotly Graph",
                      sidebarPanel(
                        selectInput("plotly_variable", "Select variable:", choices = unique(names(clean_data)[2:ncol(clean_data)]))
                      ),
                      mainPanel(plotlyOutput("plotly"))
             )
  )
)

# ---- Server ----
server <- function(input, output) {
  
  clean_data_filtered <- reactive({
    if (input$gender == "All" && input$country != "All") {
      filter(clean_data, Country == input$country)
    } else if (input$gender != "All" && input$country != "All") {
      filter(clean_data, Country == input$country, Gender == input$gender)
    } else if (input$gender != "All") {
      filter(clean_data, Gender == input$gender)
    } else {
      clean_data
    }
  })
  
  clean_data_summary <- reactive({
    clean_data_filtered() %>%
      group_by(!!sym(input$variable)) %>%
      summarize(Freq = n()) %>%
      mutate(Percentage = Freq / sum(Freq) * 100)
  })
  
  output$table <- renderDataTable({
    clean_data_summary() |> arrange(desc(Freq))
  })
  
  output$questions_table <- renderDataTable({
    survey_questions
  }, options = list(pageLength = 5, dom = 'pt'))
  
  output$ggplot <- renderPlot({
    if (input$plot == "bar graph") {
      ggplot(clean_data_summary(), aes(x = reorder(!!sym(input$variable), -Percentage), y = Percentage)) +
        geom_bar(stat = "identity", fill = "navy") +
        labs(x = input$variable)
    } else {
      ggplot(clean_data_summary(), aes(x = reorder(!!sym(input$variable), -Percentage), y = Percentage)) +
        geom_point(color = "navy", size = 6) +
        labs(x = input$variable)
    }
  })
  
  output$plotly <- renderPlotly({
    plot_ly(data = clean_data_filtered(),
            type = 'scatter', mode = 'markers',
            x = ~Age,
            y = as.formula(paste0("~", input$plotly_variable)),
            color = ~Country,
            text = ~Country,
            hovertemplate = paste(
              "<b>%{text}</b><br><br>",
              "Age: %{x}<br>",
              "Response: %{y}<br>",
              "<extra></extra>"
            ))
  })
  
  clean_data_map_filtered <- reactive({
    if (input$gender == "All" && input$country != "All") {
      filter(clean_data_merged, Country == input$country)
    } else if (input$gender != "All" && input$country != "All") {
      filter(clean_data_merged, Country == input$country, Gender == input$gender)
    } else if (input$gender != "All") {
      filter(clean_data_merged, Gender == input$gender)
    } else {
      clean_data_merged
    }
  })
  
  clean_data_map_summary <- reactive({
    clean_data_map_filtered() %>%
      group_by(!!sym(input$map_variable)) %>%
      summarize(Freq = n()) %>%
      mutate(Percentage = Freq / sum(Freq) * 100)
  })
  
  output$map <- renderLeaflet({
    leaflet(data = clean_data_map_summary()) %>%
      setView(-96, 37.8, 4) %>%
      addTiles() %>%
      addCircles(lng = clean_data_merged$x, lat = clean_data_merged$y, weight = 1,
                 radius = 15000 * sqrt(clean_data_map_summary()$Percentage))
  })
}

# ---- Run App ----
shinyApp(ui = ui, server = server)
