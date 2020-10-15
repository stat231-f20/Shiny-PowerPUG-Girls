library(tidyverse)
library(dplyr)
library(shiny)
library(forcats)
library(rgdal)
library(leaflet)



path_in <- "C:/Users/abran/OneDrive/Documents/STAT 231 - Data Science/git/Shiny-PowerPUG-Girls"
data <- read_csv("shiny_project_data.csv")


# Directory data
directory <- read_csv("2014_-_2015_DOE_High_School_Directory.csv")
directory[c("lat", "long")] <- do.call(rbind, lapply(strsplit(directory$`Location 1`, "[()]"), function(col) {
  (parts <- unlist(strsplit(col[2], " ")))
}))
directory$lat <- gsub(",$", "", directory$lat)
directory <- directory %>%
  select("dbn", "lat", "long")

# School data
school_data <- read_csv("shiny_project_data.csv")
school_data_location <- inner_join(directory, school_data, by = "dbn")

# X Choices
x_choices <- as.list(c("reading", "math", "writing", "SAT_score",
                       "academic_expectations", "communication", "engagement",
                       "safety_and_respect", "total_satisfaction", "parent_response_rate", 
                       "teacher_response_rate", "student_response_rate"
                       ))
x_choice_names <- c("SAT Reading Score"
                    , "SAT Math Score"
                    , "SAT Writing Score"
                    , "Total SAT Score",
                    "Academic Expectations",
                    "Communication",
                    "Engagement",
                    "Safety and Respect",
                    "Total Satisfaction", 
                    "Parent Response Rate"
                    , "Teacher Response Rate"
                    , "Student Response Rate")
names(x_choices) <- x_choice_names





names <- c("Academic Expectations", "Communication", "Engagement", "Safety and Respect",
           "Total Satisfaction")

variables <- c("SAT Reading Score"
               , "SAT Math Score"
               , "SAT Writing Score"
               , "Parent Response Rate"
               , "Teacher Response Rate"
               , "Student Response Rate"
               , "Academic Expectations"
               , "Communication"
               , "Engagement"
               , "Safety and Respect"
               , "Total SAT Score"
               , "Total Satisfaction")


# ui 
ui <- fluidPage(
  
  h1("Comparing Standardized Testing Scores and School Satisfaction"),
    
  
      
      tabsetPanel(
                  tabPanel(title = "Bivariate Scatterplot", 
                           
                           selectInput(inputId = "school", 
                                       label = "Choose a school to highlight:",
                                       choices = data$school_name,
                                       selected = "Staten Island Yabc"),
                           selectInput(inputId = "statistic", 
                                       label = "Choose a statistic to compare to Average SAT Score:",
                                       choices = names,
                                       selected = "Total Satisfaction"),
                           plotOutput(outputId = "bivariate"), 
                  
                  
                           
                  
  
                 
                  ),
                  
                  tabPanel(title = "Univariate Box-and-Whisker", 
                           selectInput(inputId = "variable",
                                       label = "Choose a variable to compare to school type",
                                       choices = variables,
                                       selected = "Total SAT Score"),
                           plotOutput(outputId = "univariate")
                  ),
                  tabPanel(title = "Map", 
                           selectInput(inputId = "color_grad"
                                       , label = "Choose by which variable to color the schools:"
                                       , choices = x_choices
                                       , selected = "SAT_score"),
                           leafletOutput(outputId = "map", width = "100%")
                  )
                  
      )
    )
    
  



# server
server <- function(input,output){
  
 
  data_bivariate <- reactive({
    mutate(data, y = case_when(input$statistic == "Academic Expectations" ~ data$academic_expectations,
                               input$statistic == "Communication" ~ data$communication,
                               input$statistic == "Engagement" ~ data$engagement,
                               input$statistic == "Safety and Respect" ~ data$safety_and_respect,
                               input$statistic == "Total Satisfaction" ~ data$total_satisfaction
                               )
          )
  })
  
  data_univariate <- reactive({
    mutate(data, y = case_when(input$variable == "SAT Reading Score" ~ data$reading,
                               input$variable == "SAT Math Score" ~ data$math,
                               input$variable == "SAT Writing Score" ~ data$writing,
                               input$variable == "Parent Response Rate" ~ data$parent_response_rate,
                               input$variable == "Teacher Response Rate" ~ data$teacher_response_rate,
                               input$variable == "Student Response Rate" ~ data$student_response_rate,
                               input$variable == "Academic Expectations" ~ data$academic_expectations,
                               input$variable == "Communication" ~ data$communication,
                               input$variable == "Engagement" ~ data$engagement,
                               input$variable == "Safety and Respect" ~ data$safety_and_respect,
                               input$variable == "Total Satisfaction" ~ data$total_satisfaction,
                               input$variable == "Total SAT Score" ~ data$SAT_score
    )
    )
  })
  
  colorpal <- reactive({
    column <- school_data_location %>% select(input$color_grad)
    palette <- colorNumeric(palette = "YlOrRd", domain = column)
    return(palette)
  })
  
  
  output$bivariate <- renderPlot({
    ggplot(data = data_bivariate(), aes(x = SAT_score, y = y
                                        )
           ) +
      geom_point(aes(colour = case_when(school_name == input$school ~ TRUE,
                                      school_name != input$school ~ FALSE
                                        ),
                     size = case_when(school_name == input$school ~ TRUE,
                                      school_name != input$school ~ FALSE
                                      ),
                     alpha = case_when(school_name == input$school ~ TRUE,
                                      school_name != input$school ~ FALSE
                     )
                     
                    )
                ) + 
      geom_smooth(method='lm'
                  )+
      theme(legend.position = "none"
            ) + 
      labs(
        x = "Average SAT Score (out of 2400)",
        y = input$statistic
          ) + 
      scale_colour_manual(values = c("#ff0000", "#000EFF")
                        ) +
      scale_size_discrete(range = c(2, 5)
                          ) +
      scale_alpha_discrete(range = c(.5, 1)
      )
      
    
  }) # Bivariate plot

  #univariate plot
  output$univariate <- renderPlot({
    data_univariate() %>%
      mutate(class = fct_reorder(school_type, y, .fun='median')) %>%
      ggplot(aes(x=reorder(school_type, y), y=y)) + 
      geom_boxplot(color = 'red') +
      labs(
        x = "School Type",
        y = input$variable
      ) + 
      theme(legend.position="none") 
     
    
  }) 
  
  #Map
  output$map <- renderLeaflet({
    req(input$color_grad)
    
    pal <- colorpal()
    
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = school_data_location,
                       lng = ~as.numeric(long),
                       lat = ~as.numeric(lat),
                       fillColor = ~pal(school_data_location[[input$color_grad]]),
                       radius = 10,
                       stroke = FALSE,
                       fillOpacity = 0.7
      )  %>%
      addLegend(
        pal = colorpal(),
        values = school_data_location[[input$color_grad]],
        opacity = 1,
        position = "bottomright",
        title = x_choice_names[x_choices == input$color_grad]
      )
  })
  
} # server

# call to shinyApp
shinyApp(ui = ui, server = server)
