library(tidyverse)
library(dplyr)
library(shiny)

path_in <- "C:/Users/abran/OneDrive/Documents/STAT 231 - Data Science/git/Shiny-PowerPUG-Girls"
data <- read_csv(paste0(path_in,"/shiny_project_data.csv"))





names <- c("Academic Expectations", "Communication", "Engagement", "Safety and Respect",
           "Total Satisfaction")


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
                           #plotOutput(outputId = "univariate")
                  ),
                  tabPanel(title = "Map", 
                           #plotOutput(outputId = "map")
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
  
  
  output$bivariate <- renderPlot({
    ggplot(data = data_bivariate(), aes(x = SAT_score, y = y
                                        )
           ) +
      geom_point(aes(colour = case_when(school_name == input$school ~ TRUE,
                                      school_name != input$school ~ FALSE
                                        ),
                     alpha = case_when(school_name == input$school ~ TRUE,
                                        school_name != input$school ~ FALSE
                                      ),
                     size = case_when(school_name == input$school ~ TRUE,
                                      school_name != input$school ~ FALSE
                                      )
                    )
                ) + 
      theme(legend.position = "none"
            ) + 
      labs(
        x = "Average SAT Score (out of 2400)",
        y = input$statistic
          ) + 
      scale_fill_manual(values = c("#D35400", "#6E2C00")
                        ) +
      scale_alpha_discrete(range = c(1, 1)
                           ) +
      scale_size_discrete(range = c(2, 5)
                          )
      
    
  }) # Bivariate plot

} # server

# call to shinyApp
shinyApp(ui = ui, server = server)
