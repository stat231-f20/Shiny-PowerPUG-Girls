library(tidyverse)
library(dplyr)
library(shiny)

path_in <- "C:/Users/abran/OneDrive/Documents/STAT 231 - Data Science/git/Shiny-PowerPUG-Girls"
data <- read_csv(paste0(path_in,"/shiny_project_data.csv"))


data2 <- data %>% 
  mutate(school_name = as.factor(school_name)
         )






# ui 
ui <- fluidPage(
  
  h1("Comparing Standardized Testing Scores and Student, Parent and Teacher Satisfaction"),
    
  
      
      tabsetPanel(
                  tabPanel(title = "Bivariate Scatterplot", 
                           
                           selectInput(inputId = "school", 
                                       label = "Choose a school to highlight:",
                                       choices = data2$school_name,
                                       selected = "Staten Island Yabc"),
                           plotOutput(outputId = "bivariate"), 
                  
                  
                           
                  
                  
                 # selectInput(inputId = "statistic", 
                          #    label = "Choose a statistic to compare to Average SAT Score:",
                              
                         #     choices = candy$brand,
                          #    selected = "M&M's"),
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
  
 
  
  output$bivariate <- renderPlot({
    ggplot(data = data2, aes(x = SAT_score, y = total_satisfaction)) +
      geom_point(aes(colour = case_when(school_name == input$school ~ TRUE,
                                      school_name != input$school ~ FALSE
      )
      )
      ) + 
      theme(legend.position = "none"
      ) + 
      labs(
        title = "Association between Average SAT Score and Selected Satisfaction Statistic",
        x = "Average SAT Score (out of 2400)"
      ) + 
      scale_fill_manual(values = c("#D35400", "#6E2C00"))
  })
  
  
  
  
    
    
    
 
}

# call to shinyApp
shinyApp(ui = ui, server = server)
