library(tidyverse)
library(dplyr)

path_in <- "C:/Users/abran/OneDrive/Documents/STAT 231 - Data Science/git/Shiny-PowerPUG-Girls"
data <- read_csv(paste0(path_in,"/shiny_project_data.csv"))









# ui 
ui <- fluidPage(
  
  h1("Ranking Popular Candies"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "name", 
                  label = "Choose a candy to highlight:",
                  choices = candy$brand,
                  selected = "M&M's"),
      
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs", 
                  tabPanel("Popularity", 
                           plotOutput(outputId = "pop")), 
                  tabPanel("Sugar Content", 
                           plotOutput(outputId = "sugar")),
                  tabPanel("Price", 
                           plotOutput(outputId = "price")),
                  tabPanel("Candy Information",
                           htmlOutput(outputId = "info")
                  )
      )
    )
  )
)


# server
server <- function(input,output){
  
  # Reorder datasets so that charts are shown in increasing order
  candy_win <- reactive({
    mutate(candy, brand = fct_reorder(brand, winpercent))
  })
  
  candy_sugar <- reactive({
    mutate(candy, brand = fct_reorder(brand, sugarpercent))
  })
  
  candy_price <- reactive({
    mutate(candy, brand = fct_reorder(brand, pricepercent))
  })
  
  # Filter dataset so that info graphics for single cnady can be shown
  candy_info <- reactive({
    filter(candy, brand == input$name)
  })
  
  output$pop <- renderPlot({
    ggplot(data = candy_win(), aes(x = brand, y = winpercent)) +
      geom_col(aes(fill = case_when(brand == input$name ~ TRUE,
                                    brand != input$name ~ FALSE
      )
      )
      ) + 
      theme(legend.position = "none"
      ) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)
      ) + 
      labs(
        y = "Popularity (Winning Percentage over Other Candies)",
        x = "Brand of Candy"
      ) + 
      scale_fill_manual(values = c("#D35400", "#6E2C00"))
  })
  
  output$sugar <- renderPlot({
    ggplot(data = candy_sugar(), aes(x = brand, y = sugarpercent)) +
      geom_col(aes(fill = case_when(brand == input$name ~ TRUE,
                                    brand != input$name ~ FALSE
      )
      )
      ) + 
      theme(legend.position = "none"
      ) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)
      ) + 
      labs(
        y = "Sugar Content (Percentile out of all Candies)",
        x = "Brand of Candy"
      ) + 
      scale_fill_manual(values = c("#2ECC71", "#186A3B"))
  })
  
  
  output$price <- renderPlot({
    ggplot(data = candy_price(), aes(x = brand, y = pricepercent)) +
      geom_col(aes(fill = case_when(brand == input$name ~ TRUE,
                                    brand != input$name ~ FALSE
      )
      )
      ) + 
      theme(legend.position = "none"
      ) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)
      ) + 
      labs(
        y = "Price (Percentile out of all Candies)",
        x = "Brand of Candy"
      ) + 
      scale_fill_manual(values = c("#3498DB", "#1B4F72"))
  })
  
  
  
  output$info <- renderText({
    
    line0 <- ""
    line1 <- paste("Candy Statistics for ", input$name, ":")
    line2 <- paste("Popularity (Winning Percentage over Other Candies): ", 
                   toString(candy_info()$winpercent))
    line3 <- paste("Sugar Content (Percentile out of all Candies): ", 
                   toString(candy_info()$sugarpercent))
    line4 <- paste("Price (Percentile out of all Candies): ", 
                   toString(candy_info()$pricepercent))
    line5 <- paste("Does it contain chocolate? ", 
                   candy_info()$chocolate)
    line6 <- paste("Is it fruit flavored? ", 
                   candy_info()$fruity)
    line7 <- paste("Is there caramel in the candy? ", 
                   candy_info()$caramel)
    line8 <- paste("Does it contain peanuts, peanut butter or almonds? ", 
                   candy_info()$peanutyalmondy)
    line9 <- paste("Does it contain nougat? ", 
                   candy_info()$nougat)
    line10 <- paste("Does it contain crisped rice, wafers, or a cookie component? ", 
                    candy_info()$crispedricewafer)
    line11 <- paste("Is it a hard candy? ", 
                    candy_info()$hard)
    line12 <- paste("Is it a candy bar? ", 
                    candy_info()$bar)
    line13 <- paste("Is it one of many candies in a bag or box? ", 
                    candy_info()$pluribus)
    
    HTML(paste(line0, line1, line0, line2, line0, line3, line0, line4, line0, 
               line5, line0, line6, line0, line7, line0, line8, line0, line9, 
               line0, line10, line0, line11, line0, line12, line0, line13, 
               sep = "<br/>"))
    
    
    
  })
}

# call to shinyApp
shinyApp(ui = ui, server = server)
