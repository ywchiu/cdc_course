#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    covid19 <- read_csv('covid19.csv')

    output$distPlot <- renderPlot({

        data  <- covid19 %>%
            filter((`Country/Region` == input$Country) & (`Case` == input$Case_Type)) %>%
            group_by(`Date`) %>%
            summarise(Case_Sum =  sum(Case_Number) ) %>%
            select(`Date`, `Case_Sum`)

        plot(Case_Sum ~ Date, data = data, type = 'l', col = 'red')

    })

})
