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
library(plotly)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    covid19 <- read_csv('covid19.csv')

    output$distPlot <- renderPlotly({

        data  <- covid19 %>%
            filter((`Country/Region` == input$Country) & (`Case` == input$Case_Type)) %>%
            group_by(`Date`) %>%
            summarise(Case_Sum =  sum(Case_Number) , .groups = 'drop' ) %>%
            select(`Date`, `Case_Sum`)

        fig <- ggplot(data)
        g <- fig +
            aes(x = Date, y = Case_Sum) +
            geom_line() +
            xlab('日期') +
            ylab('個案數量')
        ggplotly(g)
        #plot(Case_Sum ~ Date, data = data, type = 'l', col = 'red')

    })

    output$table <- renderTable({
        data  <- covid19 %>%
            filter((`Country/Region` == input$Country) & (`Case` == input$Case_Type)) %>%
            group_by(`Date`) %>%
            summarise(Case_Sum =  sum(Case_Number) , .groups = 'drop' ) %>%
            select(日期=`Date`, 個案數量=`Case_Sum`)
        data$`日期` <- as.character(data$`日期`)
        data
    })

})
