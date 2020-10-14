library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)

shinyServer(function(input, output, session) {
    covid <- read_csv('')

    observeEvent(input$Case_Type, {
        # TODO
    })

    output$accumPlot <- renderPlotly({
        # TODO
    })

    output$dailyNewPlot <- renderPlotly({
        # TODO
    })

    output$table <- renderDataTable({
        # TODO
    })



})
