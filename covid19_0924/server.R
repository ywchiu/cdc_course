library(shiny)
library(tidyverse)
library(leaflet)

shinyServer(function(input, output, session) {
    covid19 <- read_csv('covid19.csv')
    #output$mtcars <- renderTable(head(covid19))

    output$covidmap <- renderLeaflet({

        confirmed <- covid19 %>%
            filter(`Case` == 'confirmed')

        map <- leaflet(confirmed) %>%
            addTiles() %>%
            addCircleMarkers(
                lat = confirmed$Lat,
                lng = confirmed$Long,
                radius = log(confirmed$Case_Number),
                stroke = FALSE,
                label=as.character(paste(confirmed$`Country/Region`, confirmed$`Case_Number`)),
                popup = as.character(confirmed$`Country/Region`),
                fillOpacity = 0.5
            )
        map
    })
})
