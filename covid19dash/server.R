library(shiny)
library(tidyverse)
library(leaflet)
shinyServer(function(input, output) {
    covid19 <- read_csv('covid19.csv')
    data    <- covid19 %>%
        filter((`Case` == 'confirmed') & (`Date` == '2020-09-08'))

    output$covidmap <- renderLeaflet({
        map <- leaflet(data) %>%
            addTiles() %>%
            addCircleMarkers(
                lat = ~Lat,
                lng = ~Long,
                radius = log(data$Case_Number),
                stroke = FALSE,
                fillOpacity = 0.5
            )
        map
    })

})
