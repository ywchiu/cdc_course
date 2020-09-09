library(shiny)
library(tidyverse)
library(leaflet)


vbox <- function(covid19, case_type,color, subtitle, icon_label){
    renderValueBox({
        case_stat <- covid19 %>%
            filter((`Case` == case_type) & (`Date` == '2020-09-08'))

        valueBox(
            value = format(sum(case_stat$Case_Number), big.mark=","),
            color = color,
            subtitle = subtitle,
            icon = icon(icon_label)
        )

    })
}

shinyServer(function(input, output) {
    covid19 <- read_csv('covid19.csv')



    # map
    output$covidmap <- renderLeaflet({
        data    <- covid19 %>%
            filter((`Case` == input$case_type) & (`Date` == '2020-09-08'))

        colors_choice <- 'blue'
        if (input$case_type =='confirmed'){
            colors_choice <- 'orange'
        }else if (input$case_type =='deaths'){
            colors_choice <- 'red'
        }else{
            colors_choice <- 'green'
        }

        map <- leaflet(data) %>%
            addTiles() %>%
            addCircleMarkers(
                lat = ~Lat,
                lng = ~Long,
                radius = log(data$Case_Number),
                stroke = FALSE,
                color = colors_choice,
                label= paste(data$`Country/Region`,as.character(data$Case_Number)),
                popup = paste(data$`Country/Region`,as.character(data$Case_Number)),
                fillOpacity = 0.5
            )
        map
    })

    # valuebox
    output$confiremd_all <- vbox(covid19, 'confirmed', 'orange', '確診人數', "user-md")
    output$deaths_all    <- vbox(covid19, 'deaths',  'red', '死亡人數', "skull-crossbones")
    output$recovered_all <- vbox(covid19, 'recovered', 'green', '康復人數', "first-aid")

})
