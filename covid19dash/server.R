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
    data    <- covid19 %>%
        filter((`Case` == 'confirmed') & (`Date` == '2020-09-08'))

    # map
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

    # valuebox
    output$confiremd_all <- vbox(covid19, 'confirmed','orange', '確診人數', "user-md")
    output$deaths_all <- vbox(covid19, 'deaths','red', '死亡人數', "skull-crossbones")
    output$recovered_all <- vbox(covid19, 'recovered','aqua', '康復人數', "first-aid")

})
