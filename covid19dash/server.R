library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
COLORS_CHOICE <- list('confirmed'  ='orange',
                       'deaths'    ='red',
                       'recovered' ='green')

vbox <- function(covid19, case_type, subtitle, icon_label){
    renderValueBox({
        case_stat <- covid19 %>%
            filter((`Case` == case_type) & (`Date` == '2020-09-08'))

        valueBox(
            value = format(sum(case_stat$Case_Number), big.mark=","),
            color = as.character(COLORS_CHOICE[case_type]),
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

        colors_choice <- as.character(COLORS_CHOICE[input$case_type])

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
    output$confiremd_all <- vbox(covid19, 'confirmed', '確診人數', "user-md")
    output$deaths_all    <- vbox(covid19, 'deaths',    '死亡人數', "skull-crossbones")
    output$recovered_all <- vbox(covid19, 'recovered', '康復人數', "first-aid")

    # plotly
    output$case_ranking  <- renderPlotly({
        case_stat <- covid19 %>%
            filter((`Case` == input$case_type)  & (`Date` == '2020-09-08')) %>%
            group_by(`Country/Region`, .drop=TRUE) %>%
            summarise(Case_Number = sum(Case_Number)) %>%
            arrange(desc(Case_Number)) %>%
            head(10)

        fig <- ggplot(case_stat) +
            geom_bar(aes(x = reorder(`Country/Region`, Case_Number), y = Case_Number, text = paste(`Country/Region`, `Case_Number`)), stat = 'identity',fill =  as.character(COLORS_CHOICE[input$case_type])) +
            coord_flip() +
            xlab('') +
            ylab('個案總數')

        ggplotly(fig, tooltip = "text")
    })
})
