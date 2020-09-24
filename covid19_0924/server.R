library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)

shinyServer(function(input, output, session) {
    covid19 <- read_csv('covid19.csv')
    #output$mtcars <- renderTable(head(covid19))

    output$covidmap <- renderLeaflet({

        confirmed <- covid19 %>%
            filter(`Case` == input$case_type, Date == input$timeSlider)

        map <- leaflet(confirmed) %>%
            addTiles() %>%
            addCircleMarkers(
                lat = confirmed$Lat,
                lng = confirmed$Long,
                radius = log(confirmed$Case_Number),
                stroke = FALSE,
                color = ifelse(confirmed$Case_Number > 1000000, 'red', 'blue'),
                label=as.character(paste(confirmed$`Country/Region`, formatC(confirmed$Case_Number, format="f", big.mark=",", digits=0))),
                popup = as.character(paste(confirmed$`Country/Region`, formatC(confirmed$Case_Number, format="f", big.mark=",", digits=0))),
                fillOpacity = 0.5
            )
        map
    })

    output$confirmed_all <- renderValueBox({
        case_stat <-  covid19 %>%
            filter(Case == 'confirmed', Date == input$timeSlider)
        valueBox(
            formatC(sum(case_stat$Case_Number), format="f", big.mark=",", digits=0),
            color="orange",
            "確診人數",
            icon = icon("procedures")
        )
    })
    output$deaths_all <- renderValueBox({
        case_stat <-  covid19 %>%
            filter(Case == 'deaths', Date == input$timeSlider)
        valueBox(
            formatC(sum(case_stat$Case_Number), format="f", big.mark=",", digits=0),
            color="red",
            "死亡人數",
            icon = icon("skull-crossbones")
        )
    })
    output$recovered_all <- renderValueBox({
        case_stat <-  covid19 %>%
            filter(Case == 'recovered', Date == input$timeSlider)
        valueBox(
            formatC(sum(case_stat$Case_Number), format="f", big.mark=",", digits=0),
            color="green",
            "康復人數",
            icon = icon("first-aid")
        )
    })
    output$case_ranking <- renderPlotly({
        case_stat <- covid19 %>%
            filter(Case == input$case_type, Date == input$timeSlider) %>%
            group_by(`Country/Region`) %>%
            summarise(Case_Number = sum(Case_Number)) %>%
            arrange(desc(Case_Number)) %>%
            head(10)

        fig <- ggplot(case_stat)

        g <- fig +
            geom_bar(aes(x=reorder(`Country/Region`, Case_Number),y=Case_Number),stat="identity", fill= 'blue') +
            coord_flip() +
            ylab('Case Numbers') +
            xlab('Country')

        ggplotly(g)

    })
})
