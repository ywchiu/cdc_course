library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
library(lubridate)

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

        ggplotly(g) %>% config(displayModeBar = F)

    })

    output$table <- renderDataTable({
        data <- covid19 %>%
            filter((`Case` == input$case_type2) &(`Country/Region` == input$country)) %>%
            group_by(`Country/Region`, Date) %>%
            summarise(Case_Number = sum(Case_Number)) %>%
            select(Date, Case_Number) %>%
            arrange(desc(Date))
        data$Date <- as.character(as_date(data$Date))
        data
    })

    output$distPlot <- renderPlotly({
        data <- covid19 %>%
            filter((`Case` == input$case_type2) &(`Country/Region` == input$country)) %>%
            group_by(`Country/Region`, Date) %>%
            summarise(Case_Number = sum(Case_Number)) %>%
            select(Date, Case_Number)

        fig <- ggplot(data)

        g <- fig +
            aes(x = Date, y = Case_Number) +
            geom_line()

        ggplotly(g) %>% config(displayModeBar = F)
    })

    output$diffPlot <- renderPlotly({
        data <- covid19 %>%
            filter((`Case` == input$case_type2) &(`Country/Region` == input$country)) %>%
            group_by(`Country/Region`, Date) %>%
            summarise(Case_Number = sum(Case_Number)) %>%
            select(Date, Case_Number)
        data$diff_number <- c(0,diff(data$Case_Number))

        fig <- ggplot(data)

        g <- fig +
            geom_bar(aes(x= Date, y=diff_number),stat="identity", fill= 'blue')


        ggplotly(g) %>% config(displayModeBar = F)
    })

    #observeEvent(input$country, {
    #    updateSelectInput(session, "country", choices = unique(covid19$`Country/Region`))
    #})

})
