
library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
library(scales)
library(lubridate)

shinyServer(function(input, output, session) {
    ## 讀取資料
    covid19 <- read_csv('covid19.csv')

    ## 地圖呈現 (Leaflet)
    output$covidmap <- renderLeaflet({
        confirmed <- covid19 %>%
            filter((`Case` == input$case_type) & (`Date` == input$timeSlider))

        map <- leaflet(confirmed) %>%

            addTiles() %>%
            addCircleMarkers(
                lat = confirmed$Lat,
                lng = confirmed$Long,
                radius = log(confirmed$Case_Number),
                stroke = FALSE,
                fillOpacity = 0.5
            )
        map
    })

    # 計算確診人數
    output$confirmed_all <- renderValueBox({
        case_stat <- covid19 %>%
            filter((`Case` == 'confirmed') & (`Date` == input$timeSlider))
        valueBox(
            sum(case_stat$Case_Number),
            color="orange",
            "確診人數",
            icon = icon("procedures")
        )
    })

    # 計算死亡人數
    output$deaths_all <- renderValueBox({
        case_stat <- covid19 %>%
            filter((`Case` == 'deaths') & (`Date` == input$timeSlider))
        valueBox(
            sum(case_stat$Case_Number),
            color="red",
            "死亡人數",
            icon = icon("skull-crossbones")
        )
    })

    # 計算康復人數
    output$recovered_all <- renderValueBox({
        case_stat <- covid19 %>%
            filter((`Case` == 'recovered') & (`Date` == input$timeSlider))
        valueBox(
            sum(case_stat$Case_Number),
            color='green',
            "康復人數",
            icon = icon("first-aid")
        )
    })

    # 確診排行
    output$case_ranking <-renderPlotly({
        case_stat <- covid19 %>%
            filter((`Case` == input$case_type) & (`Date` == input$timeSlider)) %>%
            group_by(`Country/Region`) %>%
            summarise(Case_Number = sum(`Case_Number`)) %>%
            arrange(desc(Case_Number)) %>%
            head(10)

        fig <- ggplot(case_stat)
        g <- fig +
            geom_bar(aes(x=reorder(`Country/Region`, Case_Number),y=Case_Number),stat="identity", fill= 'blue') +
            coord_flip() +
            ylab('Case Numbers') +
            xlab('Country') +
            scale_y_continuous(labels = scales::comma)
        ggplotly(g)
    })

    # 更新國家選項
    observeEvent(input$Case_Type, {
        updateSelectInput(session, "Country", choices = unique(covid19$`Country/Region`))
    })

    output$table <- renderDataTable({
        data <- covid19 %>%
            filter((`Case` == input$Case_Type) &(`Country/Region` == input$Country)) %>%
            select(Date, Case_Number)
        data$Date <- as.character(as_date(data$Date))
        data
    })
    output$predictPlot <- renderPlotly({
        data <- covid19 %>%
            filter((`Case` == input$Case_Type) &(`Country/Region` == input$Country)) %>%
            group_by(Date) %>%
            summarize(Case_Number = sum(Case_Number)) %>%
            select(Date, Case_Number)
        ret <- predictSeries(data)
        ret %>%
            plot_modeltime_forecast(.interactive = TRUE,.y_lab = "New daily cases")
    })

    ## 個案類型折線圖
    output$distPlot <- renderPlotly({
        data <- covid19 %>%
            filter((`Case` == input$Case_Type) &(`Country/Region` == input$Country)) %>%
            group_by(Date) %>%
            summarize(Case_Number = sum(Case_Number)) %>%
            select(Date, Case_Number)

        fig <- ggplot(data)

        g <- fig +
            aes(x = Date, y = Case_Number) +
            geom_line(color='steelblue') +
            geom_point(color="blue", size=0.3)

        ggplotly(g)
    })

    # 繪製新增數量 (Bar)
    output$dailyNewPlot <- renderPlotly({
        data <- covid19 %>%
            filter((`Case` == input$Case_Type) &(`Country/Region` == input$Country)) %>%
            group_by(Date) %>%
            summarize(Case_Number = sum(Case_Number)) %>%
            select(Date, Case_Number)

        data$Daily_Number <- c(0, diff(data$Case_Number))

        fig <- ggplot(data)
        g <- fig +
            aes(x = Date, y = Daily_Number) +
            geom_bar(stat="identity", fill = "#FF6666")

        ggplotly(g)
    })

})
