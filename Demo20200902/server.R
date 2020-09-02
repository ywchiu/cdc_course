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

plotcovid <- function(input, tabs, covid19){
    data  <- covid19 %>%
        filter((`Country/Region` == input$Country) & (`Case` == tabs)) %>%
        group_by(`Date`) %>%
        summarise(Case_Sum =  sum(Case_Number) , .groups = 'drop' ) %>%
        select(`Date`, `Case_Sum`)

    fig <- ggplot(data)
    g <- fig +
        aes(x = Date, y = Case_Sum) +
        geom_line() +
        xlab('日期') +
        ylab('個案數量') +
        ggtitle(paste0(input$Country, '新冠肺炎趨勢圖'))
    ggplotly(g)
}
tablecovid <- function(input, tabs, covid19){
        data  <- covid19 %>%
            filter((`Country/Region` == input$Country) & (`Case` == input$tabs)) %>%
            group_by(`Date`) %>%
            summarise(Case_Sum =  sum(Case_Number) , .groups = 'drop' ) %>%
            select(日期=`Date`, 個案數量=`Case_Sum`) %>%
            arrange(desc(`日期`))

        data$`日期` <- as.character(data$`日期`)
        data
}
shinyServer(function(input, output, session) {
    covid19 <- read_csv('covid19.csv')

    output$distPlot <- renderPlotly(plotcovid(input, 'confirmed',covid19))
    output$distPlot1 <- renderPlotly(plotcovid(input, 'deaths',covid19))
    output$distPlot2 <- renderPlotly(plotcovid(input, 'recovered',covid19))

    output$table <- renderDataTable(tablecovid(input, 'confirmed', covid19))
    output$table1 <- renderDataTable(tablecovid(input, 'deaths', covid19))
    output$table2 <- renderDataTable(tablecovid(input, 'recovered', covid19))

    output$mymap <- renderLeaflet({
        covid19_lastest_confirmed_df <- covid19 %>%
            filter((`Date` == '2020-09-01') & (`Case` == 'confirmed') )

        leaflet(covid19_lastest_confirmed_df) %>% addTiles() %>%
            addCircleMarkers(
                lat = covid19_lastest_confirmed_df$Lat,
                lng = covid19_lastest_confirmed_df$Long,
                radius = log(covid19_lastest_confirmed_df$Case_Number),
                color = ifelse(covid19_lastest_confirmed_df$Case_Number >=1000000 , 'red', 'blue'),
                stroke = FALSE,
                fillOpacity = 0.5
            )
    })
    output$overallstat <- renderUI({
        covid19_lastest_confirmed_df <- covid19 %>%
            filter((`Date` == '2020-09-01') & (`Case` == 'confirmed') )
        covid19_lastest_deaths_df <- covid19 %>%
            filter((`Date` == '2020-09-01') & (`Case` == 'deaths') )
        h3(paste0('確診數:', sum(covid19_lastest_confirmed_df$Case_Number)))
    }
    )
    output$overallstat1 <- renderUI({
        covid19_lastest_deaths_df <- covid19 %>%
            filter((`Date` == '2020-09-01') & (`Case` == 'deaths') )
        h3(paste0('死亡數:', sum(covid19_lastest_deaths_df$Case_Number)))
    }
    )
    output$overallstat2 <- renderUI({
        covid19_lastest_recovered_df <- covid19 %>%
            filter((`Date` == '2020-09-01') & (`Case` == 'deaths') )
        h3(paste0('康復數:', sum(covid19_lastest_recovered_df$Case_Number)))
    }
    )
    observeEvent(input$tabs, {
        updateSelectInput(session, "Country", choices = unique(covid19$`Country/Region`))
    })

})
