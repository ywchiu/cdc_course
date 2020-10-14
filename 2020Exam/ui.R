library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
header <- dashboardHeader(
    title = "新冠肺炎測驗題"
)



body <- dashboardBody(

        tabItem("bycountry",
                fluidRow(
                    column(width = 9,
                           box(width = NULL, solidHeader = TRUE,
                               selectInput("Country",
                                           "國家:",
                                           choices = c('Taiwan*', 'US', 'China'))
                           )
                    ),
                    column(width = 3,
                           box(width = NULL, solidHeader = TRUE,
                               selectInput("Case_Type",
                                           "個案情形:",
                                           choices = c('confirmed', 'deaths', 'recovered')),

                           )
                    )
                ),
                fluidRow(
                    column(width = 6,
                           box(width = NULL, solidHeader = TRUE,
                               plotlyOutput("accumPlot")
                           )
                    ),
                    column(width = 6,
                           box(width = NULL, solidHeader = TRUE,
                               plotlyOutput("dailyNewPlot")
                           )
                    )
                ),
                fluidRow(
                    column(width = 12,
                           box(width = NULL, solidHeader = TRUE,
                               dataTableOutput('table')

                           )
                    )
                )
        )

)


dashboardPage(
    header,
    dashboardSidebar(
        sidebarMenu(
            menuItem("各國疫情分析", tabName = "bycountry")
        ), disable=TRUE
    ),
    body
)
