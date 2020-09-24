library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
header  <- dashboardHeader(title = "新冠肺炎儀表板")

body <- dashboardBody(
    fluidRow(
        valueBoxOutput('confirmed_all'),
        valueBoxOutput('deaths_all'),
        valueBoxOutput('recovered_all')
    ),
    fluidRow(
        column(width = 9,
            box(width = NULL, solidHeader = TRUE,
                leafletOutput("covidmap", height = 500)
            )
        ),
        column(width = 3,
             box(width = NULL, solidHeader = TRUE,
                 radioButtons("case_type",
                              label = "個案類型排行",
                              choices = list("確診" = "confirmed", "死亡" = "deaths", "康復" = "recovered"),
                              selected = "confirmed",
                              inline=TRUE)

             ),
             box(width = NULL, solidHeader = TRUE,
                 plotlyOutput("case_ranking")
             )
        )
    ),
    fluidRow(
        column(
            sliderInput(
                "timeSlider",
                label  = "Select date",
                min    = as.Date('2020-01-22'),
                max    = as.Date('2020-09-22'),
                value  = as.Date('2020-09-22'),
                width  = "100%",
                timeFormat = "%Y/%m/%d",
                animate  = animationOptions(loop = FALSE)
            ),
            class = "slider",
            width = 12,
            style = 'padding-left:15px; padding-right:15px;'
        )

    )
)

dashboardPage(
    header,
    dashboardSidebar(disable=TRUE),
    body
)
