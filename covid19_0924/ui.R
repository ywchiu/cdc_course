library(shiny)
library(shinydashboard)
library(leaflet)

header  <- dashboardHeader(title = "新冠肺炎儀表板")

body <- dashboardBody(
    fluidRow(
        column(width = 9,
            box(width = NULL, solidHeader = TRUE,
                leafletOutput("covidmap", height = 500)
            )
        ),
        column(width = 3,
             box(width = NULL, solidHeader = TRUE,
                   #leafletOutput("covidmap", height = 500)
             )
        )
    ),
    fluidRow(
        column(width = 12,
               box(width = NULL, solidHeader = TRUE,
                   #leafletOutput("covidmap", height = 500)
               )
        )
    )
)

dashboardPage(
    header,
    dashboardSidebar(disable=TRUE),
    body
)
