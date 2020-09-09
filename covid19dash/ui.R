library(shiny)
library(shinydashboard)
library(leaflet)
# Header
header <- dashboardHeader(title= "新冠肺炎儀表板")

# Body
body <- dashboardBody(
    fluidRow(
        valueBoxOutput("confiremd_all"),
        valueBoxOutput("recovered_all"),
        valueBoxOutput("deaths_all")
    ),
    fluidRow(
        column(width = 9,
               box(width = NULL, solidHeader = TRUE,
                   leafletOutput("covidmap", height = 500)
               )
        ),
        column(width = 3,
               box(width = NULL, solidHeader = TRUE
               )
        )
    )
)

# Dashboard Main
dashboardPage(
    header,
    dashboardSidebar(disable = TRUE),
    body
)
