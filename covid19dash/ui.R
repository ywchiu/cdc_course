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
               box(width = NULL, solidHeader = TRUE,
                   radioButtons("case_type",
                                label = "個案類型排行",
                                choices = list("確診" = "confirmed", "死亡" = "deaths", "康復" = "recovered"),
                                selected = "confirmed",
                                inline=TRUE),

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
