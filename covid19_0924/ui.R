library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)

# Header
header  <- dashboardHeader(title = "新冠肺炎儀表板")

# Menu
menu <- sidebarMenu(
    menuItem("儀表板", tabName = "dashboard"),
    menuItem("各國疫情分析", tabName = "bycountry")
)

# Dashboard
dashboard <- tabItem('dashboard',
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

# By Country
bycountry <- tabItem('bycountry',
    fluidRow(
        box(width = NULL, solidHeader = TRUE,
        column(
            selectInput("country",
                    "國家:",
                    choices = c('US', 'Taiwan*', 'China')),
            width = 6
        ),
        column(
            radioButtons("case_type2",
                     label = "個案類型排行",
                     choices = list("確診" = "confirmed", "死亡" = "deaths", "康復" = "recovered"),
                     selected = "confirmed",
                     inline=TRUE),
            width =6
        )
        )
    ),
    fluidRow(
        box(width = NULL, solidHeader = TRUE,
            plotlyOutput('distPlot')
        )
    ),
    fluidRow(
        box(width = NULL, solidHeader = TRUE,
            plotlyOutput('diffPlot')
        )
    ),
    fluidRow(
        box(width = NULL, solidHeader = TRUE,
            dataTableOutput('table')
        )
    )
)


# Body
body <- dashboardBody(
    tabItems(
        dashboard,
        bycountry
    )
)


# Dashboard Page
dashboardPage(
    header,
    dashboardSidebar(menu, collapsed = TRUE),
    body
)
