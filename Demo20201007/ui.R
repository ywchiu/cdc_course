library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)

## 標頭
header <- dashboardHeader(
    title = "新冠肺炎儀表板"
)

## 各國疫情分析
byCountry <- tabItem("bycountry",
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
                                                choices = c('確診'='confirmed',
                                                            '死亡'='deaths',
                                                            '康復'='recovered')),

                                )
                         )
                     ),
                     fluidRow(
                         column(width = 12,
                                box(width = NULL, solidHeader = TRUE,
                                    plotlyOutput("distPlot")
                                )
                         )
                     ),
                     fluidRow(
                         column(width = 12,
                                box(width = NULL, solidHeader = TRUE,
                                    plotlyOutput("dailyNewPlot")
                                )
                         )
                     ),
                     fluidRow(
                         column(width = 12,
                                box(width = NULL, solidHeader = TRUE,
                                    plotlyOutput("predictPlot")
                                )
                         )
                     ),

)

## 儀表板
dashboard <- tabItem("dashboard",
                     fluidRow(
                         valueBoxOutput("confirmed_all"),
                         valueBoxOutput("deaths_all"),
                         valueBoxOutput("recovered_all")
                     ),
                     fluidRow(
                         column(width = 9,
                                box(width = NULL, solidHeader = TRUE,
                                    leafletOutput("covidmap", height = 500)
                                )
                         ),
                         column(width = 3,
                                box(width = NULL, solidHeader = TRUE,
                                    radioButtons("case_type", label = "個案類型排行",
                                                 choices = list("確診" = "confirmed", "死亡" = "deaths", "康復" = "recovered"),
                                                 selected = "confirmed",
                                                 inline=TRUE)
                                ),
                                box(width = NULL, solidHeader = TRUE,
                                    plotlyOutput("case_ranking")
                                )
                         ),
                         column(
                             sliderInput(
                                 "timeSlider",
                                 label      = "Select date",
                                 min        = as.Date('2020-01-22'),
                                 max        = as.Date('2020-10-06'),
                                 value      = as.Date('2020-10-06'),
                                 width      = "100%",
                                 timeFormat = "%Y-%m-%d",
                                 animate    = animationOptions(loop = TRUE)
                             ),
                             class = "slider",
                             width = 12,
                             style = 'padding-left:15px; padding-right:15px;'
                         )
                     )
)


predictCountry <- tabItem("predictcountry",
                          fluidRow(
                              box(width = NULL, solidHeader = TRUE,
                                column(width = 4,
                                     selectInput("Country2",
                                                     "國家:",
                                                     choices = c('Taiwan*', 'US', 'China'))
                                     ),
                                column(width = 4,
                                     selectInput("Case_Type2",
                                                     "個案情形:",
                                                     choices = c('確診'='confirmed',
                                                                 '死亡'='deaths',
                                                                 '康復'='recovered')),

                                     ),
                                column(width = 4,
                                       selectInput("Model_Type",
                                                   "模型選擇:",
                                                   multiple =FALSE,
                                                   choices = c('ARIMA'='ARIMA',
                                                               'PROPHET'='PROPHET',
                                                               'RANDOMFOREST'='RANDOMFOREST')),
                                        )
                              )
                          ),
                          fluidRow(
                              column(width = 12,
                                     box(width = NULL, solidHeader = TRUE,
                                         sliderInput("Predict_Length",
                                                     "預測週期:",
                                                     min = 7,
                                                     max = 90,
                                                     value = 7,
                                                     step = 1
                                         )
                                     )
                              )
                          ),
                          fluidRow(
                              column(width = 12,
                                     box(width = NULL, solidHeader = TRUE,
                                         plotlyOutput("predictPlot2")
                                     )
                              )
                          ),
)

# 網頁內容
body <- dashboardBody(
    tabItems(
        dashboard,
        byCountry,
        predictCountry
    )
)

# 網頁格局配置
dashboardPage(
    header,
    dashboardSidebar(
        sidebarMenu(
            menuItem("儀表板", tabName = "dashboard"),
            menuItem("各國疫情分析", tabName = "bycountry"),
            menuItem("各國疫情預測", tabName = "predictcountry")
        )
    ),
    body
)
