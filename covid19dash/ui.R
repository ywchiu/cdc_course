library(shiny)
library(shinydashboard)

# Header
header <- dashboardHeader(title= "新冠肺炎儀表板")

# Body
body <- dashboardBody(
    valueBox(100, "Basic example"),
    tableOutput("mtcars")
)

# Dashboard Main
dashboardPage(
    header,
    dashboardSidebar(textInput("text", "Text")),
    body
)
