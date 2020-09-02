#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Covid19 儀表板"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("Case_Type",
                        "個案情形:",
                        choices = c('confirmed', 'deaths', 'recovered')),
            selectInput("Country",
                        "國家:",
                        choices = c('Taiwan*', 'US', 'China'))

        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("distPlot"),
            dataTableOutput('table')
        )
    )
))
