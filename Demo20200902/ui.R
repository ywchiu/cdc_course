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
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    navbarPage(
        "Covid19 Dashboard",
        id = "main_navbar",
        tabPanel(
            "Overview",
            leafletOutput("mymap"),
            p(),
            fluidRow(column(
                4, uiOutput("overallstat", container = span)
            ),
            column(
                4, uiOutput("overallstat1", container = span)
            ),
            column(
                4, uiOutput("overallstat2", container = span)
            )
            )

        ),
        tabPanel(
            "Country Detail",
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
                    tabsetPanel(
                        tabPanel("Plot", plotlyOutput("distPlot")),
                        tabPanel("Table", dataTableOutput('table'))
                    )
                )
            )
        )
    )
))
