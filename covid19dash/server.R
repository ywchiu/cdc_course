library(shiny)

shinyServer(function(input, output) {
    output$mtcars <- renderTable(
        head(mtcars)
    )

})
