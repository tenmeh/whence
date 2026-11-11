# A baseline for #15: one base R plot in Shiny, with no whence.
library(shiny)

ui <- fluidPage(textOutput("note"), plotOutput("plot"))

server <- function(input, output, session) {
  output$note <- renderText({
    message("baseline: rendering note")
    "Base plot below."
  })
  output$plot <- renderPlot({
    message("baseline: rendering plot")
    barplot(c(a = 3, b = 5, c = 4))
  })
  session$onFlushed(function() message("baseline: all outputs sent"))
}

shinyApp(ui, server)
