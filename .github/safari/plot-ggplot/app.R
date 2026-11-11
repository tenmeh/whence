# A baseline for #15: one ggplot2 plot in Shiny, with no whence.
library(shiny)
library(ggplot2)

ui <- fluidPage(textOutput("note"), plotOutput("plot"))

server <- function(input, output, session) {
  output$note <- renderText({
    message("baseline: rendering note")
    "ggplot2 plot below."
  })
  output$plot <- renderPlot({
    message("baseline: rendering plot")
    ggplot(data.frame(x = c("a", "b", "c"), y = c(3, 5, 4)), aes(x, y)) +
      geom_col()
  })
  session$onFlushed(function() message("baseline: all outputs sent"))
}

shinyApp(ui, server)
