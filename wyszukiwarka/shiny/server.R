library(shiny)

shinyServer(function(input, output) {
  
  wartosc <- eventReactive(input$go, {
    input$slowo
  })
  
  output$distPlot <- renderPlot({
    cat(wartosc(), file = stderr()) # diagnostyka: wypisywanie wartosci w konsoli
    plot(1,1, main=wartosc())
  })
})
