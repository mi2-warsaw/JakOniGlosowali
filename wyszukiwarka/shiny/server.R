library(shiny)

shinyServer(function(input, output) {
  
  wartosc <- eventReactive(input$go, {
    input$slowo
  })
  
  output$testPlot1 <- renderPlot({
    cat(wartosc(), file = stderr()) # diagnostyka: wypisywanie wartosci w konsoli
    plot(1, 1, main=wartosc())
  })
  
  output$testPlot2 <- renderPlot({
    cat(wartosc(), file = stderr()) # diagnostyka: wypisywanie wartosci w konsoli
    plot(2, 2, main=wartosc())
  })
})
