library(shiny)

source("tools.R")

shinyServer(function(input, output) {
  
  wartosc <- eventReactive(input$go, {
    input$slowo
  })
  
  output$test1 <- renderUI({
    cat(wartosc(), file = stderr()) # diagnostyka: wypisywanie wartosci w konsoli
  })
  
  output$testPlot1 <- renderPlot({
    cat(wartosc(), file = stderr()) # diagnostyka: wypisywanie wartosci w konsoli
    getSpeakerCounts(wartosc())
  })
  
  output$testPlot2 <- renderPlot({
    cat(wartosc(), file = stderr()) # diagnostyka: wypisywanie wartosci w konsoli
    getDateCounts(wartosc())
  })
})
