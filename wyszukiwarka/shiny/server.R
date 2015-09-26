library(shiny)

source("tools.R")

shinyServer(function(input, output) {
  
  wartosc <- eventReactive(input$go, {
    input$slowo
  })
  
  # pokazuje progress przy obliczeniach
                 
  output$tekst <- renderUI({
    #cat(wartosc(), file = stderr()) # diagnostyka: wypisywanie wartosci w konsoli
    withProgress(message = 'Obliczam,',
                 detail = 'Może chwilę potrwać...', value = 0, {
                   getBorders(wartosc())
                 })
  })
  
  output$testPlot1 <- renderPlot({
    #cat(wartosc(), file = stderr()) # diagnostyka: wypisywanie wartosci w konsoli
    getSpeakerCounts(wartosc())
  })
  
  output$testPlot2 <- renderPlot({
    #cat(wartosc(), file = stderr()) # diagnostyka: wypisywanie wartosci w konsoli
    getDateCounts(wartosc())
  })
  
})
