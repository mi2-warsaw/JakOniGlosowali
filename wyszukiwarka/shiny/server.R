library(shiny)

source("tools.R")

shinyServer(function(input, output) {
  
  wartosc <- eventReactive(input$go, {
    c(input$slowo, input$slowoNeg, input$posel)
  })
  
  # pokazuje progress przy obliczeniach
                 
  output$tekst <- renderUI({
    #cat(wartosc(), file = stderr()) # diagnostyka: wypisywanie wartosci w konsoli
    withProgress(message = 'Obliczam,',
                 detail = 'może chwilę potrwać...', value = 0, {
                   getBorders(wartosc())
                 })
  })
  
  output$speakerCounts <- renderPlot({
    #cat(wartosc(), file = stderr()) # diagnostyka: wypisywanie wartosci w konsoli
    withProgress(message = 'Obliczam,',
                 detail = 'może chwilę potrwać...', value = 0, {
                   getSpeakerCounts(wartosc())
                 })
  })

  
  output$speakerCounts2 <- renderTable({
    #cat(wartosc(), file = stderr()) # diagnostyka: wypisywanie wartosci w konsoli
    withProgress(message = 'Obliczam,',
                 detail = 'może chwilę potrwać...', value = 0, {
                   getSpeakerCounts2(wartosc())
                 })
  })
  
  output$dateCounts <- renderPlot({
    #cat(wartosc(), file = stderr()) # diagnostyka: wypisywanie wartosci w konsoli
    withProgress(message = 'Obliczam,',
                 detail = 'może chwilę potrwać...', value = 0, {
                   getDateCounts(wartosc())
                 })
  })
  
  output$dateCounts2 <- renderTable({
    #cat(wartosc(), file = stderr()) # diagnostyka: wypisywanie wartosci w konsoli
    withProgress(message = 'Obliczam,',
                 detail = 'może chwilę potrwać...', value = 0, {
                   getDateCounts2(wartosc())
                 })
  })
  
})
