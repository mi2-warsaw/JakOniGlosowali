library(shiny)

source("tools.R")

shinyServer(function(input, output) {
  
  wartosc <- eventReactive(input$go, {
    c(input$typ, input$slowo)
  })

  output$speakerDendro <- renderPlot({
    withProgress(message = 'Twają obliczenia,',
                 detail = 'To może chwilę potrwać...', value = 0, {
                   getSpeakerDendro(wartosc())
                 })
  })

  output$speakerDendro2 <- renderPlot({
    withProgress(message = 'Twają obliczenia,',
                 detail = 'To może chwilę potrwać...', value = 0, {
                   getSpeakerDendro2(wartosc())
                 })
  })
  
})
