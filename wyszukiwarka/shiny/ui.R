shinyUI(fluidPage(
  
  includeCSS("style.css"),
  
  # Tytuł
  titlePanel("Wyszukiwarka słów w wypowiedziach posłów sejmu RP VII kadencji (2011-2015)"),
  
  
  # menu
  sidebarLayout(
    sidebarPanel(
      actionButton("go", "Pokaż!"),
      textInput("slowo", "Wpisz wyszukiwane słowo, jego fragment lub wyrażenie (regexp)", "słowo")
    ),
    
    # Plot, na razie przykładowy
    mainPanel(
      tabsetPanel(
        tabPanel('Test ', uiOutput("tekst")),
        tabPanel('Test Plot1', plotOutput("testPlot1")),
        tabPanel('Test Plot2', plotOutput("testPlot2"))
      )
    )
  )
))