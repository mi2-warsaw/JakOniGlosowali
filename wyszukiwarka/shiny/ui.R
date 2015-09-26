shinyUI(fluidPage(
  
  # Tytuł
  titlePanel("Wyszukiwarka słów w wypowiedziach posłów kadencji (...)"),
  
  
  
  # menu
  sidebarLayout(
    sidebarPanel(
      actionButton("go", "Pokaż!"),
      textInput("slowo", "Wpisz wyszukiwane słowo", "przykład")
    ),
    
    # Plot, na razie przykładowy
    mainPanel(
      tabsetPanel(
        tabPanel('Test Plot1', plotOutput("testPlot1")),
        tabPanel('Test Plot2', plotOutput("testPlot2"))
      )
    )
  )
))