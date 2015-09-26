shinyUI(fluidPage(
  
  includeCSS("style.css"),
  
  # Tytuł
  titlePanel("Wyszukiwarka słów w wypowiedziach posłów sejmu RP VII kadencji (2011-2015)"),
  
  
  # menu
  sidebarLayout(
    sidebarPanel(
      textInput("slowo", "Wpisz wyszukiwane słowo, jego fragment lub wyrażenie (regexp)", "słowo"),
      actionButton("go", "Pokaż!")
    ),
    
    # Plot, na razie przykładowy
    mainPanel(
      tabsetPanel(
        tabPanel('Wypowiedzi', uiOutput("tekst")),
        tabPanel('Mówcy', plotOutput("speakerCounts")),
        tabPanel('Daty', plotOutput("dateCounts"))
      )
    )
  )
))