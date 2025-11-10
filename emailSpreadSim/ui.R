library(shiny)

fluidPage(

  titlePanel( "Rozprzestrzenianie informacji poprzez Email - Independent Cascades Model"),

  sidebarPanel(

    sliderInput(
      inputId = "probability_multiplier",
      label = "Mnożnik prawdopodobieństwa aktywacji krawędzi:",
      min = 20,
      max = 200,
      value = 100
    ),
    sliderInput(
      inputId = "iterations",
      label = "Liczba wykonań jednej symulacji symulacji:",
      min = 1,
      max = 50,
      value = 10
    )
  ),
  mainPanel(

    plotOutput(outputId = "ICPlot")
  )
)