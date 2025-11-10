library(shiny)

fluidPage(
  titlePanel("Information Spread via Email - Simulation"),
  sidebarPanel(
    sliderInput(
      inputId = "probability_multiplier",
      label = "Edge activation probability multiplier:",
      min = 20,
      max = 200,
      value = 100
    ),
    sliderInput(
      inputId = "iterations",
      label = "Experiments run for single strategy:",
      min = 1,
      max = 50,
      value = 10
    )
  ),
  mainPanel(
    plotOutput(outputId = "ICPlot")
  )
)
