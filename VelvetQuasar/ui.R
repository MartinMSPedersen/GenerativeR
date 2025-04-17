library(shiny)

shinyUI(fluidPage(
    titlePanel("Velvet Quasar Generator"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("seed", "Seed", min = 1, max = 1e5, value = sample(1:1e5, 1), step = 1),
        sliderInput("layout", "Character Set", min = 0, max = 5, value = sample(0:5, 1), step = 1),
        actionButton("randomvalues", "Randomize Parameters"),
        hr(),
        downloadButton("png_download", "Download PNG"),
        downloadButton("pdf_download", "Download PDF"),
        downloadButton("svg_download", "Download SVG")
      ),
      mainPanel(
        plotOutput("quasar") 
      )
    )
  )
)