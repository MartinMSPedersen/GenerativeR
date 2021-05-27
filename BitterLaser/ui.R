library(shiny)

shinyUI(fluidPage(
    titlePanel("Bitter Laser"),
    a("Source code:", href = "https://github.com/MartinMSPedersen/GenerativeR/tree/main/BetterLaser"),
    wellPanel(
        fluidRow(
            column(
                width = 10,
                sliderInput("seed", "Seed value:",min = 1, max = 1e5, value = sample(1:50000,1))
            )
        )
    ),
    fluidRow(
        column(6,
               actionButton("action1","Set random values"),
               downloadButton("png_download", label = "Download as png"),
               downloadButton("pdf_download", label = "Download as pdf"),
               offset = 3
        )
    ),
    plotOutput("laser", height = "600px")
))
