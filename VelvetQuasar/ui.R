library(shiny)

shinyUI(fluidPage(
    titlePanel("Velvet Quasar"),
    a("Source code:", href = "https://github.com/MartinMSPedersen/GenerativeR/tree/master/VelvetQuasar"),
    wellPanel(
        fluidRow(
            column(
                width = 10,
                sliderInput("seed", "Seed value:",min = 1, max = 1e5, value = sample(1:50000,1))
            ),
            column(
                width = 2,
                radioButtons("layout", "Character Set:", 0:6, selected = 5)
            )
        )
    ),
    fluidRow(
        column(12,
               actionButton("randomvalues","Set random values"),
               downloadButton("svg_download", label = "Download as svg"),
               downloadButton("png_download", label = "Download as png"),
               downloadButton("pdf_download", label = "Download as pdf")
        )
    ),
    fluidRow(
      column(width = 12,
             plotOutput("quasar", width = "800px", height = "800px")
      )
    )
))
