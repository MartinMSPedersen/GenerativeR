library(shiny)

shinyUI(fluidPage(
    titlePanel("Highway Sauna"),
    a("Source code:", href = "https://github.com/MartinMSPedersen/GenerativeR/tree/master/HighwaySauna"),
    wellPanel(
        fluidRow(
            column(
                width = 10,
                sliderInput("seed", "Seed value:",min = 1, max = 1e5, value = sample(1:50000,1))
            ),
            column(
                width = 2,
                radioButtons("layout", "Layout:", 0:7, selected = 0)
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
    plotOutput("sauna", height = "600px")
))
