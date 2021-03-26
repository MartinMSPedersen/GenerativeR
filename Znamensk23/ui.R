library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel("Znamensk 23"),
    a("Source code:", href = "https://github.com/MartinMSPedersen/GenerativeR/tree/master/Znamensk23"),
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
    plotOutput("znamenskPlot", height = "600px")
))
