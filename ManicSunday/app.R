library(shiny)
library(jsonlite) # base64_enc()
library(ggplot2)
library(ambient) # noise_simplex

ui <- fluidPage(
    titlePanel("Manic Sunday"),
    a("Source code:", href = "https://github.com/MartinMSPedersen/GenerativeR/tree/master/ManicSunday"),
    wellPanel(
        sliderInput("seed",
                    "Seed value:",
                    min = 0,
                    max = 50000,
                    value = 25000),
        sliderInput("size",
                    "Size:",
                    min = 1,
                    max = 200,
                    value = 100),
        sliderInput("noise",
                    "Noise:",
                    min = 4,
                    max = 10,
                    value = 8),
        sliderInput("speed",
                    "Speed:",
                    min = 5,
                    max = 50,
                    value = 25)
    ),
    fluidRow(
        column(10,
               actionButton("action1","Set random values"),
               downloadButton("png_download", label = "Download as png"),
               downloadButton("pdf_download", label = "Download as pdf"),
               offset = 1)
    ),
    plotOutput("manicsunday", height = "640px", width = "640px"),
)


server <- function(input, output, session) {
    observe({
        input$action1
        updateSliderInput(session, "seed", value = sample(0:50000,1))
        updateSliderInput(session, "size", value = sample(1:200,1))
        updateSliderInput(session, "noise", value = sample(4:10,1))
        updateSliderInput(session, "speed", value = sample(5:50,1))
    })
    

    plotInput <- function() {
        set.seed(input$seed)
        size <- 50+input$size
        noisy <- input$noise
        speedy <- input$speed/10

        df <- expand.grid(x = 1:size, y=1:size)
        df$angle <- runif(size, 0, 2*pi)
        df$speed <- as.vector(noise_simplex(c(size,size)))
        
        g <- ggplot(df, aes(x, y)) +
             geom_spoke(aes(angle = speedy*speed, radius = noisy*speed)) +
             theme_void()
        g        
    }

    output$png_download <- downloadHandler(
        filename = function() {
            paste("manicsunday-",base64_enc(paste(input$seed,input$size,input$noise,input$speed, sep = ":")),".png", sep="")
        },
        content = function(file) {
            p <- plotInput()
            ggsave(file, device = "png", width = 8, height = 8)
        }
    )
    output$pdf_download <- downloadHandler(
        filename = function() {
            paste("manicsunday-",base64_enc(paste(input$seed,input$size,input$noise,input$speed, sep = ":")),".pdf", sep="")
        },
        content = function(file) {
            p <- plotInput()
            ggsave(file, p, device = "pdf", paper = "A4", title = "Around Latvia", width=8, height=8)
        }
    )
    output$manicsunday <- renderPlot({
        print(plotInput())
    })
    
}

shinyApp(ui = ui, server = server)


