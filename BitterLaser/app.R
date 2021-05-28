library(shiny)
library(ggplot2)
library(jsonlite)

ui <- fluidPage(
    titlePanel("Bitter Laser"),
    a("Source code:", href = "https://github.com/MartinMSPedersen/GenerativeR/tree/master/BitterLaser"),
    wellPanel(
        sliderInput("seed",
                    "Seed value:",
                    min = 0,
                    max = 50000,
                    value = 40)
        ),
    fluidRow(
        column(10,
               actionButton("action1","Set random values"),
               downloadButton("png_download", label = "Download as png"),
               downloadButton("pdf_download", label = "Download as pdf"),
               offset = 1)
    ),
    plotOutput("bitterlaserplot", height = "640px", width = "640px"),
)

server <- function(input, output, session) {    
    observe({
        input$action1
        updateSliderInput(session, "seed", value = sample(0:50000,1))
    })
    
    plotInput <- function() {
        set.seed(input$seed)
        
        lmap <- function(data, datastart, dataend, targetstart, targetend) {
            if (data <= datastart) return(targetstart)
            if (data >= dataend) return(targetend)
            data/(dataend - datastart) * (targetend - targetstart) + targetstart
        }
        
        size <- 1000
#        plot.new()
        plot.window(c(1,size), c(1,size))
        z <- 40
        #chars <- 77824:(77824+1070)
        #chars <- setdiff(chars,78025:78034) 
        #chars <- as.numeric("0x13254"):as.numeric("0x13256")
        chars <- as.numeric("0x13255"):as.numeric("0x13256")
        chars <- 77824:(77824+1070)
        chars <- setdiff(chars,78025:78034)

        chars <- unlist(strsplit(intToUtf8(chars),""))
        chars <- enc2native(chars)
        for (i in 0:(size/z)*z) {
             for(j in 0:(size/z)*z) {
                 farve <- floor(lmap(i+j, 0, 2*size,0,255))
                 text(i,j, sample(chars,1),
                      cex = 3,
                      srt = sample(c(0,90,180,270),1),
                      col = rgb(farve,farve,farve, maxColorValue = 255)
                 )
             }
        }
        box()
    }
        
    output$png_download <- downloadHandler(
        filename = function() {
            paste("bitterlaser-",base64_enc(paste(input$seed, sep = ":")),".png", sep="")
        },
        content = function(file) {
            p <- plotInput()
            ggsave(file, device = "png", width = 8, height = 8)
        }
    )
    output$pdf_download <- downloadHandler(
        filename = function() {
            paste("bitterlaser-",base64_enc(paste(input$seed, sep = ":")),".pdf", sep="")
        },
        content = function(file) {
            p <- plotInput()
            ggsave(file, p, device = "pdf", paper = "A4", title = "Bitter Laser", width=8, height=8)
        }
    )
    output$bitterlaserplot <- renderPlot({
        print(plotInput())
    })

}

shinyApp(ui = ui, server = server)
