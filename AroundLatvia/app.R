library(shiny)
library(jsonlite) # base64_enc()
library(ggplot2)
library(wesanderson) #wes.palette

ui <- fluidPage(
    titlePanel("Around Latvia"),
    a("Source code:", href = "https://github.com/MartinMSPedersen/GenerativeR/tree/master/AroundLatvia"),
    wellPanel(
        sliderInput("seed",
                    "Seed value:",
                    min = 0,
                    max = 50000,
                    value = 40),
        sliderInput("slack1",
                    "Slack 1:",
                    min = 1,
                    max = 5,
                    value = 2),
        sliderInput("slack2",
                    "Slack 2",
                    min = 1,
                    max = 5,
                    value = 3),
        sliderInput("lines",
                    "Lines:",
                    min = 1,
                    max = 8,
                    value = 2)
    ),
    fluidRow(
        column(10,
               actionButton("action1","Set random values"),
               downloadButton("png_download", label = "Download as png"),
               downloadButton("pdf_download", label = "Download as pdf"),
               offset = 1)
    ),
    plotOutput("aroundplot", height = "640px", width = "640px"),
)


server <- function(input, output, session) {
    observe({
        input$action1
        updateSliderInput(session, "seed", value = sample(0:50000,1))
        updateSliderInput(session, "slack1", value = sample(1:5,1))
        updateSliderInput(session, "slack2", value = sample(1:5,1))
        updateSliderInput(session, "lines", value = sample(1:8,1))
    })
    

    plotInput <- function() {
        set.seed(input$seed)
        
        nothing <- 
            theme(
                plot.background = element_rect(fill = "#000000"),
                panel.background = element_rect(fill = "darkolivegreen"),
                panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                legend.position = "none"
            )
        
        slack1 <- input$slack1*8-7
        size1 <- 1
        
        slack2 <- input$slack2*8-7
        size2 <- 1
        
        line_mult <- input$lines*2
        
        palette <- sample(wes_palettes,1)
        farver <- as.vector(unlist(palette))

        num_lines <- line_mult*length(farver)
        
        seg1 <- data.frame(
            y = 1:num_lines,
            yend = (1:num_lines),
            x = rep(1,num_lines),
            xend = rep(num_lines,num_lines),
            size = sample(1:num_lines, replace = TRUE),
            col = sample(farver)
        )
        
        seg2 <- data.frame(
            y = 1:num_lines+slack1*runif(num_lines),
            yend = 1:num_lines+slack1*runif(num_lines),
            x = rep(1,num_lines),
            xend = rep(num_lines,num_lines),
            size = size1*sample(1:num_lines, replace = TRUE)
        )
        
        seg3 <- data.frame(
            y = 1:num_lines+slack2*runif(num_lines),
            yend = 1:num_lines+slack2*runif(num_lines),
            x = rep(1,num_lines),
            xend = rep(num_lines,num_lines),
            size = size2*sample(1:num_lines, replace = TRUE)
        )
        
        
        g <- ggplot()
        g <- g + geom_segment(data = seg1, colour = rep(farver,line_mult),
                              aes(x = x, y = y, xend = xend, yend = yend, size = size, alpha = 0.9),
        )
        g <- g + geom_segment(data = seg2, colour = sample(farver, length(farver)*line_mult, replace = TRUE),
                              aes(x = x, y = y, xend = xend, yend = yend, size = size, alpha = 0.9),
        )
        g <- g + geom_segment(data = seg3, colour = sample(farver, length(farver)*line_mult, replace = TRUE),
                              aes(x = x, y = y, xend = xend, yend = yend, size = size, alpha = 0.9),
        )
        
        g <- g + coord_polar()
        g <- g + nothing
        g
    }
    output$png_download <- downloadHandler(
        filename = function() {
            paste("aroundlatvia-",base64_enc(paste(input$seed,input$slack1,input$slack2,input$lines, sep = ":")),".png", sep="")
        },
        content = function(file) {
            p <- plotInput()
            ggsave(file, device = "png", width = 8, height = 8)
        }
    )
    output$pdf_download <- downloadHandler(
        filename = function() {
            paste("aroundlatvia-",base64_enc(paste(input$seed,input$slack1,input$slack2,input$lines, sep = ":")),".pdf", sep="")
        },
        content = function(file) {
            p <- plotInput()
            ggsave(file, p, device = "pdf", paper = "A4", title = "Around Latvia", width=8, height=8)
        }
    )
    output$aroundplot <- renderPlot({
        print(plotInput())
    })
    
}

shinyApp(ui = ui, server = server)


