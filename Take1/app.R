library(shiny)
library(jsonlite) # base64_enc()

ui <- fluidPage(
    titlePanel("Take 1 - Mondrian"),
    a("Source code:", href = "https://github.com/MartinMSPedersen/GenerativeR/tree/master/Take1"),
    wellPanel(
        sliderInput("seed",
                    "Seed value:",
                    min = 0,
                    max = 50000,
                    value = sample(0:50000,1)),
        sliderInput("subs",
                    "Subdivisions:",
                    min = 1,
                    max = 40,
                    value = 18),
        sliderInput("minSize",
                    "Minimum size",
                    min = 1,
                    max = 4,
                    value = 2),
        sliderInput("dist",
                    "Spacing",
                    min = 0,
                    max = 10,
                    value = 6)
    ),
    fluidRow(
        column(10,
               actionButton("action1","Set random values"),
               downloadButton("png_download", label = "Download as png"),
               downloadButton("pdf_download", label = "Download as pdf"),
               offset = 1)
    ),
    plotOutput("take1plot", height = "600px", width = "600px"),
)


server <- function(input, output, session) {
    observe({
        input$action1
        updateSliderInput(session, "seed", value = sample(0:50000,1))
        updateSliderInput(session, "subs", value = sample(1:40,1))
        updateSliderInput(session, "minSize", value = sample(1:4,1))
        updateSliderInput(session, "dist", value = sample(0:10,1))
    })
    
    plotInput <- function() {
        set.seed(input$seed)
        size <- 12000
        border <- 0
        farver <- c("#EBE0D8","#141414","#0D208C","#DB1A0F")
        subdivisions <- input$subs*10
        minsize <- input$minSize*400-300
        dist <- input$dist*12+16
        df <- data.frame(xleft = -24, xright = size+25, ytop = -24, ybottom = size+25)
        
        # functions
        randomColor <- function() {
            sample(farver,1)
        }
        
        subdivide <- function(df) {
            place <- sample(1:nrow(df), 1)
            what <- df[place,]
            if (runif(1) < 0.5) { # horizontal split
                if (what$xright - what$xleft < minsize) return(df)
                middle <- (what$xright+what$xleft)/2
                df <- rbind(df,c(middle+dist,what$xright,what$ytop,what$ybottom))
                df[place,"xright"] <- middle-dist
            } else { # vertical split
                if (what$ybottom - what$ytop < minsize) return(df)
                middle <- (what$ybottom+what$ytop)/2
                df <- rbind(df,c(what$xleft,what$xright,middle+dist,what$ybottom))
                df[place,"ybottom"] <- middle-dist
            }
            df
        }
        plot.window(xlim = c(1,size), ylim = c(1,size))
        while (subdivisions > 0) {
            subdivisions <- subdivisions - 1
            df <- subdivide(df)
        }
        lines(c(-24,size+25,size+25,-24,-24),c(-24,-24,size+25,size+25,-24), col = "black")
        for (i in 1:nrow(df)) {
            polygon(c(df[i,"xleft"], df[i,"xright"], df[i,"xright"], df[i,"xleft"]),
                    c(df[i,"ytop"], df[i,"ytop"], df[i,"ybottom"], df[i,"ybottom"]),
                    col = randomColor())
        }
    }
    output$png_download <- downloadHandler(
        filename = function() {
            paste("take1-",base64_enc(paste(input$seed,input$subs,input$minSize,input$dist, sep = ":")),".png", sep="")
        },
        content = function(file) {
            png(file, width = 2400, height = 2400)
            plot.new()
            par(mai=c(0,0,0,0))
            plotInput()
            dev.off()
        }
    )
    output$pdf_download <- downloadHandler(
        filename = function() {
            paste("take1-",base64_enc(paste(input$seed,input$subs,input$minSize,input$dist, sep = ":")),".pdf", sep="")
        },
        content = function(file) {
            pdf(file, paper = "A4", title = "Take 1", width=8, height=8)
            plot.new()
            par(mai=c(0,0,0,0))
            plotInput()
            dev.off()
        }
    )
    output$take1plot <- renderPlot({
        print(plotInput())
    })
    
}

shinyApp(ui = ui, server = server)


