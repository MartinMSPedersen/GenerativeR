library(shiny)
library(reshape2)
library(dplyr)
library(ggplot2)
library(jsonlite)

shinyServer(function(input, output, session) {
    observe({
        input$action1
        updateSliderInput(session, "seed", value = sample(1:1e5,1))
    })
    
    opt <- theme(legend.position="none",
                 panel.background = element_rect(fill="white"),
                 #panel.grid=element_blank(),
                 axis.ticks=element_blank(),
                 axis.title=element_blank(),
                 axis.text=element_blank(),
                 panel.border = element_rect(colour = "black",fill = NA, size = 3))
    
    hilbert <- function(m,n,r) {
        for (i in 1:n)
        {
            tmp <- cbind(t(m), m+nrow(m)^2)
            m <- rbind(tmp, (2*nrow(m))^r-tmp[nrow(m):1,]+1)
        }
        melt(m) %>% plyr::rename(c("Var1" = "x", "Var2" = "y", "value"="order")) %>% arrange(order)}
    
    plotInput <- function() {
        set.seed(input$seed)
        g <- ggplot(hilbert(m=matrix(sample(1:20,1)*0.1), n=sample(3:7,1), r=2), aes(x = x,y = y)) +  opt
        #g <- g +  coord_polar()
        g <- g + geom_path()
        print(g)
    }
    
    output$znamenskPlot <- renderPlot({
        print(plotInput())
    })
    

    output$png_download <- downloadHandler(
        filename = function() {
            paste("znamensk23-",base64_enc(as.character(input$seed)),".png", sep="")
        },
        content = function(file) {
            p <- plotInput()
            ggsave(file, device = "png", width = 8, height = 8)
        }
    )
    output$pdf_download <- downloadHandler(
        filename = function() {
            paste("znamensk23-",base64_enc(as.character(input$seed)),".pdf", sep="")
        },
        content = function(file) {
            p <- plotInput()
            ggsave(file, p, device = "pdf", paper = "A4", title = "Znamensk 23", width=8, height=8)
        }
    )



})
