library(shiny)
library(jsonlite) # base64_end

shinyServer(function(input, output, session) {
    observe({
        input$action1
        updateSliderInput(session, "seed", value = sample(1:1e5, 1))
    })
    
    
    theme_nothing <- 
        theme(legend.position="none",
              panel.background = element_rect(fill="gray95"),
              panel.grid=element_blank(),
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              axis.text=element_blank(),
              panel.border = element_rect(colour = "black",fill = NA, size = 2))
    
    output$laser <- renderPlot({
        print(plotInput())
    })
    output$png_download <- downloadHandler(
        filename = function() {
            paste("betterlaser-",base64_enc(paste(input$seed,sep = ":")),".png", sep="")
        },
        content = function(file) {
            p <- plotInput()
            ggsave(file, device = "png", width = 8, height = 8)
        }
    )
    output$pdf_download <- downloadHandler(
        filename = function() {
            paste("bitterlaser-",base64_enc(paste(input$seed,sep = ":")),".pdf", sep="")
        },
        content = function(file) {
            p <- plotInput()
            ggsave(file, p, device = "pdf", paper = "A4", title = "Bitter Laser", width=8, height=8)
        }
    )
})
