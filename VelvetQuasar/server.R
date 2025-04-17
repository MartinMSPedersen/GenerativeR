library(shiny)
library(jsonlite) # base64_enc
library(svglite)
library(broman) # hex2dec
library(ggplot2)


shinyServer(function(input, output, session) {
    observe({
        input$randomvalues
        updateSliderInput(session, "seed", value = sample(1:1e5, 1))
        updateSliderInput(session, "layout", value = sample(0:7, 1))
    })
    
    plotInput <- function() {
      theme_nothing <- 
        theme(legend.position="none",
              panel.background = element_rect(fill="gray95"),
              panel.grid=element_blank(),
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              axis.text=element_blank(),
              panel.border = element_rect(colour = "black",fill = NA, size = 2))
        
        set.seed(input$seed)
        g <- ggplot(mtcars) +
             geom_point(aes(x=mpg,y=disp), size = 0.1, color = "black") +
             theme_nothing
        g
    }
    
    output$quasar <- renderPlot({
      print(plotInput())
    })

    output$png_download <- downloadHandler(
      filename = function() {
        paste("velvetquasar-",base64_enc(paste(input$seed,input$layout,sep = ":")),".png", sep="")
      },
      content = function(file) {
        p <- plotInput()
        ggsave(file, device = "png", width = 8, height = 8)
      }
    )

    output$pdf_download <- downloadHandler(
      filename = function() {
        paste("velvetquasar-",base64_enc(paste(input$seed,input$patch_layout,sep = ":")),".pdf", sep="")
      },
      content = function(file) {
        p <- plotInput()
        ggsave(file, p, device = "pdf", paper = "A4", title = "Velvet Quasar", width=8, height=8)
      }
    )
    
    output$svg_download <- downloadHandler(
      filename = function() {
        paste("velvetquasar-",base64_enc(paste(input$seed,input$patch_layout,sep = ":")),".svg", sep="")
      },
      content = function(file) {
        p <- plotInput()
        ggsave(file, p, device = "svg", width=8, height=8)
      }
    )
}) 

