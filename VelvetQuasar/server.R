library(shiny)
library(jsonlite) # base64_enc
library(svglite)
library(broman) # hex2dec
library(ggplot2)


shinyServer(function(input, output, session) {
    observe({
        input$randomvalues
        updateSliderInput(session, "seed", value = sample(1:1e5, 1))
        updateSliderInput(session, "layout", value = sample(0:6, 1))
    })
    
    plotInput <- function() {
      set.seed(input$seed)
      size <- 1000
      plot.window(c(1,size), c(1,size))
      z <- 40
      always <- TRUE
      noise_level <- 10
      chars <- as.numeric(c("0x27D5", "0x27D6", "0x27D7")) # ⟕ ⟖ ⟗ 
      if (input$layout == 0) {
        chars <- 77824:(77824+1070)
      }
      if (input$layout == 1) {
        chars <- as.numeric("0x13254"):as.numeric("0x13256") # 𓉔 𓉕  𓉖
      }
      if (input$layout == 2) {
        chars <- as.numeric("0x13255"):as.numeric("0x13256")
      } 
      if (input$layout == 3) {
        chars <- as.numeric(c("0x1F796", "0x1F78B"))
      }
      if (input$layout == 4) {
        chars <- hex2dec("1F260"):hex2dec("1F265") # Luck, Prosperity, Longevity, Happiness, Wealth, Double-Happiness
        noise_level <- 0
      }
      if (input$layout == 5) {
        chars <- as.numeric(c("0x27D5", "0x27D6", "0x27D7")) # ⟕ ⟖ ⟗ 
      }        
      if (input$layout == 6) {
        chars <- as.numeric(97:121)
        z <- z*0.75
      }

      chars <- unlist(strsplit(intToUtf8(chars),""))
      chars <- enc2native(chars)

      for (i in 1:(size/z-1)*z) {
        for (j in 1:(size/z-1)*z) {
          if (always || runif(1) >= 0.5) {
            text(i + sample(-noise_level:noise_level, 1),j + sample(-noise_level:noise_level, 1), sample(chars,1),
                 cex = 3 , col = "black", srt = sample(c(0,90,180,270),1))
          } else {
            TRUE
          }
        }
      }
      box()
    }
    
    output$quasar <- renderPlot({
      print(plotInput())
    })

    output$png_download <- downloadHandler(
      filename = function() {
        paste0("velvetquasar-", base64_enc(paste(input$seed, input$layout, sep = ":")), ".png")
      },
      content = function(file) {
        png(file, width = 800, height = 800, units = "px", res = 100) 
        plotInput() 
        dev.off()
      }
    )

    output$pdf_download <- downloadHandler(
      filename = function() {
        paste0("velvetquasar-", base64_enc(paste(input$seed, input$layout, sep = ":")), ".pdf")
      },
      content = function(file) {
        pdf(file, title = "Velvet Quasar", paper = "A4", width = 8, height = 8)
        plotInput() # Call the plot function to draw on the pdf device
        dev.off() # Close the device
      }
    )
    
    output$svg_download <- downloadHandler(
      filename = function() {
        paste0("velvetquasar-", base64_enc(paste(input$seed, input$layout, sep = ":")), ".svg")
      },
      content = function(file) {
        svglite(file, width = 8, height = 8)
        plotInput()
        dev.off()
      }
    )
}) 

