library(shiny)
library(tidyverse)
library(jsonlite) # base64_enc
library(patchwork)


shinyServer(function(input, output, session) {
    observe({
        input$action1
        updateSliderInput(session, "seed", value = sample(1:1e5,1))
        updateSliderInput(session, "layout", value = sample(0:7,1))
    })
    
    pattern <- function(a, b, c = pi/2, d = pi/5, e = 4) {
        data_frame(
            x = accumulate(1:60, ~.x-sin((.y %% e)*c-ceiling((.y-1)/e)*d), .init = a),
            y = accumulate(1:60, ~.x+cos((.y %% e)*c-ceiling((.y-1)/e)*d), .init = b))
    }  
    
    t <- seq(0, 2*pi, length.out = 180)
    centers <- data_frame(x = sin(t), y = cos(t))
    
    
    theme_nothing <- 
        theme(legend.position="none",
              panel.background = element_rect(fill="gray95"),
              panel.grid=element_blank(),
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              axis.text=element_blank(),
              panel.border = element_rect(colour = "black",fill = NA, size = 2))
    
    plotInput <- function() {
        set.seed(input$seed)
        a <- runif(1,2,6)/100 # 0.05
        #a <- 0.1
        c <- runif(1,3,7) # 4.811
        d <- runif(1,2,8) # 5.301
        e <- sample(3:24,1) # 4
        
        c_incr <- ifelse(runif(1) > 0.5,runif(1)/10,0)
        if (c_incr == 0) {
            d_incr <- runif(1)/10
        } else {
            d_incr <- ifelse(runif(1) > 0.5,runif(1)/10,0)
        }
        #patch_layout <- sample(1:5,1)
        patch_layout <- as.numeric(input$layout)
        plots <- vector("list",)
        how_many <- 4
        if (patch_layout == 0) how_many <- 1
        if (patch_layout == 5) how_many <- 6
        if (patch_layout == 2) how_many <- 5
        if (patch_layout == 3) how_many <- 5
        if (patch_layout == 6) how_many <- 5
        if (patch_layout == 7) how_many <- 5
        plots <- vector("list",how_many)
        for (idx in 1:how_many) {
            plots[[idx]] <- 
            apply(centers, 1, function(r) pattern(a = r[1], b = r[2], c = c, d = d,e = e)) %>%
            bind_rows(.id="df") %>%
            ggplot() +
            geom_path(aes(x, y, group = df), alpha = a) +
            coord_fixed() +
            theme_nothing
            c <- c + c_incr
            d <- d + d_incr
        }
        if (patch_layout == 0) {
            patchwork <- plots[[1]]
        }
        if (patch_layout == 1) {
            patchwork <- (plots[[1]] + plots[[2]]) / ( plots[[3]] + plots[[4]] )
        }
        if (patch_layout == 2) {
            patchwork <- plots[[1]] / ( ( plots[[2]] + plots[[3]]) / (plots[[4]] + plots[[5]] ))
            patchwork
        }
        if (patch_layout == 3) {
            patchwork <- (plots[[1]] + plots[[2]]) / plots[[3]] / (plots[[4]] + plots[[5]])
        }
        if (patch_layout == 4) {
            patchwork <- plots[[1]]  / ( plots[[2]] + plots[[3]]) / plots[[4]]
        }
        if (patch_layout == 5) {
            patchwork <- ( plots[[1]]  + plots[[2]] + plots[[3]] ) / ( plots[[4]] + plots[[5]] + plots[[6]] )
        }
        if (patch_layout == 6) {
            patchwork <- ( plots[[1]] / plots[[2]] ) | plots[[3]] | ( plots[[4]] / plots[[5]] ) 
        }
        if (patch_layout == 7) {
            patchwork <- plots[[1]] | ( plots[[2]] / plots[[3]] ) | plots[[4]]
        }
        patchwork
    }
    output$sauna <- renderPlot({
        print(plotInput())
    })
    output$png_download <- downloadHandler(
        filename = function() {
            paste("highwaysauna-",base64_enc(paste(input$seed,input$layout,sep = ":")),".png", sep="")
        },
        content = function(file) {
            p <- plotInput()
            ggsave(file, device = "png", width = 8, height = 8)
        }
    )
    output$pdf_download <- downloadHandler(
        filename = function() {
            paste("highwaysauna-",base64_enc(paste(input$seed,input$layout,sep = ":")),".pdf", sep="")
        },
        content = function(file) {
            p <- plotInput()
            ggsave(file, p, device = "pdf", paper = "A4", title = "Highway Sauna", width=8, height=8)
        }
    )
    
})
