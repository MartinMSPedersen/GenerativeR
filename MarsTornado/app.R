library(shiny)
library(jsonlite) # base64_enc()
library(ggplot2)
library(svglite)

ui <- fluidPage(
  titlePanel("Mars Tornado"),
  a("Source code:", href = "https://github.com/MartinMSPedersen/GenerativeR/tree/master/MarsTornado"),
  wellPanel(
  fluidRow(
    column(
      width = 12,
      sliderInput("seed",
                  "Seed value:",
                  min = 0,
                  max = 50000,
                  value = sample(0:50000,1))
    )
  ),
  fluidRow(
    column(
      width = 3, 
      sliderInput("radius1",
                  "Radius 1:",
                  min = 5,
                  max = 25,
                  value = 10)
    ),
    column(
      width = 3,
      sliderInput("radius2",
                  "Radius 2:",
                  min = 5,
                  max = 25,
                  value = 20)
    ),
    column(
      width = 3,
      sliderInput("noiselevel",
                  "Noise Level:",
                  min = 0,
                  max = 8,
                  value = 5)
    ),
    column(
      width = 3,
      sliderInput("howmany",
                  "Lines:",
                  min = 10,
                  max = 40,
                  value = 25)
    ),
  )),
  fluidRow(
    column(12,
           actionButton("action1","Set random values"),
           downloadButton("svg_download", label = "Download as svg"),
           downloadButton("png_download", label = "Download as png"),
           downloadButton("pdf_download", label = "Download as pdf")
    )
  ),
  plotOutput("mtornado", height = "600px", width = "600px")
)
#set.seed(1)
circle_alpha <- 10

server <- function(input, output, session) {
  observe({
    input$action1
    updateSliderInput(session, "seed", value = sample(0:50000,1))
    updateSliderInput(session, "radius1", value = sample(5:25,1))
    updateSliderInput(session, "radius2", value = sample(5:25,1))
    updateSliderInput(session, "noiselevel", value = sample(0:8,1))
    updateSliderInput(session, "howmany", value = sample(10:40,1))
  })
  
  # perlin noise from https://github.com/MartinMSPedersen/noiseR
  make.noise <- function() {
    perlin <- NULL
    f <- function(x, y = 0, z = 0, seed = 1, lod = 4, falloff = 0.5) {
      set.seed(seed)
      PERLIN_YWRAPB <- 4
      PERLIN_YWRAP <- bitwShiftL(1,PERLIN_YWRAPB)
      PERLIN_ZWRAPB <- 8
      PERLIN_ZWRAP <- bitwShiftL(1,PERLIN_ZWRAPB)
      PERLIN_SIZE <- 4095
      if (lod > 0) {
        perlin_octaves = lod
      } else {
        perlin_octaves = 4	# default to medium smooth
      }
      if (falloff > 0) {
        perlin_amp_falloff = falloff
      } else {
        perlin_amp_falloff <- 0.5 # 50% reduction/octave
      }
      
      scaled_cosine <- function(i) { 0.5*(1-cos(i*pi)) }
      
      if (is.null(perlin)) { perlin <<- runif(PERLIN_SIZE+1) }
      x <- abs(x)
      y <- abs(y)
      z <- abs(z)
      xi <- floor(x)
      yi <- floor(y)
      zi <- floor(z)
      xf <- x - xi
      yf <- y - yi
      zf <- z - zi
      r <- 0
      ampl <- 0.5
      
      o <- 0
      while (o<perlin_octaves) {
        of <- xi + bitwShiftL(yi,PERLIN_YWRAPB) + bitwShiftL(zi,PERLIN_ZWRAPB)
        
        rxf <- scaled_cosine(xf)
        ryf <- scaled_cosine(yf)
        
        n1 <- perlin[bitwAnd(of,PERLIN_SIZE)+1]
        n1 <- n1 + rxf * (perlin[bitwAnd((of + 1),PERLIN_SIZE)+1] - n1)
        n2 <- perlin[bitwAnd((of + PERLIN_YWRAP),PERLIN_SIZE)+1]
        n2 <- n2 + rxf * (perlin[bitwAnd((of + PERLIN_YWRAP + 1),PERLIN_SIZE)+1] - n2)
        n1 <- n1 + ryf * (n2 - n1)
        
        of <- of + PERLIN_ZWRAP
        n2 <- perlin[bitwAnd(of,PERLIN_SIZE)+1]
        n2 <- n2 + rxf * (perlin[bitwAnd((of + 1),PERLIN_SIZE)+1] - n2)
        n3 <- perlin[bitwAnd((of + PERLIN_YWRAP),PERLIN_SIZE)+1]
        n3 <- n3 + rxf * (perlin[bitwAnd((of + PERLIN_YWRAP + 1),PERLIN_SIZE)+1] - n3)
        n2 <- n2 + ryf * (n3 - n2)
        
        n1 <- n1 + scaled_cosine(zf) * (n2 - n1)
        
        r <- r + n1 * ampl
        ampl <- ampl *perlin_amp_falloff
        xi <- xi * 2
        xf <- xf * 2
        yi <- yi * 2
        yf <- yf * 2
        zi <- zi * 2
        zf <- zf * 2
        
        if (xf >= 1.0) {
          xi <- xi + 1
          xf <- xf - 1
        }
        if (yf >= 1.0) {
          yi <- yi + 1
          yf <- yf - 1
        }
        if (zf >= 1.0) {
          zi <- zi + 1
          zf <- zf - 1
        }
        
        o <- o + 1
      }
      r
    }
    Vectorize(f)
  } #  make.noise
  
  circleFunc <- function(center = c(0,0), radius = 1, npoints = 50, start = 0, end = 2*pi) {
    tt <- seq(start, end, length.out = npoints)
    data.frame(x = center[1] + radius * cos(tt), y = center[2] + radius * sin(tt))
  }
  
  plotInput <- function() {
    set.seed(input$seed)
    circle_alpha <- 10
    
    nothing <- theme(
      plot.background = element_rect(fill = "#000000"),
      panel.background = element_rect(fill = "darkolivegreen"),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )
    
    # two random colour. One dark and one light    
    f1 <- sample(0:255,3) # the dark colour
    while (sum(f1*c(0.2126,0.7152,0.0722)) < 0.85*255) { # brightness
      f1 <- sample(0:255,3)
    }
    f2 <- sample(0:255,3) # the light colour
    while (sum(f2*c(0.2126,0.7152,0.0722)) > 0.15*255) { # brightness
      f2 <- sample(0:255,3)
    }
    if (runif(1) > 0.5) { # swap them?
      farve1 <- rgb(f1[[1]], f1[[2]], f1[[3]], maxColorValue = 255)
      farve2 <- rgb(f2[[1]], f2[[2]], f2[[3]], maxColorValue = 255)
    } else {
      farve2 <- rgb(f1[[1]], f1[[2]], f1[[3]], maxColorValue = 255)
      farve1 <- rgb(f2[[1]], f2[[2]], f2[[3]], maxColorValue = 255)
    }
    
    howmany <- 10*input$howmany
    radius1 <- 10*input$radius1
    radius2 <- 10*input$radius2
    
    p <- ggplot()
    p <- p + theme_void() + theme(panel.background = element_rect(fill=farve1))
    angle <- 0
    angle_inc <- 2*pi/howmany
    
    noise <- make.noise()
    while (angle < 2*pi) {
      circle <- circleFunc(center = radius2*c(cos(angle), sin(angle)), radius = radius1)
      #circle <- circle[-1,]
      p <- p + 
        geom_path(alpha = circle_alpha/50, 
                  aes(x = x, y = y),
                  data = circle + input$noiselevel*runif(howmany,1,sqrt(radius2)), 
                  color = farve2)
      angle <- angle + angle_inc
    }
    p
  } # plotInput()
  
  output$mtornado <- renderPlot({
    print(plotInput())
  })
  output$svg_download <- downloadHandler(
    filename = function() {
      paste("marstornado-",base64_enc(paste(input$seed,input$radius1,input$radius2,input$noiselevel,input$howmany,sep = ":")),".svg", sep="")
    },
    content = function(file) {
      p <- plotInput()
      ggsave(file, device = "svg", width = 8, height = 8)
    }
  )
  output$png_download <- downloadHandler(
    filename = function() {
      paste("marstornado-",base64_enc(paste(input$seed,input$radius1,input$radius2,input$noiselevel,input$howmany,sep = ":")),".png", sep="")
    },
    content = function(file) {
      p <- plotInput()
      ggsave(file, device = "png", width = 8, height = 8)
    }
  )
  output$pdf_download <- downloadHandler(
    filename = function() {
      paste("marstornado-",base64_enc(paste(input$seed,input$radius1,input$radius2,input$noiselevel,input$howmany,sep = ":")),".pdf", sep="")
    },
    content = function(file) {
      p <- plotInput()
      ggsave(file, p, device = "pdf", paper = "A4", title = "Mars Tornado", width=8, height=8)
    }
  )
  
}

shinyApp(ui = ui, server = server)
