#setwd("C:/Users/michael/Desktop/Scotus_Network_2/scotus/with_node_attributes/shiny_michael5_cum_in_deg")

#runApp() DON'T USE BECAUSE IT'll JUST RUN INDEFINITELY

library(shiny)
#library(datasets)
library(igraph)
library(lubridate)
library(rsconnect)
#library(PKI)

citation_net <- read.graph(file='scotus_net_EL_date.txt', format="gml")
#unique_years <- sort(unique(year(ymd(V(citation_net)$date))))


#rsconnect::setAccountInfo(name='unc-chapel-hill-scotus-research-bimc', token='1A3422380B0B14532FD2C66AA0756265', secret='ucDftIhBihfnXVprkNmbw4dFoCjciS+PEKllepQw')
#rsconnect::deployApp()

#unique_years <- year(ymd(sort(unique(V(citation_net)$date))))

#odd!
#unique_years
#sort(unique(V(citation_net)$date))


#runExample("01_hello") # a histogram
#runExample("02_text") # tables and data frames
#runExample("03_reactivity") # a reactive expression
#runExample("04_mpg") # global variables
#runExample("05_sliders") # slider bars
#runExample("06_tabsets") # tabbed panels
#runExample("07_widgets") # help text and submit buttons
#runExample("08_html") # Shiny app built from HTML
#runExample("09_upload") # file upload wizard
#runExample("10_download") # file download wizard
#runExample("11_timer") # an automated timer





# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #datasetInput <- reactive({
  #    #basically indegree that reactively changes depending on input$yearA
  #    unname(degree(citation_net, v = V(citation_net)[year(ymd(V(citation_net)$date)) == input$yearA], mode = c("in")))
  #})

  
  datasetInput3 <- reactive({
    unname(degree(citation_net, v = V(citation_net)[year(ymd(V(citation_net)$date)) <= input$animation], mode = c("in")))
  })
  
  output$cumulativePlot <- renderPlot({
    hist(datasetInput3(), 
         breaks = 39495,
         xlim = c(0, 300),
         xlab = paste("cumulative in-degree up to ", input$animation),
         ylim = c(0,100),
         main = paste("Histogram of Cumulative In-Degrees Up to ", input$animation, " of SCOTUS Network"),
         col =  "#FB8072",
         border =  "#FB8072"
    )
  })
  #?hist
  output$summary3 <- renderPrint({
    summary(datasetInput3()) 
  })
  
})






# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Histogram of Cumulative In-Degrees Up to Some Year of SCOTUS Network"),
    
    # Sidebar with a slider input for the number of bins
    sidebarLayout(
        sidebarPanel(
            
            #sliderInput("yearA",
            #            "Year:",
            #            min = 1754,
            #            max = 2015,
            #            value = 1754,
            #            sep = ""),
            sliderInput("animation", "Year:",
                        1754, 
                        2015,
                        1754,
                        sep = "",
                        animate = animationOptions(interval=1000, loop=FALSE, playButton="Play", pauseButton="Pause")
                        ),
            helpText("Note 1: ylim of the histogram ranges from 0 to 100 for
                     visualization purposes"),
            helpText("Note 2: xlim of the histogram ranges from 0 to 300 because every node in SCOTUS network has
in-degree less than 300, except 'id96405' in year 1906, which
                     has in-degree of 1295")
            
            
        ),
        
        
        
        
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("cumulativePlot"),
            verbatimTextOutput("summary3")
        )
        
    )
))

#?sliderInput
#?animationOptions
