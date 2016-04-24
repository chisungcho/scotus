#setwd("C:/Users/michael/Desktop/Scotus_Network_2/scotus/with_node_attributes/shiny_michael6_cum_out_deg")

#runApp() DON'T USE BECAUSE IT'll JUST RUN INDEFINITELY

library(shiny)
#library(datasets)
library(igraph)
library(lubridate)
library(rsconnect)
#library(PKI)

citation_net <- read.graph(file='scotus_net_EL_date.txt', format="gml")

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
  
  datasetInput <- reactive({
    #basically indegree that reactively changes depending on input$yearA
    unname(degree(citation_net, v = V(citation_net)[year(ymd(V(citation_net)$date)) <= input$yearA], mode = c("out")))
  })
  
  output$distPlot <- renderPlot({
    
    hist(datasetInput(), 
         breaks = 39450,
         xlim = c(0, 200), #Since max_out_deg is 197 in year 1949
         xlab = paste("cumulative out-degree up to ", input$yearA),
         ylim = c(0,100), #For visualization
         main = paste("Histogram of Cumulative Out-Degrees Up to ", input$yearA, " of SCOTUS Network"),
         col =  "#39BEB1",
         border =  "#39BEB1"
    )
    
  })
  
  output$summary <- renderPrint({
    summary(datasetInput())
  })
  
})







shinyUI(fluidPage(
  
  # Application title
  titlePanel("Histogram of Cumulative Out-Degrees Up to Some Year of SCOTUS Network"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("yearA", "Year:",
                  1754, 
                  2015,
                  1754,
                  sep = "",
                  animate = animationOptions(interval=1000, loop=FALSE, playButton="Play", pauseButton="Pause")
      ),
      helpText("Note 1: ylim of the histogram ranges from 0 to 100 for
               visualization purposes"),
      helpText("Note 2: xlim of the histogram ranges from 0 to 200 because maximum out-degree in the SCOTUS network
               is 197 in year 1949")
      
      ),
    
    
    
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      verbatimTextOutput("summary")
    )
    
      )
  ))

#?sliderInput
