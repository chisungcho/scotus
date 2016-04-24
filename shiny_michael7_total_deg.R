#setwd("C:/Users/michael/Desktop/Scotus_Network_2/scotus/with_node_attributes/shiny_michael7_total_deg")

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
    unname(degree(citation_net, v = V(citation_net)[year(ymd(V(citation_net)$date)) == input$yearA], mode = c("total")))
  })
  
  output$distPlot <- renderPlot({
    
    hist(datasetInput(), 
         breaks = 39450,
         xlim = c(0, 400),  #explained in lines 80-86 of michael7_total_deg
         xlab = paste("total-degree of ", input$yearA),
         ylim = c(0,100), #For visualization purposes set y limit low
         main = paste("Histogram of Total-Degrees in ", input$yearA, " of SCOTUS Network"),
         col =  "#ACA4E2",
         border =  "#ACA4E2"
    )
    
    #sum_in_deg <- summary(in_deg)
  })
  
  output$summary <- renderPrint({
    summary(datasetInput())
  })
  
})




shinyUI(fluidPage(
  
  # Application title
  titlePanel("Histogram of Total-Degrees for Each Year of SCOTUS Network"),
  
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
      helpText("Note 2: xlim of the histogram ranges from 0 to 400 because every node in SCOTUS network has
               total-degree less than 400, except 'id96405' in year 1906, which
               has total-degree of 1317"),
      helpText("Note 3: 'Error: Character(0)' for certain years of the histogram occurs due to no information of SCOTUS network
               in those years")
      
      ),
    
    
    
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      verbatimTextOutput("summary")
    )
    
      )
))

#?sliderInput
