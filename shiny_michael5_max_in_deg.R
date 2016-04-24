#setwd("C:/Users/michael/Desktop/Scotus_Network_2/scotus/with_node_attributes/shiny_michael5_max_in_deg")

#runApp() DON'T USE BECAUSE IT'll JUST RUN INDEFINITELY

library(shiny)
#library(datasets)
library(igraph)
library(lubridate)
library(rsconnect)
#library(PKI)

citation_net <- read.graph(file='scotus_net_EL_date.txt', format="gml")
unique_years <- unique(sort(year(ymd(V(citation_net)$date))))


max_in_deg_year <- function(network, yearA) {
  in_deg <- unname(degree(network, v = V(network)[year(ymd(V(network)$date)) == yearA], mode = c("in")))
  
  index <- which.max(in_deg)
  in_deg[index]
}


max_in_deg <- list()
for (i in 1:length(unique_years)) {
  max_in_deg[i] <- max_in_deg_year(citation_net, unique_years[i])
}
max_in_deg

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
      unname(degree(citation_net, v = V(citation_net)[year(ymd(V(citation_net)$date)) == input$yearA], mode = c("in")))[
          which.max(unname(degree(citation_net, v = V(citation_net)[year(ymd(V(citation_net)$date)) == input$yearA], mode = c("in"))))
          ]
  })
  
  output$distPlot <- renderPlot({
      
      plot(unique_years, max_in_deg, 
           main = paste("Maximum In-Degree Plot of SCOTUS Network"),
           xlim = c(1754, 2015), #unique_years
           xlab = paste("years: 1754-2015"),
           ylim = c(0, 1295), #max in_degree in entire network is 1295 in year 1906
           ylab = paste("maximum in-degree"),
           col = "#FB8072"
           )
    
    #sum_in_deg <- summary(in_deg)
  })
  
  output$summary <- renderText({
      paste("maximum in-degree in ", input$yearA, " is: ", datasetInput())
  })
  
})







library(shiny)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Maximum In-Degree Plot of SCOTUS Network"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("yearA",
                  "Year:",
                  min = 1754,
                  max = 2015,
                  value = 1754,
                  sep = "",
                  animate = animationOptions(interval=1000, loop=FALSE, playButton="Play", pauseButton="Pause")
                  ),
      helpText("Note 1: ylim of the histogram ranges from 0 to 1295 because maximum in-degree in SCOTUS network is 1295 in year 1906")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("summary"),
      plotOutput("distPlot")
    )
    
  )
))

#?sliderInput
