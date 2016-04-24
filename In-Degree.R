#loading the data citation_net and citation_net_years
setwd("/Users/ccho/Desktop/Research/nodes/")
library(igraph)
citation_net <-read.graph( file='scotus_net_EL_date.txt', format="gml")
citation_net_years <- read.graph(file='scotus_net_EL_year.txt', format="gml")

# storing id93405 in "id"
id <- "id93405"
#finding the index of id93405 from citation_net_years
index <- which(V(citation_net_years)$name == id )
#storing the index of id93504 into node
node <- V(citation_net_years)[index]
#finding the year the case was born
year_born <- node$year
#creating a neighborhood subgraph for in-degree for this vertex. 
nbrhd_sg <- graph.neighborhood(citation_net_years,1, nodes=node, mode=c("in"), mindist=0)[[1]]
#sorts the years in chronological order from the id's found in the neighborhood subgraph for id93405
cite_years <- sort(as.integer(V(nbrhd_sg)$year))

#creating a function to be flexible with any year sequence for plotting the progression of in-degrees for id93405
plot_net_case <- function(year_sequence){
  #starting and ending the for loop from the case's birth year to the last year it was cited
  for(i in 1:length(year_sequence)) {
    #storing the year being graphed into the variable y
    y <- year_sequence[i]
    #graphing the neighborhood subgraph for that individual year
    nbrhd_sg <- graph.neighborhood(citation_net_years,1, nodes=node, mode=c("in"), mindist=0)[[1]]
    #ending the series of plots at the years less than or equal to year_sequence[i]
    neighborhood_years <- V(nbrhd_sg)$year <= y
    #storing the subgraph into variable sg 
    sg <- subgraph(nbrhd_sg, V(nbrhd_sg)[neighborhood_years])
    #printing the transitivity of the subgraph
    print(transitivity(sg))
    #plotting the subgraph for the particular year
    plot(sg, main = y)
  }
}
#the final plot with all the years spanning from the birth of the case to the year of its last citation
plot_net_case(cite_years)

#loading package animation to produce GIF
library(animation)
#saving GIF to computer
saveGIF(plot_net_case(cite_years), interval = 0.2, movie.name="network.gif")

