#Analyze the scotus network (Supreme Court of the United States)
#created: 04/24/16

setwd("C:/Users/michael/Desktop/Scotus_Network_2/scotus/with_node_attributes")

library(igraph)

citation_net <-read.graph( file='scotus_net_EL_date.txt', format="gml")

#BASIC NETWORK SUMMARIES AND EXAMPLES OF USEFUL PLOTS

#vertex count
vcount(citation_net)
#edge count
ecount(citation_net)

#get the dates from the vertex attributes
dates <- as.Date(V(citation_net)$date)

#get the years from the dates and convert to integers
years <-dates %>% 
  as.character %>% 
  strsplit('-') %>%  
  sapply(function(x) x[1]) %>% 
  as.integer

#Isolated Vertices vs. Connected Vertices:

#table of years
tb <- table(years)

#popular years
prob_years <- as.integer(names(tb[tb> 1000]))

#degrees for each vertex in citation_net
degs<- degree(citation_net)

#isolated vertices in entire network
sum(degs == 0)

#number of isolated vertices in popular years
sum(degs[years %in% prob_years] == 0)

#scatterplot of isolated vertices in entire network
years_covered <- unique(years)
nzero <- rep(0, length(years_covered))
for(j in 1:length(years_covered)){
  y <- years_covered[j]
  nzero[j] <- sum(degs[years == y] == 0)
}
plot(years_covered, nzero)

#connnected vertices
length(V(citation_net)[degs>0]) #26972 are connected in entire network
sum(degs[years %in% prob_years] > 0) #897 nodes in popular years are connected



#Playing around with dates using histograms:

#Histogram of number of court cases in each date:
hist(dates, breaks = 1000)

#Histogram of number of court cases in popular year, 2002:
dates2002 <- dates[('2002-01-01' < dates) & (dates < '2003-01-01')]
hist(dates2002,breaks = 365 , freq = T)

#Histogram of number of court cases in October, 2002:
dates2002_oct <- dates[('2002-10-05' <= dates) & (dates <= '2002-10-8')]
hist(dates2002_oct,breaks = 4 , freq = T)

#It was a busy day for SCOTUS on October 7th, 2002!
V(citation_net)$name[dates == "2002-10-07"]


#NOT IN REPORT (BECAUSE IMPLEMENTED IN SHINY BASICALLY):
#make the degree histogram
degrees <- unname(degree(citation_net, v = V(citation_net), mode = c("total")))
summary(degrees)

par(mfrow=c(1,2))
hist(degrees, 
     breaks = 39450, 
     xlim = c(0,100),
     xlab = 'degree',
     main = 'Histogram of degrees of SCOTUS network')

sum_deg <- summary(degrees)
legend("right", legend = rbind(names(sum_deg), sum_deg),
       bty = "n",
       cex = .6)

#log-log plot of degree distribution
deg_counts <- table(degrees)
ddegs <- as.integer(names(deg_counts))

plot(log(ddegs),log(deg_counts), 
     main = 'log log plot of degree distribution',
     xlab = 'log(degree)',
     ylab = 'log(counts)')
par(mfrow=c(1,1))

#make the in degree histogram
degrees_in <- unname(degree(citation_net, v = V(citation_net), mode = c("in")))
summary(degrees_in)

par(mfrow=c(1,2))
hist(degrees_in, 
     breaks = 39450, 
     xlim = c(0,100),
     xlab = 'in degree',
     main = 'Histogram of in degrees of the SCOTUS network')

sum_deg_in <- summary(degrees_in)
legend("right", legend = rbind(names(sum_deg_in), sum_deg_in),
       bty = "n",
       cex = .6)

deg_counts_in <- table(degrees_in)
degs_in <- as.integer(names(deg_counts_in))

#log-log plot of in-degree distribution:
plot(log(degs_in),log(deg_counts_in), 
     main = 'log log plot of in degree distribution',
     xlab = 'log(in degree)',
     ylab = 'log(counts)')
par(mfrow=c(1,1))

#make the out degree histogram
degrees_out <- unname(degree(citation_net, v = V(citation_net), mode = c("out")))
summary(degrees_out)

par(mfrow=c(1,2))
hist(degrees_out, 
     breaks = 39450, 
     xlim = c(0,100),
     xlab = 'out degree',
     main = 'Histogram of out degrees of the SCOTUS network')

sum_deg_out <- summary(degrees_out)
legend("right", legend = rbind(names(sum_deg_out), sum_deg_out),
       bty = "n",
       cex = .6)

#log-log plot out-degree distribution
deg_counts_out <- table(degrees_out)
degs_out <- as.integer(names(deg_counts_out))

plot(log(degs_out),log(deg_counts_out), 
     main = 'log log plot of out degree distribution',
     xlab = 'log(out degree)',
     ylab = 'log(counts)')
par(mfrow=c(1,1))



# ANALYSIS OF NETWORK CHARACTERISTICS:

#Vertex Closeness Centrality
vc <- closeness(citation_net)
which.max(vc) #so id2959750 has highest vertex closeness centrality value of 41238

#Vertex Betweenness Centrality
vb <- betweenness(citation_net, v = V(citation_net), directed = TRUE, weights = NULL,
                  nobigint = TRUE, normalized = FALSE)
which.max(vb) #so id118365 has highest vertex betweenness centrality value of 45823

#Vertex Eigenvector Centrality
ve <- evcent(citation_net)$vector
which.max(ve) # so id106514 has highest vertex eigenvector centrality value of 2293

#maximum in-degree
degrees_in <- unname(degree(citation_net, v = V(citation_net), mode = c("in")))
summary(degrees_in)
which.max(degrees_in) #at index 110
degrees_in[110] #max in-degree is 1295
V(citation_net)$name[110] # id96405

#maximum out-degree
degrees_out <- unname(degree(citation_net, v = V(citation_net), mode = c("out")))
summary(degrees_out)
which.max(degrees_out) #at index 2220
degrees_out[2220] #max out-degree is 197
V(citation_net)$name[2220] #id104616

#transitivity of entire SCOTUS network
transitivity(citation_net)

#Reciprocation coefficient of entire SCOTUS network
reciprocity(citation_net, mode="default") #dyads of directed edges
# [1] 0.003554964
reciprocity(citation_net, mode="ratio") #directed edges
# [1] 0.001780647

#Assortativity of entire SCOTUS network
assortativity.degree(citation_net)
# [1] 0.02934283

#average of shortest-path distances of SCOTUS
average.path.length(citation_net)
# [1] 5.537517

#longest distance of the shortest-paths of SCOTUS
diameter(citation_net)
# [1] 25


#Weakly-Connected Components
comps <- decompose.graph(citation_net) #important
table(sapply(comps, vcount))
#    1     2     3     4 26786 
# 36799    69     3     3     1 

#trasitivity of giant WCC
wcc_cluster <- decompose.graph(citation_net)[[2]]
transitivity(wcc_cluster)
# [1] 0.1140603

#reciprocation coefficeint of giant WCC
reciprocity(wcc_cluster)
# [1] 0.003548021

#assortativity of giant WCC
assortativity.degree(wcc_cluster)
# [1] 0.02916661

#average of shortest-path distance of giant WCC
average.path.length(wcc_cluster)
# [1] 5.537519

#longest distance of the shortest-paths of giant WCC
diameter(wcc_cluster)
# [1] 25



#vertex-cut (edge-cut)--finding the cut vertices or aticulation points, which disconnect the graph
citation_net.cut.vertices <- articulation.points(citation_net)
length(citation_net.cut.vertices)
# 2350 are cut vertices (good for future study)




#VISUALIZING NETWORK:

#First, the neighborhood of order-1 citing id118365, the vertex with highest beteweenness centrality value
id118365_neighborhood_list <- graph.neighborhood(citation_net, 1, "id118365", mode=c("in"))
id118365_neighborhood_subgraph <- connect.neighborhood(id118365_neighborhood_list[[1]], 1, mode=c("in")) 
plot(id118365_neighborhood_subgraph)

#Second, the neighborhood of order-1 cited by id118365, the vertex with highest betewenness centrality value
igraph.options(edge.arrow.size=0.5)
id118365_neighborhood_list <- graph.neighborhood(citation_net, 1, "id118365", mode=c("out"))
id118365_neighborhood_subgraph <- connect.neighborhood(id118365_neighborhood_list[[1]], 1, mode=c("out")) 
plot(id118365_neighborhood_subgraph)

#Finally, combination of previous two graphs
id118365_neighborhood_list <- graph.neighborhood(citation_net, 1, "id118365", mode=c("all"))
id118365_neighborhood_subgraph <- connect.neighborhood(id118365_neighborhood_list[[1]], 1, mode=c("all")) 
plot(id118365_neighborhood_subgraph)





#First, the neighborhood of order-1 citing id2959750, the vertex with highest closeness centrality value
id2959750_neighborhood_list <- graph.neighborhood(citation_net, 1, "id2959750", mode=c("in"))
id2959750_neighborhood_subgraph <- connect.neighborhood(id2959750_neighborhood_list[[1]], 1, mode=c("in")) 
plot(id2959750_neighborhood_subgraph)

igraph.options(vertex.size=3, vertex.label=NA, edge.arrow.size=0.3)
#Second, the neighborhood of order-1 cited by id2959750, the vertex with highest closeness centrality value
id2959750_neighborhood_list <- graph.neighborhood(citation_net, 1, "id2959750", mode=c("out"))
id2959750_neighborhood_subgraph <- connect.neighborhood(id2959750_neighborhood_list[[1]], 1, mode=c("out")) 
plot(id2959750_neighborhood_subgraph)

#Finally, combination of previous two graphs
id2959750_neighborhood_list <- graph.neighborhood(citation_net, 1, "id2959750", mode=c("all"))
id2959750_neighborhood_subgraph <- connect.neighborhood(id2959750_neighborhood_list[[1]], 1, mode=c("all")) 
plot(id2959750_neighborhood_subgraph)





#First, the neighborhood of order-1 citing id106514, the vertex with highest eigenvector centrality value
id106514_neighborhood_list <- graph.neighborhood(citation_net, 1, "id106514", mode=c("in"))
id106514_neighborhood_subgraph <- connect.neighborhood(id106514_neighborhood_list[[1]], 1, mode=c("in")) 
plot(id106514_neighborhood_subgraph)

#Second, the neighborhood of order-1 cited by id106514, the vertex with highest eigenvector centrality value
id106514_neighborhood_list <- graph.neighborhood(citation_net, 1, "id106514", mode=c("out"))
id106514_neighborhood_subgraph <- connect.neighborhood(id106514_neighborhood_list[[1]], 1, mode=c("out")) 
plot(id106514_neighborhood_subgraph)

#Finally, combination of previous two graphs
id106514_neighborhood_list <- graph.neighborhood(citation_net, 1, "id106514", mode=c("all"))
id106514_neighborhood_subgraph <- connect.neighborhood(id106514_neighborhood_list[[1]], 1, mode=c("all")) 
plot(id106514_neighborhood_subgraph)





#id104616 (most max_out_deg in entire network)
id104616_neighborhood_list <- graph.neighborhood(citation_net, 1, "id104616", mode=c("out"))
id104616_neighborhood_subgraph <- connect.neighborhood(id104616_neighborhood_list[[1]], 1, mode=c("out")) 
plot(id104616_neighborhood_subgraph)





#id96405 (most max_in_deg in entire network)
id96405_neighborhood_list <- graph.neighborhood(citation_net, 1, "id96405", mode=c("in"))
id96405_neighborhood_subgraph <- connect.neighborhood(id96405_neighborhood_list[[1]], 1, mode=c("in")) 
plot(id96405_neighborhood_subgraph)
