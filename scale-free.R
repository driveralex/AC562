#install.packages("igraph")
library(igraph)

scale_free <- function(n,k,q)
{
  igraph  <- make_empty_graph(directed = FALSE)
 
  igraph <- add_vertices(igraph,k, color =  rgb(146/256,39/256,143/256))
  
  plot(igraph)
}



n <- 100
k <- 3
q <- 2

scale_free(n,k,q)