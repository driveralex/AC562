library(igraph)

mattestbis <- c( c(0,1,0,1,0,0,1), c(1,0,0,0,0,1,0), c(0,0,0,1,0,1,0), c(1,0,1,0,0,0,1), c(0,0,0,0,0,0,1), c(0,1,1,0,0,0,0),c(1,0,0,1,1,0,0) )
dim(mattestbis) <- c(7,7)

ig <- undirectedIgraphFromAdjancyMatrix(mattestbis)
plot(ig)

igraph_betweenness_centrality_node <- function(input_mat,node)
{
  igraph <- undirectedIgraphFromAdjancyMatrix(input_mat)
  betweenness(igraph,normalized = FALSE)[node]
}


print(igraph_betweenness_centrality_node(mattestbis,1))