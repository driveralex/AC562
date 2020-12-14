library(igraph)

mattestbis <- c( c(0,1,0,1,0,0,1), c(1,0,0,0,0,1,0), c(0,0,0,1,0,1,0), c(1,0,1,0,0,0,1), c(0,0,0,0,0,0,1), c(0,1,1,0,0,0,0),c(1,0,0,1,1,0,0) )
dim(mattestbis) <- c(7,7)

undirectedIgraphFromAdjancyMatrix <- function(input_mat)
{
  nbnode <- nrow(input_mat)
  graph  <- make_empty_graph(directed = FALSE)
  igraph <- add_vertices(graph,nbnode, color = "red")
  for(i in 1: nbnode)
  {
    for(j in i:nbnode)
    {
      #cat("i=",i, " j=",j," val=",input_mat[i,j],"\n")
      if(input_mat[i,j] == 1)
      {
        igraph <- add_edges(igraph, c(i,j))
      }
    }
  }
  igraph
}
# Posser la question sur la selection des noeud lorsque qu'il y'a plusieurs composants.
closeness_centrality_node <- function( input_mat , node ) 
{
  dist <- breadth_first_stack_list(input_mat,node)
  n <- length(dist)
  sum <- 0
  for(i in 1:n)
  {
    if( dist[[i]] > 0 ) #Here nodes that are not in the component are not take in account.faire la remarque page 47 (A modfifier ?)
    {
      sum <- sum + dist[[i]]
    }
  }
  output <- length(dist)/(sum)
  output
}

ig <- undirectedIgraphFromAdjancyMatrix(mattestbis)
plot(ig)

node <- 3
print(closeness_centrality_node(mattestbis,node))
igraph_Closeness_centrality_node <- function(mattestbis,node)
{
  closeness(ig, mode="in",normalized = FALSE)[node]*nrow(mattestbis)
}
print(igraph_Closeness_centrality_node(mattestbis,3))