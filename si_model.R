library(igraph)

init_epidemic <- function(igraph,n_0)
{
  nb_node <- vcount(igraph)
  Infected_vect <- floor(runif(n_0,min=1,max=nb_node))
  
  igraph <- set.vertex.attribute(igraph,"epidemic", value="s")
  for( i in 1 : n_0)
  {
    igraph <- set.vertex.attribute(igraph,"epidemic", Infected_vect[i] ,value="i")
  }
  
  
  for(i in 1:nb_node)
  {
    #print( get.vertex.attribute(igraph,"epidemic", i) )
  }
  igraph
}

transmission <- function(igraph,p_epidemic)
{
  nb_node <- vcount(igraph)
  vect_i <- vector(mode = "numeric",nb_node)
  for(i in 1:nb_node)
  {
    node_status <- get.vertex.attribute(igraph,"epidemic", i)
    if( (node_status == "i") && ( runif(1) < p_epidemic ) )
    {
      vect_i[i] <- 1
    }
    else
    {
      vect_i[i] <- 0
    }
  }
  adj_mat <- as_adjacency_matrix(igraph, type = c("both"))
  infected_vect <- adj_mat%*%vect_i
  for(i in 1:nb_node)
  {
    if( infected_vect[i] == 1 )
    {
      igraph <- set.vertex.attribute(igraph,"epidemic", i ,value="i")
    }
  }
  igraph
}

simulation <- function(igraph,transmission,p_epidemic,time)
{
 
  for(i in 1:time )
  {
    mainstr <- c("Plot day N°",i-1)
    igraph <- transmission(igraph,p)
    epidemic_plot(igraph,mainstr )
  }
  igraph
}


n <- 50
p <- 0.1

p_epidemic <- 0.5
time <- 15

n_0 <- 2

ig <- Erdos_Renyi_optimized(n,p)
ig <- init_epidemic(ig,n_0)


simulation(ig,transmission(ig,p_epidemic),p_epidemic,time )










epidemic_plot <- function(igraph,mainstr)
{
  nb_node <- vcount(igraph)
  for(i in 1: nb_node)
  {
    node_status <- get.vertex.attribute(igraph,"epidemic", i)
    if( node_status == "i" )
    {
      igraph <- set.vertex.attribute(igraph, 'color', i, rgb(1,0,0) )
    }
    else if (node_status == "s" )
    {
      igraph <- set.vertex.attribute(igraph, 'color', i, rgb(0,1,0) )
    }
    else
    {
      igraph <- set.vertex.attribute(igraph, 'color', i, rgb(0,0,1) )
    }
  }
  plot(igraph, main= mainstr )
}
