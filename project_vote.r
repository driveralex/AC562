initVoteBernoulli <- function(igraph)
{
  for( i in 1: vcount(igraph))
  {
    if( runif(1) < 1/2 )
    {
      igraph <- set.vertex.attribute(igraph,"vote", i ,value=1)
    }
    else
    {
      igraph <- set.vertex.attribute(igraph,"vote", i ,value=0)
    }
  }
  igraph
}

vote <- function(igraph,noise,N_vect)
{
  nb_node <- vcount(igraph)
  vect_influence <- vector(mode = "numeric",nb_node)
  vect_nb_neighb <- vector(mode = "numeric",nb_node)
  f_p <-  vector(mode = "numeric",nb_node)
  
  adj_mat <- as_adjacency_matrix(igraph)
  vect_vote <- get.vertex.attribute(igraph,"vote")
  for(i in 1: nb_node)
  {
    vect_influence[i] <-  vect_vote%*%adj_mat[i,]
    vect_nb_neighb[i] <- sum(adj_mat[i,])
    
    if(vect_nb_neighb[i] == 0)
    {
      f_p[i] <- vect_vote[i]
    }else{
      f_p[i] <- (1-2*noise)*(vect_influence[i]/vect_nb_neighb[i])+noise
    }
    
    if( f_p[i] > 0.5 )
    {
      igraph <- set.vertex.attribute(igraph,"vote",i,value = 1)
    }else{
      igraph <- set.vertex.attribute(igraph,"vote",i,value = 0)
    }
  }
  igraph
}

declare_winner <- function(mat)
{
  end_vote_vect <- mat[,ncol(mat)]
  
  if( sum(end_vote_vect)/nrow(mat) == 0.5  )
  {
    cat("Deuce\n")
  }else if( sum(end_vote_vect)/nrow(mat) < 0.5 )
  {
    cat("Jerry win\n")
  }else if( sum(end_vote_vect)/nrow(mat) > 0.5 )
  {
    cat("Tom win\n")
  }
}

simulation <- function(igraph,noise,time)
{
  igraph <- initVoteBernoulli(igraph)
  vote_plot(igraph,"Init")
  mat <- vector(mode = "numeric",(vcount(igraph)*time))
  dim(mat) <- c(vcount(igraph),time)
  for(i in 1: time)
  {
    igraph <- vote(igraph,noise)
    cat(i," over ",time,"\n")
  }
  declare_winner(mat)
  mat
}

n <- 20
p <- 0.1




vote_plot <- function(igraph,mainstr)
{
  nb_node <- vcount(igraph)
  for(i in 1: nb_node)
  {
    node_status <- get.vertex.attribute(igraph,"vote", i)
    if( node_status == 1 )
    {
      igraph <- set.vertex.attribute(igraph, 'color', i, rgb(1,0,0) )
    }
    else if (node_status == 0 )
    {
      igraph <- set.vertex.attribute(igraph, 'color', i, rgb(0.9,0.9,0.7) )
    }
    else{
      print("BUG PLOT attribute")
    }
  }
  plot(igraph, main = mainstr )
}

igraph <- sample_gnp(n, p, directed = FALSE, loops = FALSE)


noise <- 0.1
time <- 3

simulation(igraph,noise,time)


