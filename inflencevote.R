library(igraph)

n <- 501
k <- 3
q <- 2

ig <- scale_free(n,k,m)

selection <- function(igraph)
{
  vect_len        <- n_length(igraph)
  vect_reach      <- reacheable_node(igraph)
  vect_close      <- how_closness(igraph)
  vect_bad_degree <- how_bad_is_degree(igraph)
  vect_zelot      <- is_zelot(igraph)
  
  coef_len   <- 3
  coef_reach <- 5
  coef_close <- 1.5
  
  coef_bad_degree <- 1
  alpha <- 0.013
  
  vect_rank_positif <- vect_len*coef_len + vect_reach*coef_reach + vect_close*coef_close
    
  vect_rank_negatif <- vect_bad_degree*coef_bad_degree + vect_zelot
  vect_rank_negatif <- high_cut(vect_rank_negatif,alpha)
  vect_rank_tot <- vect_rank_positif - vect_rank_negatif

  print(vect_rank_tot)
    
  nb_elect <- 10
  pool <- vector(mode = "numeric",nb_elect)
  ref <- 1
  condition_is_ok <- FALSE
  while(condition_is_ok == FALSE)
  {
    sorted <- sort.int(vect_rank_tot,decreasing = TRUE, index.return = TRUE)
    for(i in ref : (ref+nb_elect) )
    {
      pool[(i-ref)] <- sorted$ix[i] 
    }
    vect_deg <- degree(igraph)
    somm <- 0
    #print( vect_deg[pool] )
    for(i in 1:length(pool))
    {
      
      somm <- somm + vect_deg[pool[i]]
    }
    if(100 > somm)
    {
      cat("Number of rank down to respect condition=",ref," (SUM of degree=",somm,")\n")
      condition_is_ok <- TRUE
    }
    ref <- ref + 1
  }
  pool
}

high_cut <- function(inputvect,alpha)
{
  outputvect <- inputvect + alpha*inputvect*inputvect
}

how_closness <- function(igraph)
{
  tryCatch( vect_close <- closeness(igraph,normalized = TRUE) , warning=function() print("-") )
  if( min(vect_close) == max(vect_close) )
  {
    print("May have pb w: how_closness")
    return( c(rep(0,vcount(igraph)) ))
  }
  n_comp_min <- min(vect_close)
  vect_close <- vect_close-n_comp_min
  n_comp_max <- max(vect_close)
  vect_close <- vect_close*100/n_comp_max
  vect_close
}

is_zelot <- function(igraph)
{
  nb <- vcount(igraph)
  vect_zelot <- vector(mode = "numeric",nb)
  for( i  in 1: nb)
  {
    if( get.vertex.attribute(igraph, "zelot",i) == 1 || get.vertex.attribute(igraph, "zelot",i) == 0 )
    {
      vect_zelot[i] <- 99999999
    }else if(get.vertex.attribute(igraph, "zelot",i) == -1 )
    {
      vect_zelot[i] <- 0
    }
  }
  vect_zelot
}

how_bad_is_degree <- function(igraph)
{
  vect_deg <- degree(igraph) 
  if( min(vect_deg) == max(vect_deg) )
  {
    print("May have pb w: how_bad_is_degree")
    return( c(rep(0,vcount(igraph)) ))
  }
  n_comp_min <- min(vect_deg)
  vect_deg <- vect_deg-n_comp_min
  n_comp_max <- max(vect_deg)
  vect_deg <- (vect_deg*100/n_comp_max)+1
  vect_deg
}



zelot <- function(igraph)
{
  nb_node <- floor(0.4*vcount(igraph))
  nb_node
  vect_zelot <- vector(mode = "numeric",vcount(igraph))
  for(i in 1: vcount(igraph) )
  {
    if( runif(1)>0.8 )
    {
      vect_zelot[i] <- 1
    }else if( runif(1) < 0.2 )
    {
      vect_zelot[i] <- 0
    }else
    {
      vect_zelot[i] <- -1
    }
  }
  vect_zelot
}



reacheable_node <- function(igraph)
{
  Comp = clusters(igraph)
  vect_reach <- Comp$csize[Comp$membership]
  if( min(vect_reach) == max(vect_reach) )
  {
    #print("One giant Component")
    return( c(rep(0,vcount(igraph)) ))
  }
  n_comp_min <- min(vect_reach)
  vect_reach <- vect_reach-n_comp_min
  n_comp_max <- max(vect_reach)
  vect_reach <- vect_reach*100/n_comp_max
  vect_reach
}


n_length <- function(igraph)
{
  nb_node <- vcount(igraph)
  n_length_vect <- vector(mode = "numeric",nb_node)
  for( i in 1:  nb_node)
  {
    bfs_i <- bfs(igraph,i,
                 unreachable = FALSE,
                 dist = TRUE);
    #print(bfs_i$dist)
    n_length_vect[i] <- sum(bfs_i$dist,na.rm = TRUE)/nb_node  
  }
  n_length_max <- max(n_length_vect)
  
  for( i in 1:  nb_node)
  {
    if( n_length_vect[i] == 0 )
    {
      n_length_vect[i] <- n_length_max
    }
  }
  n_length_min <- min(n_length_vect)
  
  
  n_length_vect <- (n_length_vect) - n_length_min
  n_length_max <- max(n_length_vect)
  n_length_vect <- 100 - (n_length_vect*100/n_length_max)
}
    

#print(selection(ig))
