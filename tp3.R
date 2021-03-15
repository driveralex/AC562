library(igraph)

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

probability_node <- function(igraph, node)
{
  adj_lst_node <- as_adj_list(igraph)[[node]]
  if( length( adj_lst_node ) == 0 )
  {
    return(0)  
  }
  sum <- 0
  for(i in 1 : length( adj_lst_node ) )
  {
    sum <- sum + get.vertex.attribute(igraph, "vote",    adj_lst_node[i]    ) 
  }
  out <- sum/ length( adj_lst_node )
  out
}

probability_vect <- function(igraph)
{
  vc <- vcount(igraph)
  vect <- vector(mode = "numeric",vc)
  sum <- 0
  for( i in 1 : vc )
  {
    sum <- sum + probability_node(igraph,i)
    vect[i] <- probability_node(igraph,i)
  }
  vect
}

probability_tot <- function(igraph)
{
  vect <- probability_vect(igraph)
  out <- sum(vect) / vcount(igraph)
}

getOneVote <- function(igraph)
{
  vc <- vcount(igraph)
  vect <- vector(mode = "numeric",vc)
  for( i in 1 : vc)
  {
    vect[i] <- get.vertex.attribute(igraph, "vote",i) 
  }
  vect
}

vote_probability <- function(igraph,noise)
{
  p_v <- probability_vect(igraph)
  len <- length(p_v)
  f_p <- vector(mode = "numeric",len)
  for(i in 1 : len  )
  {
    f_p[i] <- (1-2*noise)*p_v[i] + noise
  }
  f_p
}

vote <- function(igraph,noise)
{
  start_time <- Sys.time()
  end_time <- Sys.time()
  f_p <- vote_probability(igraph,noise)
  end_time <- Sys.time()
  exec_time <- (end_time-start_time)
  print(exec_time)
  vc <- vcount(igraph)
  for( i in 1: vc)
  {
    if( runif(1) < f_p[i] )
    {
      igraph <- set.vertex.attribute(igraph,"vote", i ,value=0)
    }
    else
    {
      igraph <- set.vertex.attribute(igraph,"vote", i ,value=1)
    }
  }
  igraph
}

simulation <- function(igraph,noise,time)
{
  igraph <- initVoteBernoulli(igraph)
  mat <- vector(mode = "numeric",(vcount(igraph)*time))
  dim(mat) <- c(vcount(igraph),time)
  for(i in 1: time)
  {
    mat[,i] <- getOneVote(igraph)
    igraph <- vote(igraph,noise)
    cat(i," over ",time,"\n")
  }
  mat
}



n <- 100
p <- 0.05
noise <- 0.1

ig <- erdos(n,p)
glob_start_time <- Sys.time()

out <- simulation(ig,0.01,2)
glob_end_time<- Sys.time()
sim_time <- glob_end_time-glob_start_time 
cat("Sim time \n")
print(exec_time)
print(out)

# ig <- initVoteBernoulli(ig)
# print( getOneVote(ig) )
# ig <- vote(ig,noise)
# print( getOneVote(ig) )
# 
# ig <- setAllVote(ig)
# out <- probability_node(ig,3)