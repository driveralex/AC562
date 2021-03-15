library(igraph)

probability_tot <- function(igraph)
{
  vect <- probability_vect(igraph)
  out <- sum(vect) / vcount(igraph)
}


initVoteBernoulli <- function(igraph,sway_vector,zelot_vector)
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
  
  for( i in 1: vcount(igraph))
  {
    if( zelot_vector[i] == 1 )
    {
      igraph <- set.vertex.attribute(igraph,"zelot", i ,value=1)
      igraph <- set.vertex.attribute(igraph,"vote", i ,value=1)
    }
    else if (zelot_vector[i] == 0 )
    {
      igraph <- set.vertex.attribute(igraph,"zelot", i ,value=0)
      igraph <- set.vertex.attribute(igraph,"vote", i ,value=0)
    }else
    {
      igraph <- set.vertex.attribute(igraph,"zelot", i ,value=-1)
    }
  }
  
  
  nb_sway <- length(sway_vector)
  for( i in 1 : nb_sway )
  {
    igraph <- set.vertex.attribute(igraph,"vote", sway_vector[i] ,value=1)
  }
  igraph
}

initZelot <- function(igraph,zelot_vector)
{
  for( i in 1: vcount(igraph))
  {
    if( zelot_vector[i] == 1 )
    {
      igraph <- set.vertex.attribute(igraph,"zelot", i ,value=1)
    }
    else if (zelot_vector[i] == 0 )
    {
      igraph <- set.vertex.attribute(igraph,"zelot", i ,value=0)
    }else
    {
      igraph <- set.vertex.attribute(igraph,"zelot", i ,value=-1)
    }
  }
  igraph
}

# probability_node <- function(igraph, node)
# {
#   
#   adj_lst_node <- as_adj_list(igraph)[[node]]
#   adj_mat <-  as_adjacency_matrix(igraph, sparse =FALSE)
#   vect_vote <- get.vertex.attribute(igraph, "vote")
#   outbis <- adj_mat%*%vect_vote
#   if( length( adj_lst_node ) == 0 )
#   {
#     return(0)  
#   }
#   sum <- 0
#   for(i in 1 : length( adj_lst_node ) )
#   {
#     sum <- sum + get.vertex.attribute(igraph, "vote", adj_lst_node[i] ) 
#   }
#   cat(outbis[node],"\t",sum,"\n")
#   out <- sum/ length( adj_lst_node )
#   out
# }

Nb_neighbors_vect <- function(igraph)
{
  mat <- as_adjacency_matrix(igraph)
  len <- nrow(mat)
  N_vect <- vector(mode = "numeric",len)
  for(i in 1 : len )
  {
    res <- sum(mat[i,])
    if(res > 0)
    {
      N_vect[i] <- sum(mat[i,])
    }else if(res == 0)
    {
      N_vect[i] <- 1
    }
    else{
      print("BUG - Nb_neighbors_vect")
    }
  }
  N_vect
}

probability_vect <- function(igraph,N_vect)
{
  adj_mat <-  as_adjacency_matrix(igraph, sparse = FALSE)
  vect_vote <- get.vertex.attribute(igraph, "vote")
  prob_vect <- adj_mat%*%vect_vote
  out <- prob_vect/N_vect
  out
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

vote_probability <- function(igraph,noise,N_vect)
{
  p_v <- probability_vect(igraph,N_vect)
  len <- length(p_v)
  f_p_opti <- vector(mode = "numeric",len)
  f_p_opti <- (1-2*noise)*p_v + noise
  f_p_opti
}

vote <- function(igraph,noise,N_vect,sway_vector,zelot_vector)
{
  
  f_p <- vote_probability(igraph,noise,N_vect)
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
  for( i in 1: vcount(igraph))
  {
    if( zelot_vector[i] == 1 )
    {
      igraph <- set.vertex.attribute(igraph,"vote", i ,value=1)
    }
    else if (zelot_vector[i] == 0 )
    {
      igraph <- set.vertex.attribute(igraph,"vote", i ,value=0)
    }
  }
  
  nb_sway <- length(sway_vector)
  for( i in 1:nb_sway )
  {
    igraph <- set.vertex.attribute(igraph,"vote", sway_vector[i] ,value=1)
  }
  igraph
}
declare_winner <- function(mat)
{
  end_vote_vect <- mat[,ncol(mat)]
  out <- vector(mode = "numeric",2)
  
  if( sum(end_vote_vect)/nrow(mat) == 0.5  )
  {
    cat("Deuce\n")
  }else if( sum(end_vote_vect)/nrow(mat) < 0.5 )
  {
    cat("Jerry win with ",(sum(end_vote_vect)/nrow(mat)),"\n")
    out[1] <- 0
  }else if( sum(end_vote_vect)/nrow(mat) > 0.5 )
  {
    cat("Tom win with ",(sum(end_vote_vect)/nrow(mat)),"\n")
    out[1] <- 1
  }
  out[2] <- (sum(end_vote_vect)/nrow(mat)) 
  out
}


simulation <- function(igraph,noise,time,sway_vector,zelot_vector)
{
  N_vect <- Nb_neighbors_vect(igraph)
  
  igraph <- initVoteBernoulli(igraph,sway_vector,zelot_vector)
  mat <- vector(mode = "numeric",(vcount(igraph)*time))
  dim(mat) <- c(vcount(igraph),time)
  for(i in 1: time)
  {
    mat[,i] <- getOneVote(igraph)
    igraph <- vote(igraph,noise,N_vect,sway_vector,zelot_vector)
    #cat(i," over ",time,"\n")
  }
  win <- declare_winner(mat)
}

n <- 501
k <- 3
q <- 2

igraph <- scale_free(n,k,q)
zelot_vector <- zelot(igraph)
igraph <- initZelot(igraph,zelot_vector)
sway_vector <- selection(igraph)


noise <- 0.001
time <- 3000

out <-  0
nb_simu <- 3
winner <- 0
score <- 0
for(i in 1: nb_simu )
{
  out <- simulation(igraph,noise,time,sway_vector,zelot_vector)
  winner <- winner + out[1]
  score <- score + out[2]
  cat("Progress:",i*100/nb_simu,"%\n")
}
cat("TOM win in ",winner/nb_simu,"%\n")
cat("Avreage score is",score/nb_simu,"%\n")



#print(out)






# ig <- initVoteBernoulli(ig)
# print( getOneVote(ig) )
# ig <- vote(ig,noise)
# print( getOneVote(ig) )
# 
# ig <- setAllVote(ig)
# out <- probability_node(ig,3)