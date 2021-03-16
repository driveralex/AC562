#install.packages("igraph")
library(igraph)

nb_init_nodes <- function(nb_init_edges)
{
  n_cal <- (1+sqrt(1+8*nb_init_edges))/2
  if( n_cal == floor(n_cal) )
  {
    return(n_cal)
  }else{
    n_cal <- floor(n_cal)+1
  }
  n_cal
}

scale_free_init <- function(k)
{
  nb_node <- nb_init_nodes(k)
  nb_tri <- nb_node*(nb_node-1)/2
  mat <- matrix( nrow = nb_node, ncol = nb_node)
  diag(mat) <- 0
  vect_init <- c( rep(1, k) , rep(0, (nb_tri-k) ))
  mat[lower.tri(mat)] <- vect_init
  mat <- t(mat)
  mat[lower.tri(mat)] <- vect_init  
  igraph <- graph_from_adjacency_matrix(mat,mode = c("undirected"))
  igraph
}

scale_free_init_node <- function(k)
{
  mat <- vector(mode = "numeric",k*k)
  dim(mat) <- c(k,k)
  for(i in 1:k)
  {
    for(j in 1:k)
    {
      if( i == j )
      {
        mat[i,j] = 0
      }
      else
      {
        mat[i,j] = 1
      }
    }
  }
  undirectedIgraphFromAdjancyMatrix(mat)
}

scale_free <- function(n,k,q)
{
  igraph <- scale_free_init(k)
  add <- 0
  nodenb <- k
  while( nodenb < n)
  {
    igraph <- add_vertices(igraph,1)
    for(i in 1:q)
    {
      igraph <- add_edges(igraph,c( (nodenb-1+q) ,scale_free_degree_range(igraph)))
    }
    nodenb <- nodenb + 1
  }
  igraph
}

scale_free_degree_range <- function(igraph,random)
{
  random <- runif(1)
  vect_deg <- degree(igraph)
  nbedges <- sum(vect_deg)/2
  sum <- 0
    for(i in 1:length(vect_deg))
    {
        sum <- sum + vect_deg[i]/(nbedges*2)
        if( sum > random)
        {
          return(i)
        }
    }
}

  
sortdeg <- function(igraph)
{
  vect_deg <- degree(igraph)
  sorted <- sort.int(vect_deg,decreasing = TRUE)
  sortedind <- sort.int(vect_deg,decreasing = TRUE, index.return = TRUE)

  scale_max <- max(sortedind$x)
  mat <- vector(mode = "numeric",(2*scale_max))
  dim(mat) <- c(2,scale_max)
  for(i in 1: length(sortedind$x))
  {
    mat[2,sortedind$x[i]] <- mat[2,sortedind$x[i]]+1
  }
  for( i in length(mat[2,]):2 )
  {
    mat[2,i-1] <- mat[2,i-1] + mat[2,i] 
  }
  normal <- mat[2,1]
  for( i in 1 : length(mat[2,]))
  {
    mat[1,i] <- i
    mat[2,i] <- mat[2,i]/normal
  }
  max_x <- max(sortedind$x)
  print(max_x)
  plot(mat[1,],mat[2,],
       xlab = "Degree k",
       ylab = "Fraction of nodes Pk having degree k or greater",
       xlim = c(1,max_x),
       ylim = c(0.001,1),
       log ="xy"
       )
  # On remarque ?peut etre ... un pb avec k = 2.... 
}
# Utiliser la maximum likelihood technique

customhisto <- function(igraph)
{
  vect_deg <- degree(igraph)
  tot <- sum(vect_deg, na.rm = FALSE)
  hsacle <- max(vect_deg) 
  pow <- powerlaw(100,hsacle)
  hist(degree(ig),
       xlim=c(2,hsacle),
       breaks=(hsacle+2),
       col= rgb(146/256,39/256,143/256)
       )
  lines(pow[2,]*tot,lwd = 2,
  col = "green")
}

powerlaw <- function(hsacle,nbpt)
{
  a <- 2.5
  C <- 1
  step <- hsacle/nbpt
  
  mat <- vector(mode = "numeric",(2*nbpt))
  dim(mat) <- c(2,nbpt)
  
  for(i in 1:nbpt)
  {
    mat[1,i] <- i*step
    mat[2,i] <- C*i^(-a)
  }
  mat
}

fitness <- sample_fitness_pl(no.of.nodes=n,no.of.edges=4000,exponent.out = 2,
                  exponent.in = -1, loops = FALSE, multiple = FALSE,
                  finite.size.correction = TRUE)


n <- 1000
k <- 3
q <- 2

#print(scale_free_degree_range(scale_free_init(4),0.551))
ig <- scale_free(n,k,q)
cat("Nbof ef")
customhisto(ig)
sortdeg(ig)
# cat("Nbof ef")
# customhisto(fitness)
# sortdeg(fitness)
#plot(scale_free_init_node(k))
#plot(scale_free_init(k))
#plot(ig)

#customhisto(ig)

