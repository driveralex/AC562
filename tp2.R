library(igraph)


erdos <- function(n,p)  ## Erdös-Rényi model necessarily has a Poisson degree ????? Parler des Components ! 
{ 
  
  combi <- n*(n-1)/2
  theoM <- p*combi
  igraph  <- make_empty_graph(directed = FALSE)
  igraph <- add_vertices(igraph,n, color = "red")
  actualM <-0
  for(i in 1 : n)
  {
    for(j in i : n)
    {
      if( runif(1) < p ) # Bonnus quel est l'effet sur les perf avec runif(combi). Faire un etude sur les perf en time et l'effet de l'ajout d'edge ? Quelque mot sur la ramdomness ?
      {
        #cat("Let's create an edge on <",i,"-",j,">\n")
        igraph <- add_edges(igraph, c(i,j))
        actualM <- actualM + 1
      }
     
    }
  }
  #cat("loo_val=",intval,"  calval=",combi,"\n")
  cat("Theorical M is :",theoM,"  Acutal M is :",actualM,"\n")
  igraph
}


Erdos_Renyi <- function(n,p)
{ 
  igraph  <- make_empty_graph(directed = FALSE)
  igraph <- add_vertices(igraph,n)
  for(i in 1 : n)
  {
    for(j in i : n)
    {
      if( runif(1) < p )
      {
        igraph <- add_edges(igraph, c(i,j))
      }
    }
  }
  mat <- as_adjacency_matrix(igraph,type= c("both"))
  print(mat)
  
  igraph
}

Erdos_Renyi_optimized <- function(n,p)
{
  nb_tri <- n*(n-1)/2
  mat <- diag(0,nrow = n, ncol = n)
  vect_rand <- runif(nb_tri , min = 0, max = 1) - p
  for(i in 1:nb_tri)
  {
    if(vect_rand[i]>0)
    {
      vect_rand[i] <- 0 
    }
    else
    {
      vect_rand[i] <- 1 
    }
  }
  mat[lower.tri(mat)] <- vect_rand
  mat <- t(mat)
  mat[lower.tri(mat)] <- vect_rand
  igraph <- graph_from_adjacency_matrix(mat,mode = c("undirected"))
  igraph
}

n <- 500
m <- 2
p <- 0.05

generate_ER <- function(n)
{
  p <- 0
  lst = list()
  for(i in 1:21)
  {
    lst[[i]] <- degree(Erdos_Renyi_optimized(n,p))
    p <- p + 0.05
  }
  
  find_xmax_histo <- function(lst_deg)
  {
    max_histo <- 0
    for( i in 2: length(lst_deg)-1)
    {
      if( max(lst_deg[[i]]) > max_histo )
      {
        max_x_histo <- max(lst_deg[[i]])
      }
    }
    max_x_histo
  }
  
  draw_histos <- function(lst_deg,x_max)
  {
    cat("Max histo = ",x_max)
    max_y_histo <- 0
    #for( i in 1: )
    for( i in 2: length(lst_deg)-1)
    {
      strmain <- c('Histogram of Erdos_Renyi for p=',(i-1)*0.05)
      vect_hist <- hist(lst_deg[[i]],
                        main=strmain,
                        xlim=c(0,x_max),
                        breaks=(x_max+2))
      if(max(vect_hist$counts) > max_y_histo )
      {
        max_y_histo <- max(vect_hist$counts)
      }
    }
    max_y_histo
  }
  xmax <- find_xmax_histo(lst)
  draw_histo(lst,xmax)
  # plot(NULL,
  #      xlabel="THE PLOT",
  #      xlim=c(0,xmax),
  #      ylim=c(0,ymax)
  #      )
  # temphis3 <- hist(lst[[3]])$counts
  # lines(temphis,
  #       lwd = 2,
  #       col = "green")
  # 
  # 
  # ER_histo <- function(lst_deg,hsacle)
  # {
  #   
  #   print(lst_deg)
  #   #tot <- sum(vect_deg, na.rm = FALSE)
  #   
  #   #pow <- powerlaw(100,hsacle)
  #   vect_hist <- hist(lst_deg[[3]],
  #        xlim=c(0,hsacle),
  #        breaks=(hsacle+2),
  #        col= rgb(146/256,39/256,143/256)
  #   )
  #   print(max(vect_hist$counts))
  #   #lines(pow[2,]*tot,lwd = 2,
  #   #     col = "green")
  # }
}







# start_time <- Sys.time()
# lstg <- generate_ER(n)
# end_time <- Sys.time()
# exec_time <- (end_time-start_time)
# print(exec_time)



p_average_length <- function(nb_nodes,nb_sample)
{
  p <- 0
  step <- 1/nb_sample
  mat <- vector(mode = "numeric",(2*(nb_sample+1)))
  dim(mat) <- c(2,nb_sample+1)
  for(i in 1: (nb_sample + 1))
  {
    mat[1,i] <- p
    ig <- Erdos_Renyi_optimized(nb_nodes,p)
    mat[2,i] <- average_length(ig)
    p <- p + step
    cat("Progress: ",(i/(nb_sample+1))*100,"%\n")
  }
  strmain <- c("Evolution of average length with p for Erdos-Renyi n=",nb_nodes)
  plot(x = mat[1,],
       y = mat[2,],
       main = strmain,
       type = "b",
       xlab = "p",
       ylab = "Average length",
       )
}

p_clustering_coef <- function(nb_nodes,nb_sample)
{
  p <- 0
  step <- 1/nb_sample
  mat <- vector(mode = "numeric",(2*(nb_sample+1)))
  dim(mat) <- c(2,nb_sample+1)
  for(i in 1: (nb_sample + 1))
  {
    mat[1,i] <- p
    ig <- Erdos_Renyi_optimized(nb_nodes,p)
    mat[2,i] <- transitivity(ig)
    p <- p + step
    cat("Progress: ",(i/(nb_sample+1))*100,"%\n")
  }
  strmain <- c("Evolution of the global clustering coefficient with p for Erdos-Renyi n=",nb_nodes)
  plot(x = mat[1,],
       y = mat[2,],
       main = strmain,
       type = "b",
       xlab = "p",
       ylab = "Global clustering coefficient",
  )
}

#p_clustering_coef(1000,20)
p_average_length(1000,20)

average_length <- function(igraph)
{
  sum <- 0
  for( i in 1: vcount(igraph) )
  {
    bfs_i <- bfs(igraph,i,
        unreachable = FALSE,
        dist = TRUE);
    for(j in i : vcount(igraph) )
    {
      if( i != j)
      {
        #cat("i:",i,"\t","j:",j,"\tdist:",bfs_i$dist[j],"\n");
        #bfs_i$dist[j] 
        if( !is.nan( bfs_i$dist[j] ) )
        {
          sum <- sum + bfs_i$dist[j]
        }
      }
    }
  }
  l <- 2*sum/( (vcount(igraph)*( vcount(igraph) -1 ))  )
}



ig <- Erdos_Renyi_optimized(n,p)



# Très facile pour le clustering coef ( reprendre fin du tp 1 )
# De meme pour la Q6.. 

l <- average_length(ig)
print(l)
# print(bfse$order)
# print(bfse$dist)


# Watts_Strogatz <- function(n,p,m)
# {
#   #igraph <- make_ring(n, directed = FALSE)
#   igraph  <- make_empty_graph(directed = FALSE)
#   test_color <- "#92278f"
#   igraph <- add_vertices(igraph,n, color = test_color)
#   
#   for(i in 1 : n)
#   {
#     cat("i=",i,"\n")
#     for(j in (-m-1) : m )
#     {
#       cat("i=",i," j=",j,"\n")
#       cat("res=",i+j,"\n")
#       tolink <- i+j
#       if(tolink < 1)
#       {
#         cat("infcase\n")
#       }else if(tolink > n)
#       {
#         cat("supcase\n")
#       }
#       else{
#         igraph <- add_edges(igraph, c(i,tolink))
#       }
#     }
#   }
#   plot(igraph)
#   
# }


