library(igraph)


erdos <- function(n,p)  ## Erdös-Rényi model necessarily has a Poisson degree ????? Parler des Components ! 
{ 
  
  combi <- n*(n-1)/2
  theoM <- p*combi
  igraph  <- make_empty_graph(directed = FALSE)
  igraph <- add_vertices(igraph,n, color = "red")
  
  
  
  actualM <-0
  for(i in 2 : n)
  {
    for(j in i : n)
    {
      #cat("i=",i,"  j=",j,"\n")
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
  plot(igraph)
}


Watts_Strogatz <- function(n,p,m)
{
  #igraph <- make_ring(n, directed = FALSE)
  igraph  <- make_empty_graph(directed = FALSE)
  test_color <- "#92278f"
  igraph <- add_vertices(igraph,n, color = test_color)
  
  for(i in 1 : n)
  {
    cat("i=",i,"\n")
    for(j in (-m-1) : m )
    {
      cat("i=",i," j=",j,"\n")
      cat("res=",i+j,"\n")
      tolink <- i+j
      if(tolink < 1)
      {
        cat("infcase\n")
      }else if(tolink > n)
      {
        cat("supcase\n")
      }
      else{
        igraph <- add_edges(igraph, c(i,tolink))
      }
    }
  }
  plot(igraph)
  
}

n <- 30
m <- 2
p <- 0.

Watts_Strogatz(n,p,m)


