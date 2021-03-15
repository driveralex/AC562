#install.packages("igraph")
library(igraph)

Watts_Strogatz <- function(n,p,m)
{
  igraph <- make_ring(n, directed = FALSE)
  for(i in 1 : n)
  {
    #cat("i=",i,"\n")
    for(j in (-m-1) : m )
    {
      # cat("i=",i," j=",j,"\n")
      # cat("res=",i+j,"\n")
      tolink <- i+j
      if(tolink < 1)
      {
        #cat("infcase\n")
        
        #cat("We want to add ",i,"->",tolink+n,"\n")
        isLinked <- FALSE
        adjlst_i <- as_adj_list(igraph)[[i]]
        #print(length(adjlst_i)  )
        for(j in  1: length(adjlst_i) )
        {
          #cat("WuT?",adjlst_i[[j]],"\n")
          if(adjlst_i[[j]] == tolink+n)
          {
            isLinked <- TRUE
          }
        }
        if(isLinked == FALSE)
        {
          igraph <- add_edges(igraph, c(i,tolink+n))
        }
        
        
      }else if(tolink > n)
      {
        
      }else if(tolink == i)
      {
        
      }
      else{
        
        
        #cat("We want to add ",i,"->",tolink,"\n")
        isLinked <- FALSE
        adjlst_i <- as_adj_list(igraph)[[i]]
        #print(length(adjlst_i)  )
        for(j in  1: length(adjlst_i) )
        {
           #cat("WuT?",adjlst_i[[j]],"\n")
          if(adjlst_i[[j]] == tolink)
          {
            isLinked <- TRUE
          }
        }
        if(isLinked == FALSE)
        {
          igraph <- add_edges(igraph, c(i,tolink))
        }
      }
    }
  }
  #Now let's suppress some edges..
  edgelst <- as_edgelist(igraph)
  nbedge <- nrow(edgelst)
  cat("Total of start edges",nbedge,"\n");
  nbremouve <- 0
  for(i in 1: nbedge)
  {
    #cat("Iteration N°=",i,"\n")
    if(runif(1) < p )
    {
      #cat("<==============>\n")
      # We use a version where mean Degree is stricitly equals to 2*m
      igraph <- delete_edges(igraph, c(edgelst[i,1],edgelst[i,2]))
      #cat("We remouve ",edgelst[i,1]," and ",edgelst[i,2],"\n")
      cat("Touch\n");
      nbremouve <- nbremouve +1
      #And now we re add edges on completly random way...
     
      # we consider that  runif(1) < 1 othewise if runif(1) == 1 => bug
      # We also need to ensure that there is not already a edge..
      
     # cat("And replace it by ",start_edge," and ",end_edge ,"\n")
    }
  }
  
  mid_edgelst <- as_edgelist(igraph)
  mid_nbedge <- nrow(mid_edgelst)
  cat("Total of mid edges",mid_nbedge,"\n");
  
  # Pk on fait 2* add edge pour 1* rm edges ?
  
  for(i in 1 : (2*nbremouve) )
  {
    newLinkCreated <- FALSE
    while( newLinkCreated == FALSE )
    {
      start_edge <- floor(runif(1)*n+1)
      end_edge   <- floor(runif(1)*n+1)
      potential_adj_lst <-  as_adj_list(igraph)[[start_edge]]
      isFree <- TRUE
      for(i in 1: length(potential_adj_lst))
      {
        
        if( potential_adj_lst[[i]] == end_edge )
        {
          cat("WARNING : There is allready a link, chossing a another one\n")
          isFree <- FALSE
        }
      }
      if( start_edge == end_edge )
      {
        isFree <- FALSE
      }
      if( isFree == TRUE )
      {
        cat("RM\n")
        igraph <- add_edges(igraph, c(start_edge,end_edge) )
        newLinkCreated <- TRUE
      }
    }
  }
  
  
  end_edgelst <- as_edgelist(igraph)
  end_nbedge <- nrow(end_edgelst)
  cat("Total of end edges",end_nbedge,"\n");
  
  
  
  cat("Total of ",nbremouve," edges remouved\n");
  
  #print(end_edgelst)
  
  
  print(mean(degree(igraph)))
  plot(igraph)
  
}

n <- 190
m <- 2
p <- 0.2

Watts_Strogatz(n,p,m)









# 
# newLinkCreated <- FALSE
# while( newLinkCreated == FALSE )
# {
#   start_edge <- floor(runif(1)*n+1)
#   end_edge   <- floor(runif(1)*n+1)
#   potential_adj_lst <-  as_adj_list(igraph)[[start_edge]]
#   isFree <- TRUE
#   for(i in 1: length(potential_adj_lst))
#   {
#     
#     if( potential_adj_lst[[i]] == end_edge )
#     {
#       cat("WARNING : There is allready a link, chossing a another one\n")
#       isFree <- FALSE
#     }
#   }
#   if( start_edge == end_edge )
#   {
#     isFree <- FALSE
#   }
#   if( isFree == TRUE )
#   {
#     cat("ICI??\n")
#     igraph <- add_edges(igraph, c(start_edge,end_edge) )
#     newLinkCreated <- TRUE
#   }
# }