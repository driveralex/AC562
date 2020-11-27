print("------START-------")




#Function definition

fromMatriceToList <- function(mat)
{
  k <- nrow(mat)
  lst <- list()
  for(i in 1:k)
  {
    lst[[i]] <- numeric()
  }
  for (i in 1:k)
  {
    for(j in i:k)
    {
      if( mat[i,j] == 1)
      {
        lst[[i]] <- c(lst[[i]],j)
        lst[[j]] <- c(lst[[j]],i)
      }
    }
  }
  lst
}

fromListToMatrice <- function(lst)
{
  k <- length(lst)
  mat <- vector(mode = "numeric",k*k)
  dim(mat) <- c(k,k)
  for( i in 1:k )
  {
    l <- length(lst[[i]])
    for(j in 1:l)
    {
      mat[i,lst[[i]][j]] <- 1
    }
  }
  mat
}

listOfDegrees <- function(input)
{
  if( is.vector(input) == TRUE )
  {
    k <- length(input)
    lst <- list()
    for( i in 1:k)
    {
      lst[[i]] <- length(input[[i]])
    }
  }
  else if( is.matrix(input) == TRUE )
  {
    templst <- fromMatriceToList(input)
   
    k <- length(templst)
    lst <- list()
    for( i in 1:k)
    {
      lst[[i]] <- length(templst[[i]])
    }
  } else
  {
    print("Wrong input of function listOfDegrees")
  }
  lst
}

degreeDistribution <- function(input)  ## Ok mais a tester avec sérieux.. 
{
  lstdeg <- listOfDegrees(input)
  print(lstdeg[[which.max(lstdeg)]])
  
  lstdistrib <- list()
  lstdistrib <- replicate(lstdeg[[which.max(lstdeg)]],0)
  k <- length(lstdeg)
  for(i in 1:k)
  {
    lstdistrib[[lstdeg[[i]]]] <- lstdistrib[[lstdeg[[i]]]] + 1
  }
  print(lstdistrib)
  print("Endfunction")
}

clustering_coef <- function(input)
{
  len <- nrow(input)
  lst <- list()
  for(i in 1:len)
  {
    output <- 0
    for(k in 1:len)
    {
      for(p in 1 :len )
      {
        output <- output + input[i,k]*input[k,p]*input[p,i]
      }
    }
    lst[[i]] <- output
  }
  lst
}

areneighbour <- function(input,i,j)
{
  if( is.vector(input) == TRUE )
  {
    matinput <- fromListToMatrice(input)
    if(matinput[i,j] == 1 )
    {
      output <- TRUE
    }else
    {
      output <- FALSE
    }
  }
  else if( is.matrix(input) == TRUE )
  {
    if(input[i,j] == 1 )
    {
      output <- TRUE
    }else
    {
      output <- FALSE
    }
  }
  output
}

# Ci dessous une premier implementation de l'algo naif pour un unique node.
breadth_first <- function(input_mat,node)
{
  k <- nrow(input_mat)
  mat <- vector(mode = "numeric",k)
  for( i in 1:k )
  {
    if( i == node )
    {
      mat[i] = 0
    }
    else{
      mat[i] = -1
    }
  }
  end <- FALSE
  d <- 0
  while( end == FALSE  )
  {
    positiveconfition <- list()
    
    for(i in 1:k)
    {
      if( mat[i]== d)
      {
        positiveconfition[length(positiveconfition)+1] = i
      }
    }
    if(length(positiveconfition) == 0 )
    {
      end <- TRUE
    }else{
      for(i in 1:length(positiveconfition))
      {
        for(j in 1:k)
        {
          if( (areneighbour(input_mat,positiveconfition[[i]],j) == TRUE) && (mat[j] == -1) )
          {
            mat[j] <- d + 1
          }
        }
      }
    }
    d <- d + 1
  }
  print(mat)
}

breadth_first_stack <- function(input_mat,node)
{
  aplist <- fromMatriceToList(input_mat)
  stack <- vector(mode = "numeric")
  stack[1] <- node
  read <- 1
  write <- 2
  k <- nrow(input_mat)
  ds <- vector(mode = "numeric",k)
  for( i in 1:k )
  {
    if( i == node )
    {
      ds[i] = 0
    }
    else{
      ds[i] = -1
    }
  }
  
  #j <- 0
  while(read != write)
  {
    for(i in 1:length(aplist[[stack[read]]]))
    {
      if(ds[ aplist[[stack[read]]][i] ] == -1 )
      {
        ds[ aplist[[stack[read]]][i] ] <- ds[ stack[read]] + 1
        stack[write] <- aplist[[stack[read]]][i]
        write <- write +1
      }
    }
    read <- read + 1 
    #j <- j+1
  }
  print(ds)
}

breadth_first_stack_list <- function(input_mat,node) # On s'en sert dans closeness centrality
{
  output_vect <- breadth_first_stack(input_mat,node)
  k <- length(output_vect)
  output_list <- list()
  for(i in 1:k)
  {
    if(output_vect[i] >= 0)
    {
      output_list[[length(output_list)+1]] <-  output_vect[i]
    }
  }
  output_list
}

mat_D_breadth_first_stack <- function(input_mat)
{
  k <- nrow(input_mat)
  outputmat <- vector(mode = "numeric")
  
  for(i in 1:k)
  {
    outputmat <- c(outputmat,breadth_first_stack(input_mat,i));
  }
  print("----")
  dim(outputmat) <- c(k,k)
  print(outputmat)
}

diammeter <- function(input_mat,node)
{
  k <- nrow(input_mat)
  bfs_node <- breadth_first_stack(input_mat,node);
  diameter <- max(bfs_node)
  lst_compenent_memeber <- list()
  for(i in 1:k)
  {
    if(bfs_node[i] > 0 )
    {
      lst_compenent_memeber[[length(lst_compenent_memeber)+1]] <- i
    }
  }
  l <- length(lst_compenent_memeber)
  for(j in 1:l)
  {
    if( max(breadth_first_stack(input_mat,lst_compenent_memeber[[j]])) > diameter )
    {
      diameter <- max(breadth_first_stack(input_mat,lst_compenent_memeber[[j]]))
    }
  }
  diameter
}

closeness_centrality_node <- function( input_mat , node ) # Posser la question sur la selection des noeud lorsque qu'il y'a plusieurs composants.
{
  dist <- breadth_first_stack_list(input_mat,node)
  n <- length(dist)
  sum <- 0
  for(i in 1:n)
  {
    #print(dist[[i]])
    if( dist[[i]] > 0 ) # Ici les noeuds qui ne sont pas dans le composant courant ne sont pas pris en compte.. faire la remarque page 47 (A modfifier ?)
    {
      sum <- sum + dist[[i]]
    }
  }
  output <- length(dist)/sum
  output
}

closeness_centrality_vector <- function( input_mat )
{
  k <-nrow(input_mat)
  output_vect = vector(mode = "numeric",k)
  for(i in 1:k)
  {
    output_vect[i] <- closeness_centrality_node(input_mat,i)
  }
  print(output_vect)
}


mattestbis <- c( c(0,0,0,1,0,0,0), c(0,0,1,0,0,1,0), c(0,1,0,1,0,1,0), c(1,0,1,0,0,0,0), c(0,0,0,0,0,0,1), c(0,1,1,0,0,0,0),c(0,0,0,0,1,0,0) )
dim(mattestbis) <- c(7,7)
listtestbis <- fromMatriceToList(mattestbis)

mattest <- c( c(0,0,0,1) , c(0,0,1,1) , c(0,1,0,1) , c(1,1,1,0))
dim(mattest) <- c(4,4)
listtest <- fromMatriceToList(mattest);
#resmat <- fromListToMatrice(listtest);


#degreeDistribution(mattestbis)

#clustering_coef(resmat)
#breadth_first(mattestbis,1)
#breadth_first_stack(mattestbis,1)

#areneighbour(listtest,1,1)

#mat_D_breadth_first_stack(mattestbis)

#diammeter(mattestbis,6)

out <- closeness_centrality(mattestbis,7)
closeness_centrality_vector(mattestbis)
print(out)
# breadth_first_stack_list(mattestbis,4)
print("------END------")


#for(i in 1:k)
#{
  #Que ce passe t'il quand areneighbour(i=j)?
#  if(areneighbour(input_mat,stack[read],i))
#  {
#    if( ds[i] == -1)
 #   {
  #    print("d")
   #   print(d)
    #  ds[i] <- d+1
      
     # stack[write] <- i
      #write <- write+1
  #  }
  #  print("dsaft")
  #  print(ds)
  #}
#}

