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

distance <- function(input_mat,node_i,node_j)
{
  mat_D_breadth_first_stack(input_mat)[node_i,node_j]
}

mat_D_breadth_first_stack <- function(input_mat)
{
  k <- nrow(input_mat)
  outputmat <- vector(mode = "numeric")
  
  for(i in 1:k)
  {
    outputmat <- c(outputmat,breadth_first_stack(input_mat,i));
  }
  dim(outputmat) <- c(k,k)
  outputmat
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
  #print(ds)
  ds
}

shortfunction <- function(input_mat,node,d)
{
  aplist <- fromMatriceToList(input_mat)
  if(d>0)
  {
    for(i in 1:length(aplist[[node]]))
    {
      #print(aplist[[node]][i])
      shortfunction( input_mat , aplist[[node]][i], d-1  )
    }
  }
  print("node")
  print(node)
  print("d=")
  print(d)
}

mattestbis <- c( c(0,0,0,1,0,0,0), c(0,0,1,1,0,1,0), c(0,1,0,1,0,1,0), c(1,1,1,0,0,0,0), c(0,0,0,0,0,0,1), c(0,1,1,0,0,0,0),c(0,0,0,0,1,0,0) )
dim(mattestbis) <- c(7,7)
listtestbis <- fromMatriceToList(mattestbis)

print(distance(mattestbis,4,6))

shortfunction(mattestbis,4,distance(mattestbis,4,6))

