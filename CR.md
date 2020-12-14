**AC562 - Complex Systems**

# Report of the practical work of complex system

*Alexandre Tisserand*

15/01/2021

## TP1

### 1) Introduction

The aim of this TP is to rise in competence with graph representations, metrics and the software R.

### 2) Representations of graph and first metrics

1. Matrix <-> List

A graph may be represented by its adjacency matrix or its adjacency list.
Below is a function that gets the adjacency list from the adjacency matrix :
```r
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

```
And now the opposite  a function that gets the adjacency matrix form the adjacency list : 
```r
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
```

2. List and distribution of degrees. 

Here is a function which return the list of degrees from the adjacency matrix or adjacency list :
```r
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
```
Here is a function which return the degrees distribution from the adjacency list ( a vérifier et posser une question quand a la définition de la question)
```r
degreeDistribution <- function(input)
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
```

3. Clustering coefficient

Faire ici un rappel de ce qu'est le clustering coef.. (maybe donner la def cc(i) = ...)

Here is a function that retruns the list of cluster coefficients from the adjacency matrix.
```r
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
```

### 3. Breadth-first search algorithm and applications

1/

Reminder : Breadth-first search algorithm give the distance between a given node to all the other node belonging to the same component.

The first implementation we use is "navie" and use the folloing principles :

Let n = number of nodes.
Let m = number of edges.
Let i = studied node.
Let s = desitination node.
Let d = number of round.

1/ Creation of an array Ds of n intergers to store the distances d(i,s). 
2/Initialisation of Ds with Ds(s)=0 and Ds(i) = -1, **quelque soit** i=!s. d=0.
3/ Find all nodes with distance d. If there is no, then stop.
4/ Find the neighbors of these nodes, assign those neighbors which don’t have a distance yet, with the distance d + 1.
5/ Set d = d + 1 and go to 3.



Bellow is a R implementation:
```r
breadth_first <- function(input_mat,node)
{
  k <- nrow(input_mat)
  outvect <- vector(mode = "numeric",k)
  for( i in 1:k )
  {
    if( i == node )
    {
      outvect[i] = 0
    }
    else{
      outvect[i] = -1
    }
  }
  end <- FALSE
  d <- 0
  while( end == FALSE  )
  {
    positivecondition <- list()
    for(i in 1:k)
    {
      if( outvect[i]== d)
      {
        positivecondition[length(positivecondition)+1] = i
      }
    }
    if(length(positivecondition) == 0 )
    {
      end <- TRUE
    }else{
      for(i in 1:length(positivecondition))
      {
        for(j in 1:k)
        {
          if( (areneighbours(input_mat,positivecondition[[i]],j) == TRUE) && (outvect[j] == -1) )
          {
            outvect[j] <- d + 1
          }
        }
      }
    }
    d <- d + 1
  }
  outvect
}
```
With the details of the areneighbours function bellow.

```r
areneighbours <- function(input,i,j)
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
```

The way we implement this could be better executed (notably the with the areneighbours) but with this algorithm, for a typical network, complexity is O(m + n log n).

A better implementation could be done with using a queue.
Here is a implementation with the stack algorithm : 

```r
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
  }
  ds
}
```
With the stack approch the complexity is samller. O(m + n)

For this tow apporch we have the result of a unique node. To have the complete matrice of distance as required in Q1, we repeat the operation for all nodes and aggregate the outputs. This is the purpose of the mat_D_breadth_first_stack function using the stack algorithm. 

```r
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
```
2/
For Question N°2: 
Diameter is a record of the largest distance observed in a component.
Once the Breadth-first search executed, finding the diameter for a given node is straightforward.
```r
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
```
3/
Closeness centrality represente

First we have to underline that if the network is not composed of a single unique component, the result of the closeness centrality must be take with care. It is possible to handle each component individualy, but this can bias the values. Indeed nodes in smaller component may have higher value. This is what the following example do:

```r
closeness_centrality_node <- function( input_mat , node ) 
{
  dist <- breadth_first_stack_list(input_mat,node)
  n <- length(dist)
  sum <- 0
  for(i in 1:n)
  {
    if( dist[[i]] > 0 ) #Here nodes that are not in the component are not take in account.faire la remarque page 47 (A modfifier ?)
    {
      sum <- sum + dist[[i]]
    }
  }
  output <- length(dist)/(sum)
  output
}
```
We use the very unelegant (but functional) breadth_first_stack_list function described bellow.

```r
breadth_first_stack_list <- function(input_mat,node)
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
```

The igraph package throw a waring when the components are not all linked togethers.

4/ Implementing the Betweennes centrality was for the hardest task to complete. In fact finding all the shortest path is the blocking point.
I tried to do this job with a recusive function knowing the lenght of every shortest path with a previous breadfirst search. 
Unfortunately I did not maneged to make this function work. The following programm visit all the nodes, but returning the target point and aggregate the coresting path is not so easy.

### 4. With the package Igraph

First you need to download the igraph libray via the R packet mananger: 
```r 
install.packages("igraph")
```
Then add load the package at the beging of the script
```r
library(igraph)
```

1/ 
The function undirectedIgraphFromAdjancyMatrix that defines an undirected graph in Igraph from an adjacency matrix.
```r
undirectedIgraphFromAdjancyMatrix <- function(input_mat)
{
  nbnode <- nrow(input_mat)
  graph  <- make_empty_graph(directed = FALSE)
  igraph <- add_vertices(graph,nbnode, color = "red")
  for(i in 1: nbnode)
  {
    for(j in i:nbnode)
    {
      if(input_mat[i,j] == 1)
      {
        igraph <- add_edges(igraph, c(i,j))
      }
    }
  }
  igraph
}
```

The function directedIgraphFromAdjancyMatrix do the same job but with a directed graph.
```r
directedIgraphFromAdjancyMatrix <- function(input_mat)
{
  nbnode <- nrow(input_mat)
  graph  <- make_empty_graph(directed = TRUE)
  igraph <- add_vertices(graph,nbnode, color = "red")
  for(i in 1: nbnode)
  {
    for(j in 1:nbnode)
    {
      if(input_mat[i,j] == 1)
      {
        igraph <- add_edges(igraph, c(i,j))
      }
    }
  }
  igraph
}
```

2/ 
Using igraph you can have **degree** with
```r
igraphdegrees <- function(igraph)
{
  degree(igraph)
}
```

The **global clustering coefficient** : 

latex : C :=number of triangles * 3 / number of connected triples

```r
global_clustering_coefficients <- function(igraph)    
{
  transitivity(igraph)
}
```

**Local clustering coefficient** for a given node. 

Reminder of the formula : Latex ! => Ci := number of pairs of neighbors of i that are connected/number of pairs of neighbors of i

```r
local_clustering_coefficient <- function(igraph,node)
{
  transitivity( igraph, type = "local")[node] 
}
```

The **normalized closeness centrality** is :

```r
igraph_Closeness_centrality_node <- function(mattestbis,node)
{
  closeness(ig, mode="in",normalized = FALSE)[node]*nrow(mattestbis)
}
```
**Betweenness_centrality** of a node is :
```r
igraph_betweenness_centrality_node <- function(input_mat,node)
{
  igraph <- undirectedIgraphFromAdjancyMatrix(input_mat)
  betweenness(igraph,normalized = FALSE)[node]
}
```
In the next section we will compare the results of the igraph function and the function we made.

TO BE COMPLETED ..........

3/ It's possible the change the color of the node acordingly to there caratecistic. 

**Color**

To determine the color of a node we use a linear repartition of atribute with rgb color.
You can use a color of refernce like the one used in the exemple: esisar's purple color.
```r
inputcolor.R <- 146
inputcolor.G <- 39
inputcolor.B <- 143

findcolor <- function(input_val,low_val, high_val,inputcolor )
{
  range <- high_val - low_val;
  step.R <- floor(inputcolor.R * (input_val-low_val) / range) 
  step.G <- floor(inputcolor.G * (input_val-low_val) / range)
  step.B <- floor(inputcolor.B * (input_val-low_val) / range)
  step <- c(step.R/256,step.G/256,step.B/256)
}
```