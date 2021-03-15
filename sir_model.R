library(igraph)

plot_epidemic_curves <- function(mat_res,time)
{
  print(mat_res)
  plot( x = c(0:time), y = mat_res[2,],xlab="days",ylab="Percentage of people",type="b",col="green")
  lines(x= c(0:time), y = mat_res[3,],type="b",col="blue")
  lines(x= c(0:time), y = mat_res[1,],type="b",col="red")
  legend( ( time-(time*0.15) ), 90, legend=c("Suceptible", "Infected","Recovered"),
          col=c("green","blue","red"), lty=1:3, cex=0.8)
}

epidemic_plot <- function(igraph,mainstr)
{
  nb_node <- vcount(igraph)
  for(i in 1: nb_node)
  {
    node_status <- get.vertex.attribute(igraph,"epidemic", i)
    if( node_status == "i" )
    {
      igraph <- set.vertex.attribute(igraph, 'color', i, rgb(1,0,0) )
    }
    else if (node_status == "s" )
    {
      igraph <- set.vertex.attribute(igraph, 'color', i, rgb(0,1,0) )
    }
    else if(node_status == "r" )
    {
      igraph <- set.vertex.attribute(igraph, 'color', i, rgb(0,0,1) )
    }
    else{
      print("BUG PLOT attribute")
    }
  }
  for(i in 1: nb_node)
  {
    node_status <- get.vertex.attribute(igraph,"i_time", i)

    if( node_status > 0 )
    {
      igraph <- set.vertex.attribute(igraph, "label", i, node_status  )
    }else if( node_status == 0 )
    {
      igraph <- set.vertex.attribute(igraph, "label", i, "R"  )
    }else
    {
      igraph <- set.vertex.attribute(igraph, "label", i, "S"  )
    }


  }
  plot(igraph, main = mainstr )
}

init_epidemic <- function(igraph,n_0,n_d)
{
  nb_node <- vcount(igraph)
  Infected_vect <- floor(runif(n_0,min=1,max=nb_node))
  
  igraph <- set.vertex.attribute(igraph,"epidemic", value="s")
  igraph <- set.vertex.attribute(igraph,"i_time", value=-1 )
  for( i in 1 : n_0 )
  {
    igraph <- set.vertex.attribute(igraph,"epidemic", Infected_vect[i] ,value="i")
    igraph <- set.vertex.attribute(igraph,"i_time", Infected_vect[i] ,value= n_d )
  }
  epidemic_plot(igraph,"init")
  igraph
}

infected_heal <- function(igraph)
{
  nb_node <- vcount(igraph)
  for( i in 1: nb_node)
  {
    node_time_status <- get.vertex.attribute(igraph,"i_time", i)
    if( node_time_status > 0 )
    {
      #cat("Infected N°",i,"\tremaning time to heal",node_time_status,"\n")
      igraph <- set.vertex.attribute(igraph,"i_time", i ,value= (node_time_status - 1) )
      # node_time_status <- get.vertex.attribute(igraph,"i_time", i)
      # cat("Infected N°",i,"\ttime",node_time_status,"\n")
    }
    if( node_time_status == 0 )
    {
      igraph <- set.vertex.attribute(igraph,"epidemic", i ,value="r")
    }
  }
  igraph
}

transmission <- function(igraph,p_epidemic,n_d)
{
  igraph <- infected_heal(igraph)
  nb_node <- vcount(igraph)
  
  adj_mat <- as_adjacency_matrix(igraph, type = c("both"))
  
  vect_i <- vector(mode = "numeric",nb_node)
  vect_i_opo <- vector(mode = "numeric",nb_node)
  for(i in 1:nb_node)
  {
    node_status <- get.vertex.attribute(igraph,"epidemic", i)
    if( node_status == "i" )
    {
      vect_i[i] <- 1
      vect_i_opo[i] <- 0
    }
    else
    {
      vect_i[i] <- 0
      vect_i_opo[i] <- 1
    }
  }
  
  vect_r_op <- vector(mode = "numeric",nb_node)
  for(i in 1:nb_node)
  {
    node_status <- get.vertex.attribute(igraph,"epidemic", i)
    if( node_status == "r" )
    {
      vect_r_op[i] <- 0
    }
    else
    {
      vect_r_op[i] <- 1
    }
  }

 
  neigbour_infected_vect <- adj_mat%*%vect_i
  try_infect <- as.vector(neigbour_infected_vect)*vect_r_op*vect_i_opo
  for(i in 1:nb_node)
  {
    if(try_infect[i] > 0 )
    {
      for(j in 1:try_infect[i] )
      {
        if(runif(1) < p_epidemic)
        {
          igraph <- set.vertex.attribute(igraph,"epidemic", i ,value="i")
          igraph <- set.vertex.attribute(igraph,"i_time", i ,value= n_d )
          #cat("We inflect node N°",i,"\n")
        }
      }
    }
  }
  igraph
}

stat_days <- function(igraph)
{
  nb_node <- vcount(igraph)
  tot_i <- 0
  tot_s <- 0
  tot_r <- 0
  res <- vector(mode = "numeric")
  for(i in 1:nb_node)
  {
    epi <- get.vertex.attribute(igraph,"epidemic", i )
    if(epi == "i" )
    {
      tot_i <- tot_i + 1
    }
    if(epi == "s" )
    {
      tot_s <- tot_s + 1
    }
    if(epi == "r" )
    {
      tot_r <- tot_r + 1
    }
  }
  res[1] <- tot_i
  res[2] <- tot_s
  res[3] <- tot_r
  res
}

simulation <- function(igraph,p_epidemic,n_d,time)
{
  cat("Begin simulation \n")
  mat_res <- matrix(ncol = time+1,nrow = 3 )
  mat_res[,1 ] <- stat_days(igraph)
  for(i in 1:time )
  {
    cat("Day n°",i,"\n")
    igraph <- transmission(igraph,p_epidemic,n_d)
    restrans <- stat_days(igraph)
    mat_res[, (i+1) ] <- restrans
  }
  mat_res
}


R0_calc <- function(result_mat,nb_node, n_d)
{
  nb_day <- ncol(result_mat)
  gam <- 1/n_d
  outmat <- matrix(nrow = 6, ncol = nb_day)
  
  x <- result_mat[1,]/nb_node
  s <- result_mat[2,]/nb_node
  r <- result_mat[3,]/nb_node
  
  d_x <- vector(mode = "numeric",nb_day)
  d_s <- vector(mode = "numeric",nb_day)
  d_r <- vector(mode = "numeric",nb_day)
  beta <- vector(mode = "numeric",nb_day)
  gamma <- vector(mode = "numeric",nb_day)
  for(i in 2:nb_day )
  {
    d_x[i] <- x[i] - x[i-1]
    d_s[i] <- s[i] - s[i-1]
    d_r[i] <- r[i] - r[i-1]
  }
  for(i in 2:nb_day )
  {
    beta[i] <- d_s[i]/(s[i]*x[i])
  }
  print(beta/0.1)
}

n <- 200
p <- 0.01

p_epidemic <- 0.01
time <- 200

n_0 <- 10
n_d <- 10

ig <- Erdos_Renyi_optimized(n,p)
cat("Erdos Renyi created\n")
ig <- init_epidemic(ig,n_0,n_d)
cat("Infection initialized \n")
res <- simulation(ig,p_epidemic,n_d,time)
#R0_calc(res,n,n_d)
res <- res*100/n
plot_epidemic_curves(res,time)




# nb_node <- vcount(igraph)
# 
# for( i in 1: nb_node)
# {
#   node_time_status <- get.vertex.attribute(igraph,"i_time", i)
#   if( node_time_status > 0 )
#   {
#     igraph <- set.vertex.attribute(igraph,"i_time", i ,value = (node_time_status-1) )
#   }
# }
# 
# vect_i <- vector(mode = "numeric",nb_node)
# for(i in 1:nb_node)
# {
#   node_status <- get.vertex.attribute(igraph,"epidemic", i)
#   if( node_status == "i" )
#   {
#     vect_i[i] <- 1
#   }
#   else
#   {
#     vect_i[i] <- 0
#   }
# }
# 
# vect_s <- vector(mode = "numeric",nb_node)
# for(i in 1:nb_node)
# {
#   node_status <- get.vertex.attribute(igraph,"epidemic", i)
#   if( (node_status == "s"))
#   {
#     vect_s[i] <- 1
#   }
#   else
#   {
#     vect_s[i] <- 0
#   }
# }
# 
# 
# 
# adj_mat <- as_adjacency_matrix(igraph, type = c("both"))
# infected_vect <- adj_mat%*%vect_i
# 
# 
# cat("Stat(I)      ",vect_i,"\n")
# cat("Stat(I)(t+1) ",as.vector(infected_vect),"\n")
# cat("Stat(S)      ",vect_s,"\n")
# for(i in 1:nb_node)
# {
#   if(vect_s[i] == 1  ) # && vect_r[i] ==0
#   {
#     for( j in 1: infected_vect[i] )
#     {
#       if( runif(1) < p_epidemic )
#       {
#         
#         igraph <- set.vertex.attribute(igraph,"epidemic", i ,value="i")
#         igraph <- set.vertex.attribute(igraph,"i_time", i ,value=n_d)
#       }
#     }
#   }
# }
# 
# for( i in 1: nb_node)
# {
#   node_time_status <- get.vertex.attribute(igraph,"i_time", i)
#   if( node_time_status == 0 )
#   {
#     igraph <- set.vertex.attribute(igraph,"epidemic", i ,value="r")
#   }
# }

# igraph







