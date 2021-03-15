path <- file.path("C:","Users","SESA458137","OneDrive - Schneider Electric","Travail","ESISAR","S6","AC 562 Complex Systems","Repo","AC562","USA_data_tocheck.csv")

election_data <- read.table(path,
            header = TRUE,
            sep = "\t",
            na.strings = "n/a",
            stringsAsFactors = FALSE)


nb_node <- 1000
p <- 0.01
igraph <- sample_gnp(nb_node, p, directed = FALSE, loops = FALSE)



vect_age <- election_data[,5]
vect_pop_age <- election_data[,6]

poptot <- sum(vect_pop_age)
popbynode <- poptot/nb_node
vect_node_pop_age <- floor(vect_pop_age/popbynode)

print(vect_node_pop_age)

iterator <- 0
while(sum(vect_node_pop_age) > 0)
{
  tirage <- floor(runif(1,min = 1,max=nb_node) )
  if( vect_node_pop_age[tirage] > 1 )
  {
    vect_node_pop_age[tirage] <- vect_node_pop_age[tirage] - 1
    igraph <- set.vertex.attribute(igraph,"age",iterator, value = tirage)
    iterator <- iterator + 1
  }else{
    print("bad luck")
  }
  print(sum(vect_node_pop_age))
}

