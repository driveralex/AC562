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

age_it <- 1
node_it <- 1
while(sum(vect_node_pop_age) > 0)
{
  while(vect_node_pop_age[age_it] > 0)
  {
    igraph <- set.vertex.attribute(igraph,"age",node_it, value = age_it)
    vect_node_pop_age[age_it] <- vect_node_pop_age[age_it] - 1
    node_it <- node_it + 1
  }
  age_it <- age_it + 1
}



