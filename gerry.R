## POLITICAL GERRYMANDERING IN OHIO

## Authors: Desh Raj, Tara Abrishami

# Install function for packages    
packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(rgdal)
packages(maptools)
packages(ggplot2)
packages(broom)
packages(rgeos)
packages(igraph)
packages(spatialEco)
packages(rlist)

##--- WEIGHTS (Hyperparameters)------------


##-------- SCORE FUNCTIONS ----------------

## Score functions for a redistricting
## This function takes as input a possible
## redistricting, and assigns a score to it
## which is the weighted sum of several
## scores.

## Two redistricting formats are available:
## D => list of vectors, where each vector 
## represents a district, and each element in a 
## vector represents a precinct
## E => dictionary of precinct-to-district
## allocation

# Total score: section 3.1
score.total <- function(state, D, iso.score, w=c(1,1,1)){
  return (w[1]*score.pop(D) + w[2]*iso.score + w[3]*score.county(state,D)) 
}

#function to calculate precinct populations
#voter turnout in Ohio: 64.2% (according to http://www.electproject.org/2016g)
#population of Ohio: 11.66 million (2017) 
# Population score: section 3.1.1
parse.num <- function(x) {
  if (is.na(x)) {
    return(0)
  } else {
    return(x)
  }
}

score.pop <- function(D){
  ohio.population <- 11660000
  district.pop <- rep(0, length(D))
  for (i in 1:length(D)) {
    district <- D[[i]]
    for (j in 1:length(district)) {
      district.pop[i] = district.pop[i] + parse.num(ohio$PRES_DEM16[j]) + parse.num(ohio$PRES_REP16[j]) + parse.num(ohio$PRES_GRN16[j])
    }
  }

  pop.ideal <- ohio.population/length(D)
  pop.score <-0
  for (i in 1:length(D)) {
    pop.score = pop.score + ((district.pop[i]/0.642)/pop.ideal - 1)^2
  }
  return(sqrt(pop.score))
}

# Isoperimetric score: section 3.1.2
score.isoperimetric <- function(district){
  district.map <- unionSpatialPolygons(ohio[district,], rep(1,length(district)))
  perimeter <- polyPerimeter(district.map)
  return(perimeter^2/district.map@polygons[[1]]@Polygons[[1]]@area)
}

# County score: section 3.1.3
score.county <- function(state, D, w = c(1,1)){
  all_counties = vector()
  for (i in 1:length(D)) {
    district = D[[i]]
    district.counties = rep(NA, length(district))
    for (j in 1:length(district)) {
      district.counties[j] = state$CNTY_GE[district[j]]
    }
    district.counties = unique(district.counties)
    all_counties = c(all_counties, district.counties)
  }
  frequencies = as.vector(table(all_counties))
  split.two = sum(frequencies > 2)
  split.three = sum(frequencies > 3)
  return (w[1]*split.two + w[2]*split.three)
}

# Minority score: section 3.1.4
score.minority <- function(E){
  #is not implemented because we do not have the data
}

equals <- function(x, y) {
  if (sum(x == y) == 2) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#----------- MH SAMPLING ------------------------

## Sample new redistrictings using MH algorithm
## Input: A (adjacency matrix), D (redistricting
## by district), E (redistricting by precinct)
## Output: generated redistricting samples
## section 3.3
sampleMH <- function(A, D, E, beta=0.4, T=1000, weights = c(200,0.005,1)){
  
  isoperimetric.scores <- rep(0, length(D)) 
  for (i in 1:length(D)) {
    isoperimetric.scores[i] = score.isoperimetric(D[[i]])
  }
  
  conflicted.edges <- findConflictingEdges(E, A)

  accept <- 0
  samples <- matrix(, nrow = T, ncol = length(E))

  # step 1
  dist <- rep(NA, length(D))
  samples[1,] <- E 

  for (i in 2:T){
    
    # step 2
    
    edge <- pickConflictingEdge(conflicted.edges)
    
    u <- edge[1]; v <- edge[2]
    E.new <- E
    
    x <- u
    if (runif(1,0,1) < 0.5){
      E.new[u] <- E[v]
    } else {
      E.new[v] <- E[u]
      x <- v
    }
    
    D.new <- getDistrictsFromPrecincts(E.new)
    
    iso.score.new <- isoperimetric.scores
    conflicted.edges.new <- conflicted.edges
  
    old_dist <- E[x]
    new_dist <- E.new[x]
    iso.score.new[old_dist] <- score.isoperimetric(D.new[[old_dist]])
    iso.score.new[new_dist] <- score.isoperimetric(D.new[[new_dist]])
    
    edges <- which(A[x,]==1, arr.ind = TRUE)
    for (e in 1:length(edges)) {
      if (E[e] != E[x]) {
        for (j in 1:length(conflicted.edges.new)) {
          if (sum(conflicted.edges.new[[j]] == c(e, x)) == 2 || sum(conflicted.edges.new[[j]] == c(x, e)) == 2) {
            conflicted.edges.new = removeEdge(conflicted.edges.new, c(e, x), c(x, e))
          } 
        }
      }
      if (E.new[e] != E.new[x]) {
        list.append(conflicted.edges.new, c(e, x))
        list.append(conflicted.edges.new, c(x, e))
      } 
    }
    
    # step 3
    
    new_total_score = score.total(ohio, D.new, sum(iso.score.new), weights)
    old_total_score = score.total(ohio, D, sum(isoperimetric.scores), weights)
    
    accept.prob <- (length(conflicted.edges)/length(conflicted.edges.new)) *
    exp(-beta * (score.total(ohio, D.new, sum(iso.score.new)) - 
                   score.total(ohio, D, sum(isoperimetric.scores))))
      accept.prob = min(accept.prob,1)
    
    print (accept.prob)
    unif <- runif(1,0,1)
    print (unif)
    
    if (accept.prob > unif){
      D <- D.new
      E <- E.new
      accept <- accept + 1
      samples[i,] <- E.new
      conflicted.edges <- conflicted.edges.new #update conflicted edges
      isoperimetric.scores <- iso.score.new #update isoperimetric scores
      print("accepted")
    } else {
      samples[i,] <- E
      print("rejected")
    }
  }
  
  return (list(samples, accept))
  
}


#-------- HELPER FUNCTIONS FOR SAMPLING -----------

removeEdge <- function(my.list, my.tuple1, my.tuple2) {
  new.list <- list()
  j <- 1
  for (i in 1:length(my.list)) {
    if (sum(my.list[[i]] == my.tuple1) != 2 && sum(my.list[[i]] == my.tuple2) !=2) {
      new.list[[j]] <- my.list[[i]]
      j = j + 1
    }
  }
  return(new.list)
}

## Pick any conflicting edge randomly in graph
## Conflicting edge means the vertices belong to 
## different districts, i.e., the edge
## crosses a district boundary.
pickConflictingEdge <- function(edges) {
  n <- length(edges)
  ind <- ceiling(runif(1, 0, n))
  return (edges[[ind]])
}

## Find all conflicting edges in graph
## Inputs: E (precinct-wise allocation dict),
## A (adjacency matrix)
## Output: list of conflicting edges
findConflictingEdges <- function(E, A){
  edges <- which(A==1, arr.ind = TRUE)
  conflict <- list()
  j <- 1
  for (i in 1:dim(edges)[1]){
    if(!is.na(E[edges[i,1]]) && !is.na(E[edges[i,2]]) && 
        (E[edges[i,1]] != E[edges[i,2]])){
      conflict[[j]] <- edges[i,]
      j <- j+1
    }
  }
  return (conflict)
}

## Create random districts from given precincts
randomDist <- function(precincts, num_dist){
  precincts.shuffled <- sample(precincts, size=length(precincts))
  rDist <- split(precincts.shuffled, sort(precincts.shuffled%%num_dist))
  return (rDist)
}


#------- DATA PROCESSING FUNCTIONS -------------

## Get full adjacency matrix from dense lists
getAdjMatrix <- function(adj){
  n <- length(adj)
  A <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n){
    for (j in 1:length(adj[[i]])){
      A[i,adj[[i]][j]] = 1
    }
  }
  return (A)
}

## Find all connected components in the graph
## Note: This function is not being used in the
## code at present, but available in case of
## future requirement.
findConnectedComponents <- function(A){
  n <- dim(A)[1]
  C <- matrix(0, nrow = n, ncol = n)
  g  <- graph.adjacency(A)
  groups <- groups(components(g))
  for (i in 1:length(groups)){
    group <- groups[[i]]
    for (j in 1:length(group)){
      for (k in 1:length(group)){
        C[group[j],group[k]] = 1
      }
    }
  }
  return (C)
}

## Function to preprocess data fields
preprocess <- function(state){
  data <- cbind(c(as.numeric(state@data[["id"]])+1),
                c(state@data[["PREC_EL"]]),
                c(state@data[["MET_ARE"]]),
                c(state@data[["PRES_DEM16"]]),
                c(state@data[["PRES_IN"]]),
                c(state@data[["PRES_GRN16"]]),
                c(state@data[["PRES_REP16"]]))
  
  # remove precincts with <NA> districts
  data.df <- data.frame(data)
  data.new <- data.df[complete.cases(data.df),]
  
  return(data.new)
}

## Functions to get redistricting
getRedistrictingByDistrict <- function(data){
  districts <- unique(data[,2])
  n <- length(districts)
  D <- list()
  for (i in 1:length(districts)){
    D[[i]] <- subset(data[,1], data[,2]==districts[i])
  }
  return (D)
}

getRedistrictingByPrecinct <- function(data, n){
  p <- rep(NA, n)
  for (i in 1:n){
    p[i] <- data[toString(i),2]
  }
  return (p)
}

## Function to get District wise list, given a
## dictionary of precinct allocations
getDistrictsFromPrecincts <- function(E){
  n <- ifelse( !all(is.na(E)), max(E, na.rm=T), NA)
  D <- vector("list", n)
  for (i in 1:length(E)){
    if(!is.na(E[i])){
      D[[E[i]]] <- c(D[[E[i]]], i)
    }
  }
  return (D)
}

getInitialDistrict <-function(state, county_file) {
  ## code to get initial districting
  district_by_county <- readLines(county_file)
  county_membership <- state$CNTY_NA
  precinct_to_district <- rep(0, length(county_membership))
  for (i in 1:length(county_membership)) {
    for (j in 1:16) {
      if (!is.na(county_membership[i])) {
        if (grepl(county_membership[i], district_by_county[j])) {
          precinct_to_district[i] = j
        }
      } else {
        precinct_to_district[i] = NA
      }
    }
  }
  return(precinct_to_district)
}

assignNAprecincts <- function(E, A){
  while(anyNA(E)){
    s = sum(is.na(E))
    NAs = which(is.na(E))
    for (i in 1:length(NAs)) {
      precinct = NAs[i]
      adjacent_precincts = which(A[precinct,] == 1)
      adj.precinct = which(!is.na(E[adjacent_precincts]))[1]
      if (!is.na(adj.precinct)) {
        E[precinct] = E[adj.precinct]
      }
    }
    if (sum(is.na(E)) == s) {
      E[which(is.na(E))] = 0
      break
    }
  }
  return(E)
}

assignNINEprecincts <- function(E, A, D){
  nine_graph <- graph_from_adjacency_matrix(A[D[[9]], D[[9]]])
  comps <- components(nine_graph)
  comp_sizes <- sort(comps$csize, decreasing = TRUE)
  true.nine.comps <- which(comps$csize %in% c(comp_sizes[1], comp_sizes[2], comp_sizes[3]))
  bad.nines <- D[[9]][which(!comps$membership %in% true.nine.comps)]
  print(length(bad.nines))
  print(sum(is.na(E)))
  E[bad.nines] = NA
  print(sum(is.na(E)))
  return(assignNAprecincts(E, A))
}

#------- DATA ANALYSIS FUNCTIONS -------------

#plots the districts in sixteen unique colors
plotDistrict <- function(state, E) {
  map = unionSpatialPolygons(state, E)
  plot(map, col = c("black", "turquoise", "red", "orange", "yellow", "blue", "coral2", 
                    "cornflowerblue", "darkmagenta","darkseagreen2", "deeppink", 
                    "forestgreen", "chocolate4", "burlywood4", "azure1", "cornsilk3", "darkorchid1"))
}

#returns a tuple (dems, reps) with the number of Democrat representatives and the number
#of Republican representatives
getElectionResults <- function(state, D) {
  reps <- 0
  dems <- 0
  for (i in 1:length(D)) {
    dem_votes <- 0
    rep_votes <- 0
    for (j in 1:length(D[[i]])) {
      dem_votes = dem_votes + parse.num(state$PRES_DEM16[D[[i]][j]])
      rep_votes = rep_votes + parse.num(state$PRES_REP16[D[[i]][j]])
    }
    if (dem_votes > rep_votes) {
      dems = dems + 1
    } else {
      reps = reps + 1
    }
  }
  return(c(dems, reps))
}

#returns a vector of 
sampleElectionResults <- function(sample) {
  num.dems <- rep(0, length(sample))
  for (i in 1:length(sample)) {
    num.dems[i] = getElectionResults(sample[i,])
  }
  return(num.dems)
}

#returns an nxn matrix A, where A[i, j] is the probability that precinct i and precinct j are in the 
#same district
#takes a matrix of MCMC results as input
getPrecinctAdjProbabilities <- function(sample) {
  num.precincts = length(sample[,1])
  P <- matrix(0, num.precincts, num.precincts)
  for (i in 1:num.precincts) {
    for (j in 1:num.precincts) {
      P[i, j] <- sum(sample[,i] == sample[,j])
    }
  }
  return(P)
}

#returns the sum of the probabilities that each edge in a district map E is present, 
#given the geographical adjancency matrix A and the precinct adjacency probability matrix P
getMapLikelihood <- function(P, E, A) {
  conflicted.edges = findConflictingEdges(E, A)
  score = 0
  for (i in 1:length(conflicted.edges)) {
    edge = conflicted.edges[[i]]
    u <- edge[1]
    v <- edge[2]
    score = score + log(1 - P[u, v])
  }
  return(score)
}

##-----------DRIVER CODE--------------------------

set.seed(1)

## read shapefile
ohio <- readOGR("ohio/precincts_results.shp")

## create adjacency matrix
adj_matrix <- gTouches(ohio, byid=TRUE, returnDense=FALSE)

## find connected components
A <- getAdjMatrix(adj_matrix)

## random redistricting
#E <- getRedistrictingByPrecinct(data, length(ohio))
#D <- getRedistrictingByDistrict(data)

## initial redistricting from county data (with correct number of districts)
D <- getDistrictsFromPrecincts(E)

## plot the initial district mapping because it's beautiful
# plotDistrict(ohio, E)

sample <- sampleMH(A, D, E)
