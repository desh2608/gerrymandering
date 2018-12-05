## POLITICAL GERRYMANDERING IN OHIO

## Authors: Desh Raj, Tara Abrishami, Vasileios Papaioannou

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
score.total <- function(D, E){
  return(score.pop(D)) 
}

#function to calculate precinct populations
#voter turnout in Ohio: 64.2% (according to http://www.electproject.org/2016g)
#population of Ohio: 11.66 million (2017) 
# Population score: section 3.1.1
pop.num <- function(x) {
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
      district.pop[i] = district.pop[i] + pop.num(ohio$PRES_DEM16[j]) + pop.num(ohio$PRES_REP16[j]) + pop.num(ohio$PRES_GRN16[j])
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
score.county <- function(E){
  
}

# Minority score: section 3.1.4
score.minority <- function(E){
  #is not implemented because we do not have the data
}

#----------- MH SAMPLING ------------------------

## Sample new redistrictings using MH algorithm
## Input: A (adjacency matrix), D (redistricting
## by district), E (redistricting by precinct)
## Output: generated redistricting samples
## section 3.3
sampleMH <- function(A, D, E, beta=0.5, T=100){
  
  isoperimetric.scores <- rep(0, length(D)) 
  for (i in 1:length(D)) {
    isoperimetric.scores[i] = score.isoperimetric(D[[i]])
  }
  print("computed initial iso scores")
  
  conflicted.edges <- unique(findConflictingEdges(E, A))
  number.conflicted <- length(conflicted.edges)
  
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
    
    if (runif(1,0,1) < 0.5){
      E.new[u] <- E[v]
    } else {
      E.new[v] <- E[u]
    }
    
    D.new <- getDistrictsFromPrecincts(E.new)
    print("starting to update iso scores")
    iso.score.new <- isoperimetric.scores
    if (E[u]!=E.new[u]){
      old_dist <- E[u]
      new_dist <- E.new[u]
      iso.score.new[old_dist] <- score.isoperimetric(D.new[[old_dist]])
      iso.score.new[new_dist] <- score.isoperimetric(D.new[[new_dist]])
      
      edges <- which(A[u,]==1, arr.ind = TRUE)
      for (e in 1:length(edges)) {
        if (E[e] != E[u]) {
          conflicted.edges <- conflicted.edges[!conflicted.edges %in% c(c(e, u), c(u, e))]
        }
        if (E.new[e] != E.new[u]) {
          
        } 
      }
      
    } else{
      old_dist <- E[v]
      new_dist <- E.new[v]
      iso.score.new[old_dist] <- score.isoperimetric(D.new[[old_dist]])
      iso.score.new[new_dist] <- score.isoperimetric(D.new[[new_dist]])
    }
    print("update iso scores")
    
    # step 3
    print(iso.score.new)
    if (max(iso.score.new) < 1000) {
      accept.prob <- (conflict_old/conflicted(D.new, A)) *
      exp(-beta * (score.total(D.new, E.new) - score.total(D, E)))
      if (accept.prob > 1){
        accept.prob = 1
      }
      
      if (accept.prob > runif(1,0,1)){
        D <- D.new
        E <- E.new
        accept <- accept + 1
      }
      print("accepted")
    } else {
      samples[i,] <- E
      print("rejected")
    }
  }
  
  return (list(samples, accept))
  
}

#-------- HELPER FUNCTIONS FOR SAMPLING -----------

## Pick any conflicting edge randomly in graph
## Conflicting edge means the vertices belong to 
## different districts, i.e., the edge
## crosses a district boundary.
pickConflictingEdge <- function(edges) {
  #edges <- findConflictingEdges(E, A)
  total <- length(edges)
  n <- length(edges)
  ind <- ceiling(runif(1, 0, n))
  return (list(edges[[ind]],total))
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

## Count number of conflicted edges in a 
## redistricting
## inputs: D (redistricting by districts),
## A (adjacency matrix)
conflicted <- function(D, A){
  total <- 0
  for (i in 1:length(D)){
    district <- D[[i]]
    for (j in 1:(length(district)-1)){
      for (k in j+1:length(district)){
        if (A[j,k]==0){
          total <- total + 1
        }
      }
    }
  }
  return (total)
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
        precinct_to_district[i] = 1
      }
    }
  }
  return(precinct_to_district)
}

## Functions for data analysis

#plots the districts in sixteen unique colors
plotDistrict <- function(state, E) {
  map = unionSpatialPolygons(state, E)
  plot(map, col = c("turquoise", "red", "orange", "yellow", "blue", "coral2", "cornflowerblue", "darkmagenta","darkseagreen2", "deeppink", "forestgreen", "chocolate4", "burlywood4", "azure1", "cornsilk3", "darkorchid1"))
}

#returns a tuple (dems, reps) with the number of Democrat representatives and the number
#of Republican representatives
getElectionResults <- function(state, E) {
  
}
##----------------------------------------------------------------
## DRIVER CODE

## read shapefile
ohio <- readOGR("ohio/precincts_results.shp")

## create adjacency matrix
adj_matrix <- gTouches(ohio, byid=TRUE, returnDense=FALSE)

## find connected components
A <- getAdjMatrix(adj_matrix)
C <- findConnectedComponents(A)

## remove NA rows from data
data <- preprocess(ohio)

## random redistricting
#E <- getRedistrictingByPrecinct(data, length(ohio))
#D <- getRedistrictingByDistrict(data)

## initial redistricting from county data (with correct number of districts)
E <- getInitialDistrict(ohio, "counties_to_districts.txt")
D <- getDistrictsFromPrecincts(E)

## plot the initial district mapping because it's beautiful
# plotDistrict(ohio, E)

sample <- sampleMH(A, D, E)
