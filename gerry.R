library(rgdal)
library(maptools)
library(ggplot2)
library(broom)
library(rgeos)
library(igraph)

## Score functions for a redistricting
## This function takes as input a possible
## redistricting, and assigns a score to it
## which is the weighted sum of several
## scores.

## A redistricting E is just a list of vectors,
## where each element in the list represents
## a district, and each element in a vector
## represents a precinct.

# Total score: section 3.1
score.total <- function(E){
  
}

# Population score: section 3.1.1
score.pop <- function(E){
  
}

# Isoperimetric score: section 3.1.2
score.isoperimetric <- function(E){
  
}

# County score: section 3.1.3
score.county <- function(E){
  
}

# Minority score: section 3.1.4
score.minority <- function(E){
  
}

## Sample new redistrictings using MH algorithm
## Input: vector of precinct ids, adjacency matrix
## Output: generated redistricting samples
## section 3.3
sampleMH <- function(precincts, A){
  
  # step 1
  dist <- rep(NA, 16)
  E <- randomDist(precincts, len(dist))
  
  # step 2
  list[u, v] <- pickConflictingEdge(precincts, A)
  
}

## Pick any conflicting edge randomly in graph
## Conflicting edge means the vertices belong to 
## different districts, i.e., the edge
## crosses a district boundary.
pickConflictingEdge <- function(E, A){
  edges <- findConflictingEdges(E, A)
  n <- length(edges)
  ind <- runif()
}

## Find all conflicting edges in graph
## Inputs: E (precinct-wise allocation dict),
## A (adjacency matrix)
## Output: list of conflicting edges
findConflictingEdges <- function(E, A){
  edges <- which(A==1, arr.ind = TRUE)
  conflict <- list()
  j <- 1
  for (i in 1:length(edges)){
    if(E[edges[i,1]] != E[edges[i,2]]){
      conflict[[j]] <- edges[i]
      j <- j+1
    }
  }
}

## Create random districts from given precincts
randomDist <- function(precincts, num_dist){
  precincts.shuffled <- sample(precincts, size=length(precincts))
  rDist <- split(precincts.shuffled, sort(precincts.shuffled%%num_dist))
  return (rDist)
}

## Count number of conflicted edges in a 
## redistricting
conflicted <- function(D, adj){
  total <- 0
  for (i in 1:length(D)){
    district <- D[i]
    for (j in 1:(length(district)-1)){
      for (k in j+1:length(district)){
        if (adj[j,k]==0){
          conflicted <- conflicted + 1
        }
      }
    }
  }
}

## find connected components
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
  data.new <- as.matrix(data.df[complete.cases(data.df),])
  
  return(data.new)
}

## Functions to get redistricting
getRedistrictingByDistrict <- function(data){
  districts <- unique(data[,2])
  n <- length(districts)
  p <- list()
  for (i in 1:length(districts)){
    p[[i]] <- subset(data[,1], data[,2]==districts[i])
  }
  return (p)
}

getRedistrictingByPrecinct <- function(data){
  return (list(data[,2]))
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

## initial redistricting
E <- getRedistrictingByPrecinct(data)
D <- getRedistrictingByDistrict(data)