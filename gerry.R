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
## Input: vector of precinct ids
## Output: generated redistricting samples
## section 3.3
sampleMH <- function(precincts){
  
  # step 1
  dist <- rep(NA, 16)
  E <- randomDist(precincts, len(dist))
  
  # step 2
  
}

## Create random districts from given precincts
randomDist <- function(precincts, num_dist){
  precincts.shuffled <- sample(precincts, size=length(precincts))
  rDist <- split(precincts.shuffled, sort(precincts.shuffled%%num_dist))
  return (rDist)
}

## Transition kernel Q for E to E.new
## section 3.3
Q <- function(E){
  
}

## Function to preprocess data fields
preprocess <- function(state){
  data <- cbind(c(as.numeric(state@data[["id"]])),
                c(state@data[["PREC_EL"]]),
                c(state@data[["MET_ARE"]]),
                c(state@data[["PRES_DEM16"]]),
                c(state@data[["PRES_IN"]]),
                c(state@data[["PRES_GRN16"]]),
                c(state@data[["PRES_REP16"]]))
  
  # remove precincts with <NA> districts
  data.new <- subset(data, data[ , 3] != "<NA>")

  return(data.new)
}

## Count number of conflicted edges in a 
## redistricting
conflicted <- function(E, connected){
  total <- 0
  for (i in 1:length(E)){
    precincts <- E[i]
    for (j in 1:(length(precincts)-1)){
      for (k in j+1:length(precincts)){
        if (connected[]==0){
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

