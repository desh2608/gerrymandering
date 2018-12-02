library(rgdal)
library(maptools)
library(ggplot2)
library(broom)
library(rgeos)

## read shapefile
ohio <- readOGR("ohio/precincts_results.shp")

## create adjacency matrix
adj_matrix <- gTouches(ohio, byid=TRUE, returnDense=FALSE)

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


