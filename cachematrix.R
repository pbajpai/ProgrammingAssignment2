## R Programming : Week 3 : Programming Assignment 2 : Lexical Scoping 
## This file contains two functions makeCacheMatrix and cacheSolve


## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse. It returns the list of following functions. 

##   setMatrix: set the value of a matrix
##   getMatrix: get the value of a matrix
##   cacheInverse: set the cached value
##   getInverse: get the cached value
##
## Inverse of the square matrix is calculated by using Solve function.
## for this assignment we are assuming that assume that the matrix supplied is 
## always invertible 



makeCacheMatrix <- function(x = matrix()) {
  # cacheValue Holdes the cached value or NULL if nothing is chased.
  cacheValue <- NULL
  
  # store a Matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    # As newValue is assigned to matrix, reset the cacheValue to NULL
    cacheValue <<- NULL
  }
  # Get the stored matrix
  getMatrix <- function() {
    x
  }
  # cache the inverse result
  cacheInverse  <- function(solve) {
    cacheValue <<- solve
  }
  # return the stored matrix
  getInverse <- function(){ 
    cacheValue
  }
  # return a list of functions. 
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       cacheInverse = cacheInverse,
       getInverse = getInverse)

}


## cacheSolve: This function computes the inverse of the special "matrix"  
## returned by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieve the 
## inverse from the cache.


cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  # get the cached Value of InverseMatrix. 
  inverseMatrix <- x$getInverse()
  
  # if inverseMatrix is cached then return it. 
  
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  # if cacheMatrix is NULL, then calculate the inverse and store in cache.
  data <- x$getMatrix()
  inverseMatrix <- solve(data)
  x$cacheInverse(inverseMatrix)
  
  # return the inverse
  inverseMatrix
}
