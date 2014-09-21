## Functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() {
    m
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of "makeCacheMatrix". 
## If inverse has already been calculated 
## and matrix is unchanged, 
## "cacheSolve" retrieves inverse from cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("Retrieving cached inverse")
    return(m)
  }
  
  ## Get inverse matrix from calculating using matrix multiplication
  matrixData <- x$get()
  m <- solve(matrixData) %*% matrixData
  x$setInverse(m)
  
  ## Return inverse matrix
  m
}