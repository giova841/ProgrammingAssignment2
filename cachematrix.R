## Programming Assignment 2
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than computing it repeatedly.
# Hereinafter a pair of functions will be provided that can be used to cache the
# inverse of a matrix.


##  makeCacheMatrix
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setMatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getMatrix <- function() x
  setInverseMatrix <- function(randazzo) i <<- randazzo
  getInverseMatrix <- function() i
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## cacheSolve
# This function computes the inverse of the special "matrix" returned by 
# 'makeCacheMatrix' above. If the inverse has already been calculated (and 
# the matrix has not changed), then 'cacheSolve' should retrieve the inverse 
# from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getInverseMatrix()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$getMatrix()
  i <- solve(data, ...)
  x$setInverseMatrix(i)
  i
}
