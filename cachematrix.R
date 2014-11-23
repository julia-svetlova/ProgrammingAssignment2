##Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.
## Below are two functions that cache the inverse of a matrix.

## This function makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function cacheSolve calculates the inverse 
## of the special "matrix" created with the above function. 
## First it checks if the inverse matrix has already been calculated.
## If so, it gets the inverse matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix 
## and sets the result in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}