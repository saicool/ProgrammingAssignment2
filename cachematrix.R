## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() : creates a special "matrix" object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL
  # Setting matrix
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  # Getting matrix
  get  <- function() x
  # Setting cache inverse matrix
  setInverse  <- function(inverse) i  <<- inverse
  # Getting cache inverse matrix
  getInverse  <- function() i 
  # Returning list of functions
  list(set= set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve() : computes the inverse of a matrix returned by makeCacheMatrix() 
## checks if inverse of the matrix has already been computed, and retrieves the cached inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Getting inverse of the matrix
  i  <- x$getInverse()
  # Checking if the inverse has already been computed
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  ## If the inverse has not already been computed
  # Getting the matrix
  data  <- x$get()
  # Computing the inverse
  i  <- solve(data, ...)
  # Caching the inverse
  x$setInverse(i)
  # Returning the inverse
  i
}
