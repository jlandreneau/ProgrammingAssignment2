## This file contains functions useful for calculating the
## inverse of a matrix and cacheing its result

## creates a special vector which is a list which 
## points to functions that will be used to get,set the
## value of the matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## calculates the inverse of matrix x
## first looks in cache to see if it was already solved,
## otherwise calculates it and sets it in the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
