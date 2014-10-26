## These two functions create a matrix and cache it's inverse.
## 

## This function creates a list of functions that can be used to create, cache
## and retrieve a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    Inverse <- NULL
    set <- function(y) {
      Inverse <<- y
      Inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) Inverse <<- solve
    getInverse <- function() Inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
}


## cacheSolve retrieves the inverse of a matrix from the cache or solves
## the matrix to find the inverse if the cache is empty.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inverse <- x$getInverse()
  if(!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  data <- x$get()
  Inverse <- solve(data, ...)
  x$setInverse(Inverse)
  Inverse
}
