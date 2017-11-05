## These functions use the ability to assign a value to an object
## in an environment other than the current to cache the result of
## costly computations like matrix inversion.

## Creates a list with functions to get/set the value of the matrix,
## and to get/set the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of the matrix created with makeCacheMatrix,
## checking first to see if the inverse has been calculated already.
## If so it gets the value from the cache and skips calculation.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
