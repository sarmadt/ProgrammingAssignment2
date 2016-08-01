## Two functions are provided that create a cached matrix, which
## caches its inverse when it is first computed. This is to avoid
## the costly computation of inverse if the matrix contents have
## not changed

## ## This function returns a list of functions that can be used to 
## 1) get the matrix 2) set the matrix 3) get cached inverse
## 4) set or cache inverse for later use

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inver) inv <<- inver
  getinv <- function() inv
  list(set = set, get = get,
        setinv = setinv, 
        getinv = getinv)
}


## This function returns the cached inverse if available
## otherwise it computes and returns the inverse and 
## caches it for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

