
## Caching the Inverse of an Invertible Square Matrix
## Getting the results if available in cache otherwise calculet and set results in cache

## This function sets the value of the matrix; gets the value of the matrix;
## sets the value of the inverse matrix & gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## This function searches the cache to find if the inverse has been already calculated and
## if found gets the value and ships computation
## otherwise calculates the inverse and sets in cache using setsolve function

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
