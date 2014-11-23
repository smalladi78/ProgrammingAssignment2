## The two functions (makeCacheMatrix and cacheSolve) together allow you to cache the
## matrix and its inverse (the inverse is calculated lazily)

## The makeCacheMatrix function has two matrices (x and invMatrix) and provides
## setter and getter methods to get and set these properties

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- matrix()
  set <- function(y) {
    x <<- y
    invMatrix <<- matrix()
  }
  get <- function() x
  setInverse <- function(inv) invMatrix <<- inv
  getInverse <- function() invMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function accepts an instance of makeCacheMatrix and returns the inverse
## of the matrix in that instance. The inverse is computed if not already present and cached
## for future access.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.na(inv[1,1])) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
