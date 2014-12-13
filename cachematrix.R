## Put comments here that give an overall description of what your
## functions do

## Caches inverse matrix for given one

makeCacheMatrix <- function(x ) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(matr) m <<- matr
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Solves inverse of matrix or takes cached version

cacheSolve <- function(x,...){
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
