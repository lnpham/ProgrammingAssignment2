## Caching the Inverse of a Matrix

## Creating a vector

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)  i <<- inverse
  getinverse <- function()  i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Compute an inverse of the vector

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
