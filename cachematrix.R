## Create list of functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## Creates function that checks for cache or solves and returns inverse

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  ## if cached, return matrix from cache
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## else, calculate and return a matrix that is the inverse of 'x'
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
