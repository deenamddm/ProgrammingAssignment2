## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Creates the objects "set", "get", "setinverse" and "getinverse" in separate environment


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# Checks whether the inverse already available in cache memory
# If avaialable, displays the message "getting cached data" and inverse of the matrix from cache 
# If not available, calculates the inverse of the matrix and returns the inverse

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
