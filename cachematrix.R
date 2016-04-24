## makeCache matrix contains 4 functions 
## get returns the matrix x stored in the main function
## set changes the x vector stored in the main function to y
## setinverse stores the inverse in the main function
## getinverse returns the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y = matrix()) {
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


## for any invertible matrix cacheSolve will:
## [for a matrix whose inverse has already been calculated] return the message 
## "getting cached data" and the inverse OR
##[for a matrix whose inverse has not already been calculated] calculate the 
## inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)
  ## Return a matrix that is the inverse of 'x'
}


