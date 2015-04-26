## makeChaceMatrix, A special function that create a Matrix to cache an Inverse Matrix
##

## This function is similar to the example and is internal to the user

makeCacheMatrix <- function(x = matrix()) {
  ## name set, function to assign the value to cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## name get, function to return the value from cache
  get <- function() x
  
  ##call procedures
  setInverse <- function(x) m <<- solve(x)
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Function that calculate the Inverse Matrix, but check firstly if Inverse is cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    ##Check if Inverse is cached and return if true
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }    
    ## If not cached, call the routine to calc
    data <- x$get()
    m <- x<-solve(data, ...)
    x$setInverse(m)
    m
}
  
