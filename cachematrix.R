## Put comments here that give an overall description of what your
## functions do

## Function to create a 'special' matrix, whose inverse matrix can be cached

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      setMatrix <- function(y) {
            x <<- y
            i <<- NULL
      }
      getMatrix <- function() x
      setInverse <- function(solve) i <<- solve
      getInverse <- function() i
      list(setMatrix = setMatrix, getMatrix = getMatrix,
           setInverse = setInverse,
           getInverse = getInverse)
}


## 'Interface' function to be actually called by user to get the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getInverse()     # check if the Inverse has been cached
      if(!is.null(i)) {       # if cached, return the Inverse
            message("getting cached data")
            return(i)
      }
      data <- x$getMatrix()   # if not yet cached, calculate and cache it
      i <- solve(data, ...)
      x$setInverse(i)
      i
}
