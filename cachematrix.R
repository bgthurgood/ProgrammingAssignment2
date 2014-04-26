## These functions save computational time by caching the inverse of a matrix 
## rather than recomputing it. The first creates a list of functions that 
## the second uses to calculate or cache the mean.

## makeCacheMatrix() is a list of 4 functions that cacheSolve can call upon.
## The "<<-" operator is used to assign to variables that exist outside of
## the formula's environment. 

makeCacheMatrix <- function(x = matrix()) {
      mtx <- NULL
      set <- function(y) {
            x <<- y
            mtx <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) mtx <<- inverse
      getinverse <- function() mtx
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}



## This function inverts a matrix more efficiently by returning it from cache
## rather than recalculating it each time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      mtx <- x$getinverse()
      if(!is.null(mtx)) {
            message("getting cached data")
            return(mtx)
      }
      data <- x$get()
      mtx <- solve(data)
      x$setinverse(mtx)
      mtx
}
