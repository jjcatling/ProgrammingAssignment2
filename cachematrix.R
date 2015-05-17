## Creates a special 'matrix' object that can cache its inverse.
## Does so by creating a special 'vector', which is really a
## list containing a function to:
## - set the matrix
## - get the matrix
## - set the inverse
## - get the inverse

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     setmatrix <- function(y) {
         x <<- y
         i <<- NULL
     }
     getmatrix <- function() x
     setinverse <- function(solve) i <<- solve
     getinverse <- function() i
     list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse, getinverse = getinverse)
  
}


## Calculates the inverse of the 'matrix' created with the above.
## First, however, it checks if the inverse has already been calculated.
## If so, it pulls the inverse from the cache instead of calculating.
## Otherwise, it makes the calculation...
## ...and sets the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i)) {
         message("Getting cached inverse...")
         return(i)
     }
     data <- x$getmatrix()
     i <- solve(data, ...)
     x$setinverse(i)
     i        
}
