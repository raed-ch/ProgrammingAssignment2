## These two fonctions are used to store inverse of matrix
## in a special "vector" so that we can call it again
## without recomputing again in order to save time.

## This function creates a special "vector" object
## that can cache the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
     invers <- NULL
     
     set <- function(y){
          x <<- y
          invers <<- NULL
     }
     
     get <- function() x
     
     set_invers <- function(inv) invers <<- inv
     get_invers <- function() invers
     list(set = set, get = get, set_invers = set_invers, get_invers = get_invers)
}


## this function calculates the inverse of a matrix
## if it doesn't exist else it brings it from cache

cacheSolve <- function(x, ...) {
     invers <- x$get_invers()
     if(!is.null(invers)){
          message("getting cached data")
          return(invers)
     }
     data <- x$get()
     invers <- solve(data, ...)
     x$set_invers(invers)
     invers
}
