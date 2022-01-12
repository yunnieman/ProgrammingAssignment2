## Programming assignment 2 for Week 3
## Lexical Scoping and caching Inverses

## This function creates a matrix that can cache its inverse.
## assuming the input matrix is invertible

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # set value of matrix
  set <- function(y){
    # callback to value
    x <<- y   
    inv <<- NULL
    }
    # get values of matrix
    get <- function() {x}
    sInverse <- function(inverse) {inv <<- inverse}
    # get values of inverse
    gInverse <- function() {inv}
    list(set = set, get = get, sInverse = sInverse, gInverse = gInverse)
}


## computes the inverse of matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$gInverse()
  # check if inverse was made
  if(!is.null(inv)){
    message("retrieving the cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$sInverse(inv)
  inv #returning
}
