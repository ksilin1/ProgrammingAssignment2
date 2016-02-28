##################################################################
#                                                                #
#                                                                #
#   Assignment: Programming Assignment 2: Lexical Scoping        #
#  ########################################################      #
#           Caching the Inverse of a Matrixless                  #
#                                                                #
##################################################################

## This assignment presents a pair of functions that cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix",  object that can cache its inverse.
## It contains a list containing a function to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <-  function(x = matrix()) {
  inverse <- NULL   ## the inverse is cached here
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x 
    setInv <- function(inv) inverse <<- inv
    getInv <- function() inverse
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## If the inverse has not yet been calculated, then the cachesolve calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...) ## Computing the inverse of a square matrix is done with the solve function in R
  x$setInv(inverse)
  inverse
}
