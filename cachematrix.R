## Description of Assignment 2 for R Programming class from:
## https://class.coursera.org/rprog-016


## Matrix inversion is usually a costly computation and their may
## be some benefit to caching the inverse of a matrix rather than
## compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## This version created 2014-12 by https://github.com/cubeconvict
## significant help from
## https://class.coursera.org/rprog-016/forum/thread?thread_id=96


# makeCacheMatrix: This function creates a special "matrix" object
# that can cache its inverse.

makeCacheMatrix <- function(x=matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() {x}
     setinverse <- function(solve) {m <<- solve}
     getinverse <- function() {m}
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special
##"matrix" returned by makeCacheMatrix above. If the inverse has
##already been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.


cacheSolve <- function(x, ...) {

     ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     # test if the inverse has already been cached
     # if there is a cached version, return it
     if (!is.null(m)) {

          return(m)
     }
     # If there is no cached version, calculate it
     else {
          data <- x$get()
          m <- solve(data, ...)
          x$setinverse(m)
          m
     }

}

## Unittest function is a simple way to run a test
## with a simple canned matrix, saves typing while debugging

unittest <- function() {
     littleMatrix <- matrix(c(-1, -2, 1, 1), 2, 2)
     littleMatrixList <- makeCacheMatrix(littleMatrix)
     print(littleMatrixList$getinverse())
     cacheSolve(littleMatrixList)
     print(littleMatrixList$get())
     cacheSolve(littleMatrixList)
     print(littleMatrixList$get())
     print(littleMatrixList$getinverse())


     # now we have an object 'bigMatrixList' of type list
#      biggerMatrix <- matrix(1:100000, nrow=100, ncol=100)
#      biggerMatrixList <- makeCacheMatrix(biggerMatrix)
#      cacheSolve(biggerMatrixList)
#      cacheSolve(biggerMatrixList)


}
