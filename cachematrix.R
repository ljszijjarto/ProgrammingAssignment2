##
## Author:                    ljszijjarto
## Date:                      3/20/15
##
## Coursera Specialization:   Data Science 
## Course:                    R Programming
##
## Programming Assignment 2:  Lexical Scoping 
##
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. Your assignment is to write a pair of functions that cache 
## the inverse of a matrix: makeCacheMatrix() and cacheSolve().
##

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix() 
## above. If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the cache.
## x is assumed to be invertible.
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}


## TEST cachematrix.R - At The Command Line =====================
##
##  m1<-matrix(c(1,0,1,0,1,1,1,1,1), nrow=3, ncol=3)
##  print(m1)
##  cm1<- makeCacheMatrix(m1)
##  cacheSolve(cm1)
##  cacheSolve(cm1)
##  
##  m2<-matrix(c(1,0,1,0,1,1,1,0,2), nrow=3, ncol=3)
##  print(m2)
##  cm2<- makeCacheMatrix(m2)
##  cacheSolve(cm2)
##  cacheSolve(cm2)
##
##  cacheSolve(cm1)
##  cacheSolve(cm2)
##
