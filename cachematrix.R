##########################################################################
## This file has 2 functions makeCacheMatrix and cacheSolve
## A typical is to cache the inverse of a matrix, 
## so it doesn't have to be computed again.

## It is useful when dealing with large matrices when computing
## the inverse can be computationally expensive

## The normal use-case would be to call makeCacheMatrix, and then 
## call cacheSolve on the object that makeCacheMatrix returned.
###########################################################################



# The first function, makeCacheMatrix is a list of 4 functions. 
# In object-oriented terms, one could use it as a class with 4 "methods" 
# The get() and set(x) functions do the obvious:
# get the value of matrix, or set it to the input
#
# The set function does one additional key step, It resets the inverse
# to NULL because the cache is invalid when x has a new value
# 
# getinverse returns the CACHED inverse of the matrix
# setinverse sets a value to the matrix inverse, that's then cached
#
# Typically, a user will use makeCacheMatrix in conjuction with
# cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve takes an invertible makeCacheMatrix as an input
# and computes the inverse of the underlying matrix inside
# makeCacheMatrix
# It does not recompute the inverse if the inverse has a non-null
# value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
