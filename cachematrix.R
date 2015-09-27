## R Functions for the R Programming - Programming Assignment 2
##
## These functions take a matrix and create a special matrix that is really a list of functions.
## This list of 4 functions allow for the matrix to be set and got, and if the cacheSolve function
## has been called the special matrix also contains functions to get and set the matrix's inverse
##


## This function creates a 'special matrix' that for a given input matrix creates functions to get
## and set the matrix as well as get and set the inverse of the matrix (see cacheSolve)

makeCacheMatrix <- function(x = matrix()) {
  # create a veriable to store the matrix inverse
  inv <- NULL
  # set functions for both the matrix and clearing the cache
  set <- function(y) {
    # set the matrix x to y
    x <<- y
    # clear the inverse cache
    inv <<- NULL
  }
  # get the matrix 
  get <- function() x
  # set the matrix inverse 
  setinv <- function(solve) inv <<- solve
  # get the matrix inverse
  getinv <- function() inv
  ## return a list of the four functions above 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  }

## This function when called on the special matrix output of makeCacheMatrix
## checks to see if the matrix inverse is already calculated if it is not it calculates
## it and uses the set function to store it, otheriwse it returns the cached inverse

cacheSolve <- function(x, ...) {
    # call the getinv function of the special matrix    
    inv <- x$getinv()
    # check to see if it is null
    if(!is.null(inv)) {
      message("getting cached data")
      # if its not hull return the cached inverse
      return(inv)
    }
    #else call the get function to obtain the matrix, then call solve, then set the inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
  }

