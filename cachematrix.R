## These functions provide a way to calculate the
## inverse of a matrix and cache the results for
## quicker access.

## makeCacheMatrix is an "object" (actually a list)
## that has the following four methods:
## $set(y)        Sets the value of the stored matrix to the matrix passed as a parameter 
## $get()           Gets the value of the stored matrix
## $setInverse(inverse)   Sets the value of the inverse of the matrix to the passed parameter
## $getInverse()            Gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
      inv <- NULL  ## sets the inital inverse to NULL

      # $set(y) - stores the matrix passed to this function in the object
      set <- function(y) {
        ## checks that y is a matrix and if so, sets x = y
        if (is.matrix(y)) {
          x <<- y 
        } else {
          stop("'y' must be a matrix")
        }
        inv <<- NULL
      }
      
      # $get() - returns the stored matrix
      get <- function() x
      
      # $setInverse(inverse) - takes a matrix and stores it as the "inverse"
      setInverse <- function(inverse) inv <<- inverse
      
      # $getInverse() - returns the stored inverse
      getInverse <- function() inv
      
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve takes a makeCacheMatrix object and
## calculates its inverse, storing the result in
## the makeCacheMatrix object using $setInverse(inverse)
## Assumes that the matrix is invertible

cacheSolve <- function(x, ...) {
  
        ## check that x has all required makeCacheMatrix functions first
        if ("getInverse" %in% names(x) & "get" %in% names(x) & "setInverse" %in% names(x)) {
          
          ## all required methods are available, so proceed
          
          ## get the inverse, if any
          inv <- x$getInverse()
          
          ## if there in an inverse, return cached result with message
          if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
          }
          
          ## only here in inv is NULL, so calculate inverse
          data <- x$get()     # get the matrix
          inv <- solve(data)  # find inverse
          x$setInverse(inv)   # store inverse in object
          
          ## Return a matrix that is the inverse of 'x'
          return(inv)
          
        } else {
          
          ## error out as makeCacheMatrix was not provided
          stop("'x' requires a makeCacheMatrix object")
        }  
}
