## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list of functions to set and get the matrix, 
## and set and get the invere of a given matrix. 

makeCacheMatrix <- function(x = matrix()) {
  # sets the local copy to NULL
  matInverse <- NULL
  # sets the global (cached) squar matrix to mat and matrix invers to NUll
  set <- function(mat) {
    x <<- mat
    matInverse <<- NULL
  }
  # returns the squar marix
  get <- function() x
  
  # sets the global copy of matInverse by the passed value MatInv
  setinv <- function(MatInv) matInverse <<- MatInv
  
  # returns the cached inverse (if there is any)
  getinv <- function() matInverse
  
  # creates a list of setters and getters to spec. matrix and its invers
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function checks if there is a chached matrix inverse to return, 
## otherwise it calculates the invers 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', if there is any. Otherwise NULL
  matInverse <- x$getinv()
  if(!is.null(matInverse)) { 
    message("Retreiving the cahced inverse")
    return(matInverse)
  }
  # gets the matrix to calc it inverse
  mat <- x$get()
  matInverse <- solve(mat, ...)
  # sets the global copy to the claculated matrix inverse 
  x$setinv(matInverse)
  # prints the matrix inverse.
  matInverse
}

