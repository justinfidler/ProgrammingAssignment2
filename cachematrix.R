## These functions calculate calculate and then cache the 
## inverse of a matrix.  Subsequent calls to cacheSolve with 
## an identical matrix will be much faster than the initial
## function call.

## This function creates a special "matrix"
## The "matrix" is really a list containing 4 functions
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse(solve) of the matrix
## get the value of the inverse(solve) of the matrix 

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


#This function calculates the inverse of the special "matrix"
#created by makeCacheMatrix.  First, it checks to see if the
#inverse has already been calculated by a call to getinverse
#If getinverse returns a non-null value, cacheSolve returns
#the cached value of the inverse of the matrix

#If getinverse returns NULL, then the inverse must be 
#calculated by a call to solve 
#and stored by a call to setinverse 
#and finally returned 
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
