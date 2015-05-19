## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly (there 
## are also alternatives to matrix inversion that we will not discuss here). Your 
## assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## creating a variable for the matrix
  m <- NULL
  
  ##Set function  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##Get function
  get <- function() x
  
  ##Set matrix function
  setmatrix <- function(solve) m <<- solve
  
  ##Get matrix function
  getmatrix <- function() m
  
  ##Return a list
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {   
  
  ## get the matrix from cache
  m <- x$getmatrix()
  
  ## when matrix is present in cache, show message and return from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## when no cache, get and inverse the matrix
  data <- x$get()
  m <- solve(matrix(), ...)
  x$setmatrix(m)
  m
}
