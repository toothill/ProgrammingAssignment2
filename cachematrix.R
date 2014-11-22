## These functions cache the inverse of an invertible matrix 
## so that this costly operation does not need to be completed 
## multiple times.

## makeCacheMatrix Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse matrix variable m to NULL
  m <- NULL
  
  ## Set method initializes input matrix to x variable & sets inverse matrix variable to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Get method simply returns the original matrix (prior to inverse)
  get <- function() x 
  
  ## Assign the inverse matrix to variable m
  setinverse <- function(mat) m <<- mat
  
  ## Return the cached inverse matrix (stored in m)
  getinverse <- function() m
  
  ## Return a list of the above methods so they can be access (in cacheSolve)
  list(set = set, get = get,
       getinverse = getinverse,
       setinverse = setinverse)
  
}

## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix.  If the inverse has already been calculated then the
## function returns the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Trying getting the cached inverse
  m <- x$getinverse()
  
  ## Test if inverse exists, return if it does
  if(!is.null(m)) {
    message("getting cache data")
    return(m)
  }
  
  ## If inverse doesn't exist ... Get Matrix, Compute Inverse, 
  ## Cache Inverse, Return Inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}