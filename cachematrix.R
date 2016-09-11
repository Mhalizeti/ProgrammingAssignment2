## These pair of functions cache the inverse of a matrix. This is especially
## important for large matrices calculations with high computation times.

## This first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## set the values of the matrix and initialize the values of the inverse matrix
  mat_inverse <- NULL
  
  set <- function(y) {
    x <<- y
    mat_inverse <<- NULL
  }
  
  ## get the values of the matrix
  get <- function() x
  
  ## set the inverse matrix
  setinverse <- function(solve) mat_inverse <<- solve
  
  ## get the inverse matrix
  getinverse <- function() mat_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This second function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return the inverse matrix 'mat_inv' from the cache if it has been
  ## previously calculated
  mat_inv <- x$getinverse()
  if(!is.null(mat_inv)) {
    message("getting inverse matrix from cache")
    return(mat_inv)
  }
  
  ## Calculate and return 'mat_inv', wchich is the inverse matrix of 'x' 
  data <- x$get()
  mat_inv <- solve(data, ...)
  x$setinverse(mat_inv)
  mat_inv
  
}