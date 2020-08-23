## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set the matrix, clear the cache
  set <- function(y) {
    x <<- y
    ## reset the cache
    inv <<- NULL
  }
  
  # Get the matrix
  get <- function() x
  
  # Update the cached matrix value or inverse
  setInverse <- function(inverse) {
    print( "setting cached data")
    inv <<- inverse
  }
  
  # Get the cached matrix value
  getInverse <- function() inv
  
  # List of getters and setters
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}	


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- inverse(data, ...)
  x$setInverse(i)
  i
}
