## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#set: Sets the matrix and resets the cached inverse.
#get: Retrieves the matrix.
#setInverse: Caches the inverse of the matrix.
#getInverse: Retrieves the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL  # This will store the cached inverse
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cached inverse when the matrix is changed
  }
  
  get <- function() x  # Return the matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
  
  getInverse <- function() inv  # Return the cached inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
# Checks if the inverse is already cached. If so, it retrieves the cached inverse.
# If the inverse is not cached, it computes the inverse, caches it, and then returns the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  # Check if the inverse is already cached
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not cached, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  
  inv
}
