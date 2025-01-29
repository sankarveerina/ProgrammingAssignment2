## The makeCacheMatrix function creates a special matrix object that can
## cache its inverse. It returns a list of functions to set and get the matrix
## and to set and get the inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the cached inverse as NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y    # Assign the matrix y to the parent environment (x)
    inv <<- NULL  # Reset the cached inverse when the matrix changes
  }
  
  # Function to get the matrix
  get <- function() {
    x
  }
  
  # Function to set the inverse of the matrix
  setinverse <- function(inverse) {
    inv <<- inverse  # Assign the inverse to the parent environment (inv)
  }
  
  # Function to get the cached inverse
  getinverse <- function() {
    inv
  }
  
  # Return a list containing all functions to interact with the special matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the matrix created by 
## makeCacheMatrix. It first checks if the inverse is already cached, and if 
## so, it retrieves it. If not, it computes the inverse and caches it for later.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # Check if the inverse is already cached
  
  # If the inverse is already cached, return it
  if(!is.null(inv)) {
    message("Getting cached data")  # Message to indicate the cache is being used
    return(inv)
  }
  
  # If the inverse is not cached, compute it
  data <- x$get()  # Get the matrix from the cache object
  inv <- solve(data, ...)  # Compute the inverse using the solve() function
  
  # Cache the computed inverse
  x$setinverse(inv)
  
  # Return the computed inverse
  inv
}
