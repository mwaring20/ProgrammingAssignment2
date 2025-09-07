## These functions calculate the inverse of a matrix and store that value in cache to save time 

## Create a matrix object to store both a matrix and a cached inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  
  get <- function(){
    x
  }
  
  setInverse <- function(inverse) {
    inv <<- inverse 
  }
  
  getInverse <- function() {
    inv
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse of the matrix from makeCacheMatrix
## If it has already been computed, retrieve the inverse from cache 

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("Retrieving cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
