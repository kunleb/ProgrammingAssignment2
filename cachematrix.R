## The following pair of functions are used to cache and calculate   
## the inverse of an invertible matrix in order to reduce the computation  
## time of this potentially time-consuming operation.



## This function creates a special "matrix" object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function checks and retrieves the inverse from the above special "matrix"   
## object if it has been calculated and if not, calculates its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv 
  
}
