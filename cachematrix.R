## Matrix inversion is usually a costly computation 
## There may be some benefit to caching the inverse of a matrix 

## cache the matrix and it's inverse
## creates a special "matrix", object which has a list containing
## get,set for the matrix and getinv,setinv for it's inverse
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  ## solve returns the inverse of the matrix
  inv <- solve(data)
  x$setinv(inv)
  inv
}
