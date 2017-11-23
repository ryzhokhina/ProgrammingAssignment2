##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invX <<- inv
  getinv <- function() invX
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##The following function calculates the inverse of the special "matrix" 
##created with the above function. 
##it first checks to see if the inverse has already been calculated. 
##If so, it gets the value from the cache. 
##Otherwise, it calculates the inverse of the matrix and sets the value in the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
