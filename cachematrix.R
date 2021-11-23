## This pair of functions allows you to calculate the inverse of a matrix, and store it in cache. 
## If its calculation is required again, it reuses the stored value with the consequent savings in computational consumption.

## The first function makeCacheMatrix creates a special "matrix", which is really a list containing four functions: "set the value of the matrix", "get the value of the matrix", 
## "set the value of the inverse of matrix" and "get the value of the inverse of matrix"

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The second function calculates the inverse of the special "matrix" created with the first function. However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data (matrix) and sets the value of the inverse in the cache 
## via the setinv function.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
