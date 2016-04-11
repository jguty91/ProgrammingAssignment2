## The main objective of the functions below is to compute and cache
## the inverse of an invertible matrix.

## The first function creates a matrix which will set the value of the matrix, 
## get its value, set the value of the inversed matrix and get the values.

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

## The next function computes the value of an inversed matrix, first checking
## if the inversed values have already been computed. If so, the code gets the
## inversed matrix from the cache (setinv function); if not, it computes it.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
## Return a matrix that is the inverse of 'x'
  
  matrix <- x$get
  m <- solve(matrix,...)
  x$setinv(m)
  m
}
