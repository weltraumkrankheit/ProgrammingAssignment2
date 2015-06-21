################################################################
##
##     makeCacheMatrix
## creates a special "matrix" object that can cache its inverse
##
##     cacheSolve
## computes the inverse of the special "matrix" 
## returned by makeCacheMatrix
## 
################################################################

## $get     gets the stored matrix
## $set     sets the matrix
## $setinv  sets the inverse
## $getinv  gets the stored inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## if M is a square invertible matrix, 
## then solve(M) returns its inverse

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
