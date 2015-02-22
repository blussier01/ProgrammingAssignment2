## Returns a matrix object and computes its inverse, but only if
## the inverse has not already been computed and stored using <<-.

## This function returns a matrix object.

makeCacheMatrix <- function(x = matrix()) {
    X_inv <- NULL
    set <- function(A) {
      x <<- A
      X_inv <<- NULL
    }
    get <- function() x
    set_inv <- function(inverse) X_inv <<- inverse
    get_inv <- function() X_inv
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
  }


## This function returns a matrix that is the inverse of x (output
#               by makeCacheMatrix function), UNLESS the inverse of x
#               has already been produced and stored/cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  X_inv <- x$get_inv()
  if(!is.null(X_inv)) {
    message("getting cached data")
    return(X_inv)
  }
  data <- x$get()
  X_inv <- solve(data, ...)
  x$set_inv(X_inv)
  X_inv
}
