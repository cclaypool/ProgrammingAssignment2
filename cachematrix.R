## These functions implement the caching of matrix inverses,
## saving repeated computations

## Returns a special 'matrix' object, a list of functions to
## set or get a matrix or its cached inverse

makeCacheMatrix <- function(X = matrix()) {
  I <- NULL
  set <- function(Y) {
    X <<- Y
    I <<- NULL
  }
  get <- function() X
  setInverse <- function(inverse) I <<- inverse
  getInverse <- function() I
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns the inverse of a special 'matrix' created by makeCacheMatrix,
## retrieving the cached value if it exists

cacheSolve <- function(X, ...) {
  I <- X$getInverse()
  if(!is.null(I)) {
    message("using cached data")
    return(I)
  } else {
    data <- X$get()
    I <- solve(data, ...)
    X$setInverse(I)
    return(I)
  }
}
