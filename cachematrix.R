## These two functions cache the inverse of a matrix

## This first function makes a special "matrix" that can cache 
## its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) i <<- inverse
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## This function finds the inverse of the "matrix" created
## by the first function
cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}
