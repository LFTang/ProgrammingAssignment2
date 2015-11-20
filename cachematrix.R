## These functions perform inverse for matrices and cache the solved inverse matrix

## This function is composed of a lis of 4 functions
## It can store a Matrix (x) and its inverse (inv) in two variables for future use
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function performs inverse of a matrix, utilizing the caching capacity
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- ginv(data, ...)
  x$setinverse(inv)
  inv
  }
