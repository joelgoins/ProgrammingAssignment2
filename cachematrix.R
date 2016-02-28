## Caching the Inverse of a Matrix:
## 
## I am very new to 'R' programming, so please give all suggestions.  I can't get better without your help.

## Below is the function to create a matrix.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     
     get <- function() x
     setInverse <- function(inverse) inv <<- inverse
     getInverse <- function() inv
     list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## This function creates the inversion of 'matrix1'.

cacheSolve <- function(x, ...) {
     inv <- x$getInverse()
     if (!is.null(inv)) {
          message("Using Cached Data")
          return(inv)
     }
     mat <- x$get()
     inv <- solve(mat, ...)
     x$setInverse(inv)
     inv
}