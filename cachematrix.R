## Caching the Inverse of a Matrix:
## 
## I am very new to 'R' programming, so please give all suggestions.  I can't get better without your help.

## Below is the function to create a matrix.

makeCacheMatrix <- function(x = matrix()) {
     inversion <- NULL
     set <- function(y) {
          x <<- y
          inversion <<- NULL
     }
     
     get <- function() x
     setInverse <- function(inverse) inversion <<- inverse
     getInverse <- function() inversion
     list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## This function creates the inversion of the original matrix.

cacheSolve <- function(x, ...) {
     inversion <- x$getInverse()
     if (!is.null(inv)) {
          message("Using Cached Data")
          return(inv)
     }
     mat <- x$get()
     inversion <- solve(mat, ...)
     x$setInverse(inversion)
     inversion
}