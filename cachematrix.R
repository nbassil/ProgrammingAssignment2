## Caching the Inverse of a Matrix:
## makeCacheMatrix cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(f) {
          x <<- f
          inv <<- NULL
     }
     get <- function() x
     sinv <- function(inverse) inv <<- inverse
     ginv <- function() inv
     list(set = set,
          get = get,
          sinv = sinv,
          ginv = ginv)
}




## cacheSolve computes the inverse of  makeCacheMatrix function.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$ginv()
     if (!is.null(inv)) {
          
          message("getting cached data")
          return(inv)
     }
     zz <- x$get()
     inv <- solve(zz, ...)
     x$sinv(inv)
     inv
}


