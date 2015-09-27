## Given an initial invertible matrix, cache the matrix, calculate its 
## inverse, and the cache its inverse.
## Run the function again.  If it is the same cached matrix,
## return its cached inverse.
## If not, calculate the inverse of new matrix

## This first function gets the first matrix and calculates 
## its inverse

makeCacheMatrix <- function(x = matrix()) {  
     m <- NULL
     n <- NULL
     setmatrix <-function(n) n <<- x
     getmatrix <- function() n
     set <- function(y) {
     x <<- y
     m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     
     list(setmatrix = setmatrix,
          getmatrix = getmatrix,
          set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}




## Checks to see if new matrix is the same as cache matrix
## If so, returns the inverse.
## If not, calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getsolve()
        ## If the new matrix is the same as the cache matrix
        ## Returns the cache inverse     
     if(x == n) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setsolve(m)
     m
}
