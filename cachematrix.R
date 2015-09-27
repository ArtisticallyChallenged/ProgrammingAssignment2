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
     set <- function(y) {
     x <<- y
     m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     setmatrix <-function(z) n <<-x
     list(set = set, get = get,setmatrix=setmatrix
          setsolve = setsolve,
          getsolve = getsolve)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getsolve()
     n <- x$setmatrix()
     if(!is.null(m) && !is.null(n)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setsolve(m)
     m
}
