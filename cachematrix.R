## Given an initial invertible matrix, cache the matrix, calculate its 
## inverse, and the cache its inverse.
## Run the function again.  If it is the same cached matrix,
## return its cached inverse.
## If not, calculate the inverse of new matrix

## This first function gets the first matrix and calculates 
## its inverse

makeCacheMatrix <- function(x = matrix()) {  
     m <- NULL
     set <- function(y) {
     x <<- y
     m <<- NULL
     }
     get <- function() x
     setsolve <- function() m <<- solve(x)
     getsolve <- function() m
     
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}




## Checks to see if new matrix is the same as cache matrix
## If so, returns the inverse.
## If not, calculates the inverse

cacheSolve <- function(x,y=matrix()) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getsolve()
        ## If the new matrix is the same as the cache matrix
        ## Returns the cache inverse     
     if(y == x$get()) {
          message("getting cached data")
          return(m)
     }
     m <- solve(y)
     m
}
