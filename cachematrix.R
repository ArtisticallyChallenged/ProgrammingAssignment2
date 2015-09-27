## This first function gets the matrix and calculates 
## its inverse

makeCacheMatrix <- function(x = matrix(1:4,2,2)) {  
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




## Checks to see if inverse has been computed
## If so, returns the inverse.
## If not, calculates the inverse

cacheSolve <- function(x, ...) {
     m <- x$getsolve()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data)
     x$setsolve(m)
     m
}

