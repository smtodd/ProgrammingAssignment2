## These functions calculate the value of a nonsingular matrix and store it to memory.

## This function takes as input a matrix 'x' and returns a list that sets the value of the matrix,
## gets the value of the matrix, calculates the inverse and assigns it to variable 'i'
## and gets the inverse 'i'.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y){
          x <<- y
          i <<- NULL
     } 
     get <- function() x
     setinv <- function(solve) i <<- solve
     getinv <- function() i
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## If the inverse of matrix x has already been set, this function
## retrieves it, otherwise it calculates the inverse.

cacheSolve <- function(x, ...) {
     i <- x$getinv()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinv(i)
     i
}
