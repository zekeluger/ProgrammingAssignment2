## Put comments here that give an overall description of what your
## functions do

# These functions allow a user to take the inverse
# of a matrix and cache it. This way the user doesn't
# have to recalculate the inverse if they've already
# calculated it once.


## Write a short comment describing this function

# makeCacheMatrix() creates an object, a vector of 4 functions.
# These functions allow the user to get and set
# both the original matrix and its inverse.
# This object has the ability to cache
# a matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {

     xinv <- NULL
     
     # The set function assigns a value to 
     # x and m in the makeCacheMatrix environment
     set <- function(y) {
          x <<- y
          xinv <<- NULL
     }
     
     get <- function() x
     
     setxinv <- function(inv) xinv <<- inv
     
     getxinv <- function() xinv
     
     # The function makeVector() returns an object of type makeVector()
     # which is the following list
     list(set = set, get = get,
          setxinv = setxinv,
          getxinv = getxinv)     
     # cachemean() requires an object of the type makeVector() as an argument
}



## Write a short comment describing this function

# cacheSolve() returns the inverse of the matrix
# from the makeCacheMatrix() object.
# It first checks if the inverse is already calculated.
# If the invers has not been calculated, it
# calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
     
     # if xinverse has already been calculated
     # return xinverse
     # else compute inverse of x:
     xinv <- x$getxinv()
     if(!is.null(xinv)) {
          message("getting cached data")
          return(xinv)
     }
     
     # return a matrix that is the inverse of 'x'
     data <- x$get()
     xinv <- solve(data, ...)
     x$setxinv(xinv)
     
     xinv
}






