## This program consists out of two functions which use lexical scoping to
## inverse a matrix using the R-function solve(). Because such operation can be
## memory consuming, the matrix is first stored in cache. The program then
## evaluates whether a result is already available in cache before calculating.

## The first function, makeCacheMatrix creates a special matrix, 
## which is really a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## Set the value of the inversed matrix
## get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL 
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## This function calculates the inverse of the special "matrix" created with 
## the above function. However, it first checks to see if the inversed matrix has 
## already been calculated. If so, it gets the inversed matrix from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the matrix
## and stores it in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        matrix <- x$get()
        s <- solve(matrix, ...)
        x$setsolve(s)
        s
}

