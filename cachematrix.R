## As matrix inversion is usually a costly computation, there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This functions cache the inverse of a matrix to get this benefit.

## Creates a special "matrix" object that can cache its inverse, which is really
## a list containing a function to get/set the value of the matrix and
## get/set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated it retrieves the inverse from the 
## cache. Otherwise, it calculates the inverse and sets the value in the cache

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    ## Return a matrix that is the inverse of 'x'
    i
}
