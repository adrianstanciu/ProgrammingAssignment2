## Functions for calculating the inverse of a matrix, using cache optimizations.

## Creates a special "matrix" function, which is a list of 4 functions
## get - to get the matrix
## set - to set the matrix
## getinverse - to get the inverse of the matrix (we suppose the given matrix is always inversable)
## setinverse - to set the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Performs the optimized calculation of the inverse of the matrix
## The result of the calculation is stored in cache, and returned from cache for subsequent calls
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
