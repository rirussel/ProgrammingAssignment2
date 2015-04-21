## A pair of functions to implement matrix inversion with a cache for
## R Programming assignment 2

## Construct a matrix, and return a list of four functions that respectively
## set & get the matrix, cache the inverse, and return the cached inverse
## > makeCacheMatrix()  - initialise with empty matrix
## > makeCacheMatrix(x) - initialise with matrix x

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) minv <<- inverse
    getinverse <- function() minv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Return the inverse of matrix x, using the cached version if available.
## Print a message to show when the cached version is being used.
## If cached version not available, compute the inverse and store it in the cache.

cacheSolve <- function(x) {
    minv <- x$getinverse()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data)
    x$setinverse(minv)
    minv
}
