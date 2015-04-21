## A pair of functions to implement a cacheable matrix inversion 

## Construct a matrix, and return a list of four functions that respectively
## set & get the matrix, set the cached inverse, and return the cached inverse

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


## Return the inverse of matrix x, using the cached version if available

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'

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
