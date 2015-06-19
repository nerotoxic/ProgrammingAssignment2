## Matrix inversion is usually a costly computation.
## The following two function creates a special matrix and saves
## the inversion of it to cache as to lessen the overhead
## of repeatedly running the iversion if no changes have been
## made to the matrix.

## The makeCacheMatrix function accepts an optional 
## matrix object, if no object is specified a new matrix 
## object is created.

makeCacheMatrix <- function(x = matrix()) {
    ## Set the initial value for the inverted matrix to NULL
    inv <- NULL
    ## Set function that will assign the matrix object and clear
    ## the inverted cache incase the matrix has changed.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## Get function to return the matrix object.
    get <- function() x
    ## SetInvert function to cache the inverted matrix.
    setinvert <- function(invert) inv <<- invert
    ## GetInvert function to return the cached inverted matrix.
    ## Will return NULL if matrix has not been inverted or new instance 
    ## of the matrix has been created.
    getinvert <- function() inv
    ## Set the available functions to a list object in the makeCacheMatrix
    list(set = set, get = get, 
         getinvert = getinvert, 
         setinvert = setinvert)
}


## The cacheSolve function will check if there is a cached version
## of the matrix available. If no cache is available it will create
## the inverse and save it to cache. If a cached value is available
## the cache will be loaded.

cacheSolve <- function(x, ...) {
    ## Get the cache inverted matrix and check if the value exists.
    ## If its not null return the cache value.
    m <- x$getinvert()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## No cache value to return so create a the matrix.
    data <- x$get()
    ## Complete the inversion on the version passing in ... from the parent
    m <- solve(data, ...)
    ## Set the invert cache on the matrix.
    x$setinvert(m)
    ## Display the inverted matrix.
    m
}
