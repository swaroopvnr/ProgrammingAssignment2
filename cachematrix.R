## There are two funcitons in this file. 
## makeCacheMatrix takes in a matrix
## object and and computes its inverse and caches the inverse.
## cacheSolve takes the matrix input from makeCacheMatrix. If the inverse 
## is already present in the cache for the same matrix, cacheSolve does not
## recompute the inverse and instead gets it from the cache. If the cache is
## empty, cacheSolve computes the inverse.

## makeCacheMatrix is a function that takes in a matrix and returns the list of
## functions of setters and getter to work with the matrix and its inverse (using
## the solve function).
makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(solve) m <<- solve
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve is a function that returns a matrix that is the inverse of 'x',
##  either from the cache or by computing using the solve function.
cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
