## The functions makeCacheMatrix() and cacheSolve() can calculate the inverse
## of a matrix using the solve() function and manage a cached version of this
## inverse for speedy lookups.
##
## Usage:
## x <- makeCacheMatrix(m); constructs object with matrix m
## x$get()                ; retrieves matrix m
## x%get(n)               ; overwrites previous matrix m with matrix n
## cacheSolve(x)          ; obtains (calculation or lookup) and returns the
##                          inverse.

## Object that stores a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    
    get <- function() {x}
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    
    setCachedInverse <- function(s) {cachedInverse <<- s}
    getCachedInverse <- function() {cachedInverse}
    
    list(get = get, set = set,
         getCachedInverse = getCachedInverse,
         setCachedInverse = setCachedInverse)
}

## Calculate inverse of a matrix or returns cached version if available
cacheSolve <- function(x, ...) {
    inv <- x$getCachedInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setCachedInverse(inv)
    inv
}
