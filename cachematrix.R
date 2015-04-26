## This R source contain two functions, makeCacheMatriz that represent an special matrix that can hold a reference of a matrix and their computed 
## matrix inverse, and a function cacheSolve, that returns the inverse of that special matrix, handling if the inverse matrix should be computed 
## or returned from the cache.

## How-to test:
## > m <- makeCacheMatrix(x = matrix(1:4, 2, 2))
## > cacheSolve(m) # should compute, cache and return the inverse
## > cacheSolve(m) # should return inverse from cache
## > m$set(matrix(1:4, 2, 2)) # should do nothing, matrices are identical
## > cacheSolve(m) # should return inverse from cache
## > m$set(matrix(4:7, 2, 2)) # should set new matrix and clear the cache
## > cacheSolve(m) # should compute, cache and return the inverse
## > cacheSolve(m) # should return inverse from cache

## Author: Rafael Tartalia (rafael.tartalia@gmail.com)

## This function receive a matrix and compare to the cached matrix to verify if the received matrix is equal to the cached matrix.
## If the cached matrix is not equal to the received matrix, then the received matrix is cached. This function contain methods to
## cache the computed inverse matrix too.
makeCacheMatrix <- function(x = matrix()) {
        # the inverse matrix reference
        i <- NULL
        # returns the matrix
        get <- function() x
        # sets the cached matrix to the new value and clear de inverse matrix reference,
        # is matrices are not equal (prevents new inverse computation)
        set <- function(y) {
                # change matrix and clear cache only if the matrices 
                # are not equal
                if (!identical(y, x)) {
                        message("matrices are not identical")
                        x <<- y
                        i <<- NULL
                }
        }
        # sets de inverse matrix
        setinv <- function(inv) {
                i <<- inv
        }
        # returns the inverse matrix
        getinv <- function() i
        # set internal list to expose functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function receive a special matrix (an R function object), that can hold reference to the computed inverse matrix (cache the inverse matrix).
## Return the inverse matrix from cache if that matrix is already cached, else compute inverse matrix, store on cache, and return that matrix.
cacheSolve <- function(x, ...) {
        # cached inverse
        c <- x$getinv()
        if (!is.null(c)) {
                message("getting inverse from cache")
                return(c)
        }
        message("computing inverse matrix")
        c <- solve(x$get(), ...)
        x$setinv(c)
        c        
}
