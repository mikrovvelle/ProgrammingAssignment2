## makeCacheMatrix takes a single R matrix (non-cached)
## as it's argument and returns a cacheMatrix object enclosing
## the argument given. The object is implemented as an R list.
##
## A cached matrix object is designed to hold two members:
## 1. The matrix itself (x)
## 2. *Maybe* the matrix's inverse (i), if it's been calculated
##
## The underlying data are accessible with their respective
## get and set functions, which are the list members:
## get(), set(), getinverse(), setinverse()
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list (set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## cacheSolve takes advantage of the caching mechanism
## built into a cacheMatrix object. Before starting a potentially
## expensive inverse calculation (solve()), it checks the matrix's
## cache. If there's anything there, cacheSolve returns that instead.
## `i <- solve(x)` is the expensive operation which runs if the
## cache is empty. The result of that is then stored in the cache
## (as `i`) for future use, if cacheSolve gets called again.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
            message("getting cached data")
            return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

## A quick demonstration of the cache's advantage for a large matrix:
##
## > mc<-makeCacheMatrix(matrix(rnorm(3200^2),nrow=3200))
## > system.time(cacheSolve(mc))
##    user  system elapsed
##  10.445   0.056   3.549
## > system.time(cacheSolve(mc))
## getting cached data
##    user  system elapsed
##   0.001   0.000   0.000
