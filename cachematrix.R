## R Programming, Assignment 2
## Functions for creating a matrix that caches its inverse
## and for retrieveing the cached inverse or calculating it.



## creates a 'special' matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    cacheInv <- NULL
    set <- function(y) {
        # test that the input is a matrix
        if (!is.matrix(y)) stop("Input must be a matrix!")
        # test that the input matrix is square
        if (dim(y)[1]!=dim(y)[2]) stop("Input matrix must be square!")
        
        
        # if the matrix is being set to a different matrix than what's
        # cached, cache it and reset the cache Inverse
        cacheMat <<- y
        cacheInv <<- NULL
    }
    set(x)
    get <- function() cacheMat
    setinv <- function(inv) cacheInv <<- inv
    getinv <- function() cacheInv
    list (set = set, get=get, 
          setinv = setinv, getinv = getinv)
}


## computes the inverse of a special matrix created with makeCacheMatrix
## if the cached value exists, return it. otherwise calculate the inverse and
## return it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("returning cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv = solve(mat,...)
    x$setinv(inv)
    inv
}
