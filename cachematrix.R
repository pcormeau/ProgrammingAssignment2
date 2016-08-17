## makeCacheMatrix and cacheSolve are used together to optimize the calculation
## of inverted matrix by caching previously calculated result.
##
## makeCacheMatrix create a special matrix object (list):
##      sm <- makeCacheMatrix(x) : Return a special matrix object initialised with x
##      sm$set(x) : Set storedd matrix to x and clear cached inverted result
##      sm$get() : Return the original stored matrix object
##      sm$setinverse(ix) : Store a calculated inverted matrix. Used by cacheSolve)
##      sm$getinverse() : return the stored inverted matrix. Used by cacheSolve)
##
## cacheSolve return the inverted matrix by calculating it or, if
##    it as already been calculated, the previously calculated result:
##      cacheSolve(sm) : Return the inverted matrix of sm
## 
## Usage :
## 1. Initialise special matrix object by using sm <- makeCacheMatrix(m) where
##    m is a square invertible matrix.
##
## 2. cacheSolve(sm) will return the inverted matrix by calculating it or, if
##    it as already neen calculated, the previously calculated result.


## makeCacheMatrix return a special matrix object (list) to store the matrix
## and its cached inverse is it as already been calculated.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinverse <- function(invertedmatrix) im <<- invertedmatrix
        getinverse <- function() im
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve take a special matrix object created by makeCacheMatrix and
## calculate or return the previsously calculated matrix inverse.

cacheSolve <- function(x, ...) {
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
}
