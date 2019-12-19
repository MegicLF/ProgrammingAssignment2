## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix: creates a special "matrix" object that can
## cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    matrix(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve: compute the inverse of the special "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated,
## then the cacheSolve retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached matrix")
        return(i)
    }
    data <- x$get()
    i <- inverse(data, ...)
    x$setinverse(i)
    i
}