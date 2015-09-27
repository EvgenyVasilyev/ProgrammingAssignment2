## Put comments here that give an overall description of what your
## functions do

## This function allows create special matrix with ability to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function (y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function (inverse) i <<- inverse
    getinverse <- function () i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function allows to get data from cache of special matrix that was created via
## function makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message ("This is cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
