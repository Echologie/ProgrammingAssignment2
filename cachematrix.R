## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix implements a "matrix" able to carry along itsef its own inverse
## by creating a list of functions which can set and get the value of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve return the inverse of the "matrix" created above,
## using the cached value if available, or calculates it if not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached inverse matrix")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
