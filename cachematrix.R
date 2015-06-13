## The purpose of this couple of functions is to implement "matrices"
## that can potentially carry along themselvas their own inverses
## and a "solve" function which takes this kind of "matrices" as argument
## and return the inverses with the minimum of calculation.

## makeCacheMatrix implements a "matrix" by creating a list of functions
## which can set and get the value of the (usual) matrix and its inverse.

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
## In the latter case, cacheSolve caches the inverse in the "matrix".

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
