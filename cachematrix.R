## The purpose of this couple of functions is to implement "matrices"
## that can potentially carry along themselves their own inverses
## and a "solve" function which takes this kind of "matrices" as argument
## and return the inverses with the minimum of calculation.

## makeCacheMatrix implements a "matrix" by creating a list of four functions
## which can set and get the value of the (usual) matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) { # x should be a regular matrix

  inverse <- NULL # Setting inverse as NULL allows testing whether
                  # the inverse as already been cached or not.

  set <- function(y) { # typing cachedMatrixName$set(someRegularMatrix)
    x <<- y            # will replace the cached matrix by the new one specified
    inverse <<- NULL } # and therefore reinitialize the inverse.

  get <- function() x # typing cachedMatrixName$get() just return the matrix.

  setinverse <- function(inv) inverse <<- inv
  # typing cachedMatrixName$setInverse(inverseOfTheMAtrixStoredInGet)
  # will cache the inverse of the matrix. Inverse must be properly given by the argument.

  getinverse <- function() inverse
  # typing cachedMatrixName$getInverse() just return the inverse of the matrix
  # provided that it has been set by setInverse (otherwise it would return NULL).

  list( set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse ) }


## cacheSolve return the inverse of a "matrix" of the type created above,
## using the cached value if available, or calculates it if not.
## In the latter case, cacheSolve caches the inverse in the "matrix".

cacheSolve <- function(x, ...) { # x should be of the type output by makeCacheMatrix

  inverse <- x$getinverse()                  # This block will test if the inverse
  if(!is.null(inverse)) {                    # has already been cached and quit
    message("getting cached inverse matrix") # the function by returning the
    return(inverse) }                        # cached inverse if so.

  theMatrix <- x$get()             # In the case where the inverse has not been cached already,
  inverse <- solve(theMatrix, ...) # this block will get the value of the matrix stored in x,
  x$setinverse(inverse)            # calculate the inverse, store it in x and
  inverse }                        # finally return the inverse of the matrix.
