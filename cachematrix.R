## Put comments here that give an overall description of what your
## functions do

## Create a closure for solving (getting the matrix inverse) a matrix and
## caching the inverse.
## The argument matrix must be square and invertable!
## The resulting object can then be passed to cachSolve to retrieve the inverse
## matrix repeatedly without recalculating the inverse.
## Call with:
##   mat = matrix(c(1,.25),c(.25,1)) # matrix must be square and invertable!
##   cmat = makeCacheMatrix(mat)     # create a cacheable matrix object
##
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set =set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Solve (get inverse) a matrix that has been preprocessed with function
## makeCacheMatrix.
##
## Call cacheSolve like this:
##   mat = matrix(c(1,.25),c(.25,1)) # matrix must be square and invertable!
##   cmat = makeCacheMatrix(mat)     # create a cacheable matrix object
## Then, to get the inverse:
##   cacheSolve(cmat)                # get the inverse matrix
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
