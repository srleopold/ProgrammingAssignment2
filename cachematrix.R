## These functions implement a caching matrix inverter.
## makeCacheMatrix creates a special "matrix" object that can hold a matrix
## and a cached copy of its inverse.
## cacheSolve retrieves the cached inverse or performs the inverse calculation 
## depending on the contents of the special "matrix" object and updates it
## accordingly.

## makeCacheMatrix creates a list object that can hold a matrix and a cached
## copy of its inverse. 
## Note that the only way to modify the stored matrix is using the set function 
## (R matrices are immutable). As set assigns NULL to the
## cachedInverse variable, it clears the cached value (if it exists), 
## forcing the cacheSolve function to recalculate the inverse. If the user
## tries to assign the same value to the matrix x, it is ignored to prevent
## clearing the cache unnecessarily.

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(newMatrix) {
        if(!identical(newMatrix,x)){
            x <<- newMatrix
            cachedInverse <<- NULL
        }
    }
    get <- function() x
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve receives a special "matrix" object (it is assumed that the matrix
## within this object is invertible, as stated in the assignment instructions).
## It attempts to retrieve the cached inverse. If it is NULL, the inverse is 
## calculated and stored. Otherwise, the cached value is returned and a message
## is printed to inform the user.
## As the cached value is deleted if the matrix is modified, cacheSolve will always 
## calculate the inverse of a modified matrix, even if a previously 
## cached matrix existed before calling set.

cacheSolve <- function(x, ...) {
    matrixInverse <- x$getInverse()
    if(!is.null(matrixInverse)) {
        message("Retrieved cached matrix inverse")
        return(matrixInverse)
    }
    matrix <- x$get()
    matrixInverse <- solve(matrix, ...)
    x$setInverse(matrixInverse)
    matrixInverse
}
