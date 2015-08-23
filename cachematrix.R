## The first function creates a special "matrix" object that can cache 
## its inverse, and the second return the inverse of the matrix.


## create a special "matrix" which contains four functions and the matrix
##  set: set the matrix
##  get: return the matrix
##  setinverse: set the inverse of the matrix
##  getinverse: return the inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        #reading caching
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    #Computing the inverse of a square matrix
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
