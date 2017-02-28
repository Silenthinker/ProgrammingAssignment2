## Caching the Inverse of a Matrix
## using lexical scoping

## creates a special "matrix" object that can cache its inverse
## return a list of functions

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  ## inverse of matrix
    
    ## set new value of matrix and its inverse to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## get matrix
    get <- function() x
    
    ## set inverse of the matrix
    setinv <- function(inverse) inv <<- inverse
    
    ## get inverse of the matrix
    getinv <- function() inv
    
    ## return a list of functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
