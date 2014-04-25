## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(new_x) {
        x <<- new_x
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(new_inv) inv <<- new_inv
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv
    if (!is.null(inv)) {
        message("returning the cached inverse")
        return(inv)
    }
    temp_x <- x$get()
    inv <- solve(temp_x, ...)
    x$setinv(inv)
    inv
}
