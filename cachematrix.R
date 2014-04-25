## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    matrix_inverse <- NULL
    set <- function(new_x) {
        x <<- new_x
        matrix_inverse <<- NULL
    }
    get <- function() x
    setinv <- function(new_matrix_inverse) matrix_inverse <<- new_matrix_inverse
    getinv <- function() matrix_inverse
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrix_inverse <- x$getinv()
    print(matrix_inverse)
    if (!is.null(matrix_inverse)) {
        message("returning the cached inverse")
        return(matrix_inverse)
    }
    temp_x <- x$get()
    matrix_inverse <- solve(temp_x, ...)
    x$setinv(matrix_inverse)
    matrix_inverse
}
