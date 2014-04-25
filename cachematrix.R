## A couple helper functions that allow for caching the inverse of the matrix
## Usage:
##      Initialization...
##          a <- makeCacheMatrix(matrix(1:16, 4))
##      To see your matrix...
##          a$get()
##      To get the inverse of this matrix...
##          a$getinv()
##          Note: The first this is called, you will take the hit to 
##          do the actual calculation. Subsequent calls used the cached value.
##      To assign another matrix...
##          a$set(matrix(1:4, 2))



## Use this function to initialize your matrix and cached matrix.
## This is what persists these values past the actual call to "makeCacheMatrix"
makeCacheMatrix <- function(x = matrix()) {
    matrix_inverse <- NULL
    
    # We need to blow away our cached value anytime we change our matrix.
    set <- function(new_x) {
        x <<- new_x
        matrix_inverse <<- NULL
    }
    get <- function() x
    setinv <- function(new_matrix_inverse) matrix_inverse <<- new_matrix_inverse
    getinv <- function() matrix_inverse
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function takes any instance of makeCacheMatrix and uses its
## cached value if it exists. Otherwise, it will calculate the inverse
## of the matrix and update that instance's matrix_inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrix_inverse <- x$getinv()
    if (!is.null(matrix_inverse)) {
        message("returning the cached inverse")
        return(matrix_inverse)
    }
    # The inverse isn't yet calculated. Do the calculation and persist it.
    temp_x <- x$get()
    matrix_inverse <- solve(temp_x, ...)
    x$setinv(matrix_inverse)
    matrix_inverse
}
