## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(get=get, set=set, get_inverse=get_inverse, set_inverse=set_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Try to get cached inverse
    inv <- x$get_inverse()
    
    if(!is.null(inv)){
        # if there is a cached inverse, use it
        return(inv)         
    }
    
    # if inverse is not yet cached, compute
    inv <- solve(x$get())
    # populate cache
    x$set_inverse(inv)
    inv
}
