## Below functions allow to avoid potentially expensive matrix iverse computing,
## by caching inverse computed for a given instance of a matrix.
## example usage:
## m <- matrix(1:4, 2, 2)
## cm <- makeCacheMatrix(m)
## s <- cacheSolve(cm)  # cache is populated in this call
## s2 <- cacheSolve(cm)  # inverse is fetched from cache


# Creates a structure that holds matrix data (x)
# and cached matrix's inverse (inv).
# Returs list of function references that allow
# to get and set a matrix data and an inverse for this matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # inverse cache
    set <- function(y) {
      x <<- y
      # reset cached inverse
      inv <<- NULL  
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    
    # return list of operations
    list(get=get, 
         set=set, 
         get_inverse=get_inverse, 
         set_inverse=set_inverse)
}


# Returns an inverse of a matrix.
# Gets an inverse from cache if exists,
# otherwise populates cache with computed matrix inverse before returning this value.
# Works only with objects returned from makeCacheMatrix.
# Provided matrix is assumed to be invertible.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # try to get cached inverse
    inv <- x$get_inverse()
    
    if(!is.null(inv)){
        # if there is a cached inverse, return it
        return(inv)         
    }
    
    # if inverse is not yet cached, compute
    matrix_data <- x$get()
    inv <- solve(matrix_data, ...)
    
    # populate cache
    x$set_inverse(inv)
    
    # return computed inverse
    inv
}
