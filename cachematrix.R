## Below functions allow to avoid potentially expensive matrix iverse computing,
## by caching inverse computed once for a given instance of matrix
## example usage:
## m <- matrix(1:4, 2, 2)
## cm <- makeCacheMatrix(m)
## s <- cacheSolve(cm)  # cache is populated in this call
## s2 <- cacheSolve(cm)  # inverse is fetched from cache


# Creates a structure that holds matrix data (x)
# and cached matrix's inverse (inv).
# Returs list of function references that allow
# to get and set matric data and inverse for this matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # inverse cache
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(get=get, set=set, get_inverse=get_inverse, set_inverse=set_inverse)
}


# Returns inverse for a matrix.
# Gets an inverse from cacfe if exists. Otherwise populates cache with computed matrix inverse.
# Works only with objects returned from makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # try to get cached inverse
    inv <- x$get_inverse()
    
    if(!is.null(inv)){
        # if there is a cached inverse, use it
        return(inv)         
    }
    
    # if inverse is not yet cached, compute
    matrix_data <- x$get()
    inv <- solve(matrix_data, ...)
    
    # populate cache
    x$set_inverse(inv)
    inv
}
