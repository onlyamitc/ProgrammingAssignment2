# makeCacheMatrix: This function creates a special "matrix" object 
#       that can cache its inverse.
# It takes matrix as an input (blank matrix is default input for this!)

# The function makeCacheMatrix creates a special matrix, 
# which is really a list containing a function to
    # set the value of the matrix
    # get the value of the matrix
    # set the inverse of the matrix
    # get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                             
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse<- function(inv_mat) inv <<- inv_mat
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse= setinverse,
         getinverse = getinverse)
    # output is a list with functions as its elements
}


# cacheSolve: This function computes the inverse of the special "matrix" 
#       returned by makeCacheMatrix above. If the inverse has already 
#       been calculated (and the matrix has not changed), then the cachesolve
#       should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {        # x is the output of makeCacheMatrix()
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting inverse matrix from cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
