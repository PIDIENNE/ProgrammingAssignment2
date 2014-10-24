## Put comments here that give an overall description of what your
## functions do

## It creates a list of three funcions
# set -> It assigns to the "global" variable "x" the matrix. Upon definition it's inversion is defined as global and NULL
# get -> It retrieves the matrix from the cache
# getinv -> It retrieves the inverse from the cache (if already computed)

makeCacheMatrix <- function(x = matrix()) {
    # By default the inverse is not calculated when the matrix is made first
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        # Just return x
        x
    }

    getinv <- function() {
        inv
    }
    list(set = set,
         get = get,
         getinv = getinv)
}


## It computes the inverse of the matrix (if not already in the cache)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Getting the inverse matrix, if exists
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Getting the matrix (Matrix To Invert...)
    mti <- x$get()
    # Computing inverse matrix (in this case "b" must be equal to 1)
    inv <- solve(mti)
    # Let the inverted matrix in the upper environment
    x$setinv(inv)
    inv
}
