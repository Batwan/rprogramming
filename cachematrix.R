## This function will follow the following steps:
##     1. Initialize the inverse matrix by NULL because we didn't calculate it.
##     2. The 'set' function will set the target matrix x, and reset the inverse matrix by NULL.
##     3. The 'get' function will return the matrix x.
##     4. The 'setInversion' will store the calculated inverse matrix.
##     5. The 'getInversion' will return the stored inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL   # initialize the inverse matrix
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInversion <- function(inversion) i <<- inversion
    getInversion <- function() i
    
    # Return the list
    list(set = set, get = get
         , setInversion = setInversion
         , getInversion = getInversion)
}

## First, the function will get the inverse matrix from the cached object.
## Second, if the inverse matrix is not NULL, we can just return the inverse matrix.
## Third, if the inverse matrix is NULL, we need to calculate it with solve function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	i <- x$getInversion()   # get the inverse matrix by calling the function in the list
    
    if (!is.null(i)) {
        # The inverse matrix has been calculated
        message("Getting cached matrix inversion")
        return(i)
    }
    
    # The inverse matrix doesn't exist, we need to calculate it and return it
    data <- x$get()
    i <- solve(data, ...)
    x$setInversion(i)
    i
}
