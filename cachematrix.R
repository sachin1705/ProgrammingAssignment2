##  Below are two functions that are used to create a
##  special object that stores a matrix and caches its inverse.

## The first function, `makeCacheMatrix` creates a special "vector", which is
## really a list containing a function to
##  1.  set the value of the vector
##  2.  get the value of the vector
##  3.  set the value of the mean
##  4.  get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    inverseOfMatrix <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(val) inverseOfMatrix <<- val
    getmatrix <- function() inverseOfMatrix
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

##  The following function calculates the inverse of the cached marix that is
##  created with the above function. However, it first checks to see if the
##  inverse has already been calculated. If so, it get's the inverse from the
##  cache and skips the computation. Otherwise, it calculates the inverse of
##  the matrix and sets the value of the inverse in the cache via the `setmatrix`
##  function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseOfMatrix <- x$getmatrix()
    if(!is.null(inverseOfMatrix)) {
        message("getting cached data")
        return(inverseOfMatrix)
    }
    data <- x$get()
    inverseOfMatrix <- solve(data)
    x$setmatrix(inverseOfMatrix)
    inverseOfMatrix
}
