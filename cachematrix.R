## In sum these functions creates an object(makeCacheMatrix) that is a list of 4 functions,
## and these 4 functions serve as "methods" for the object
## Then cacheSolve uses these methods to cache the inverted matrix

## This function creates a list of functions that sets the matrix, gets the matrix,
## sets the inverted matrix, and gets the inverted matrix respectively

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function retrieves the cached inverted matrix or calculates a new inverted matrix input
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}