## Caching of the inverse of a matrix
## function makeCacheMatrix: create a special "matrix", a list of functions to
##  - set and get the value of the matrix
##  - set and get the inverse of the matrix
## function cacheSolve: calculate and return he inverse of the special "matrix" 
## created with the above function, checking to see if the inverse has already
## been calculated

## create a special "matrix" of a given input matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## solve and "cache" the inverse of a given input special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
