## Programming Assignment 2

## makeCacheMatrix stores the input matrix 'x' 
## and inverse (initialized as NULL) of that matrix 'm'

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { 
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve 
        getsolve <- function() m 
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve figures out if the inverse of input matrix 'x'  has been cached. 
## If not, it then calculates the inverse of input matrix 'x'
## assigns it to 'm', and returns the value 'm'

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
