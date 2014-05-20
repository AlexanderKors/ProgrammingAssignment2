## These two functions are to cache the inverse of a matrix "x" and later use it 
## witout having to do the time consuming computation again.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }}
        get <- function() x
        setsolve <- function(solve) m <<- solve()
        getsolve <- function() m
        list(set = set, get = get,
             setInv = setsolve,
             getInv = getsolve)
}


## This function computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m) & (m!=x$get())) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}



