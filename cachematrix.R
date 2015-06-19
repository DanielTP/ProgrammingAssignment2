## The following functions are used to create a special list that stores a 
## matrix and caches it's inverse matrix

## This function is trying to set a matrix , get a matrix, set a matrix which 
## is an inverse matrix, and get a matrix which is an inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function is trying to check if an inverse matrix exists , if so, return
## that cached inverse matrix, if not, calculate one and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
}