## makeCacheMatrix accepts a matrix and cache it if it is the first time
## the matrix is input. subsequently if the same matrix is input it will 
## return the inverse of the matrix from its cache.
## function set - sets the value of the matrix
## function get - displays the input matrix
## function setinv - sets the cache with the inverse of the matrix
## functio getinv - displays the inverse of the cache


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
              
    
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinv <- function(inv) m <<- inv
                getinv <- function() m
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)    
}


## This functions inverses a matrix, checks if its in the cache
## If it is returns the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        
        
}
