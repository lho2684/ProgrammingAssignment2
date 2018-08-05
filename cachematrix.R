## Put comments here that give an overall description of what your
## functions do
## Response: the functions cache the inverse of a matrix

## Write a short comment describing this function
## Response: Creates a matrix object (x) and calculates the inverse of the object to be cached
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- matrix(data,...)
        x$setinverse(m)
        m
}
