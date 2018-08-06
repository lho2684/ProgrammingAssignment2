## Put comments here that give an overall description of what your
## functions do
## Response: the functions cache the inverse of a matrix

## Write a short comment describing this function
## Response: Creates a matrix object (x) with user input, and calculates the inverse 
## of the object that can be cached.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## Response: Returns a matrix that is the inverse of 'x', if the inverse has been calculated,
## then the function retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
