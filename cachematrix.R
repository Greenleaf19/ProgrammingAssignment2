## In combination these two functions create a matrix object and
## calculates its inverse if not already stored in the cache.

## makeCacheMatrix creates a Matrix object that can cache its Inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheinverse returns the inverse of a Matrix object x either by
## a) retrieving the inverse from the cache, if the inverse exists or
## b) calculating the inverse from the data, in this case it stores
## the result in the cache for later use.

## (!) Attention: This function assumes that the matrix is square

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
