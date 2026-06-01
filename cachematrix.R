## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix: creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x

    setinverse <- function(inverse) {
        inv <<- inverse
    }

    getinverse <- function() inv

    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## cacheSolve: computes the inverse of the special matrix
## and retrieves it from the cache if already available

cacheSolve <- function(x, ...) {

    inv <- x$getinverse()

    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)

    inv
}

A <- matrix(c(1, 2, 3, 4), 2, 2)

B <- makeCacheMatrix(A)

cacheSolve(B)

cacheSolve(B)

