## Create a special matrix object that can cache its inverse
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


## Compute or retrieve the cached inverse
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

##output##
> cacheSolve(B)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
