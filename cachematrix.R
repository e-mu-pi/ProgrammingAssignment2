## Define a cached matrix class that can cache 
## the inverse.

## Construct a cached matrix from a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
     x <<- y
     inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list( set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
}


## Compute inverse of a cached matrix. If already 
## computed, cached version is used.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if( !is.null(inv) ){
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
