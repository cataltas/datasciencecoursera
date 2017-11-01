## Both functions save inverses of matrices through the use of a cache so they can be re-accessed without needing to re-calculate them each time they are called

## Creates the cache for the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  	set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Create a function that tests if the inverse wanted is already saved in a cache and return it or compute the inverse, save it and print it.

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
