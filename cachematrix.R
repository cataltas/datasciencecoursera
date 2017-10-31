## Both these functions allow the creation of a cache for matrix inverse computations so an inverse can be called multiple times but computed only once.

## This function stores the cache for the inverse of the matrix in a list which can be re-accessed when needed.

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


## This function will return the cache value of the inverse if it has already been computed and stored earlier. If not, it will compute the inverse of the matrix and store it in the cache.

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

