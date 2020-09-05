## Code meant to cache the inverse of an invertible matrix. When the matrix is (re)set, Cache is also refreshed 


##   makeCacheMatrix creates a special matrix which caches itself and returns a liet of functions
    ##   set the value of the matrix
    ##   get the value of the matrix
    ##   set the the inverse of the matrix and store in cache
    ##   get the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse)  m <<- inverse
  
  getinverse <- function() m
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## function to retrieve the inversed matrix from cache,
## if cache is empty, then it sets the inverse again
## returns the inversed matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinverse(m)
  
  return(m)
}
