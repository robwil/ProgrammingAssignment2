## This set of functions enables creating and working with a matrix object
## that can cache its own inverse.

## `makeCacheMatrix` creates the special matrix object with an internal cached inverse
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  # `set` enables user to change the matrix to a new one
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL # values of matrix changed, so bust the cache
  }
  # `get` retrieves the currently stored matrix
  get <- function() x
  # `setInverse` is used by cacheSolve to store the cached inverse in this object
  setInverse <- function(inverse) cachedInverse <<- inverse
  # `getInverse` returns the cached inverse (which may be NULL)
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## `cacheSolve` handles the logic of using the cache if present.
## If cache is not present, it actually calculates the inverse and stores to object.
## As the name makes clear, this internally uses the solve(x, ...) function to get inverse.
cacheSolve <- function(x, ...) {
  # Get cached inverse, if any
  inverse <- x$getInverse()
  # Cache hit, so return cached value immediately
  if(!is.null(inverse)) {
    #message("getting cached data") # message is useful for testing; but commented now.
    return(inverse)
  }
  # Cache miss, so calculate inverse and store to cache for next time.
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}


### Test code follows
m <- matrix(rnorm(1000000), c(1000,1000))
inverse <- solve(m) # takes non-trivial amount of time
m2 <- makeCacheMatrix(m)
inverse <- cacheSolve(m2) # takes non-trivial amount of time
inverse <- cacheSolve(m2) # uses cached result, returns immediately