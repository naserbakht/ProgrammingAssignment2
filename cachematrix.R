## makeCacheMatrix: function to create a generic cacher object for the purpose of caching inverse matrices

makeCacheMatrix <- function(x = matrix()) {
  ## generic cache
  cache <- NULL

  ## to set a new matrix and invalidate the cache
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }

  ## to get the initial matrix
  get <- function() x

  ## for setting the cache
  setCache <- function(data) cache <<- data

  ## for getting the cache value
  getCache <- function() cache

  ## returning a named list with four functions/properties for setting and getting matrix values
  ## as well as working with the cache
  list(set = set, get = get, setCache = setCache, getCache = getCache)
}

## cacheSolve: this function accepts an object of makeCacheMatrix to compute the original matrix' inverse
## if the inverse has already been calculated, the function reads and returns from the cache

cacheSolve <- function(x, ...) {
  ## get cache if any
  cache <- x$getCache()

  ## check if we already have a cached inverse matrix
  if(!is.null(cache)) {
    message("getting cached data")

    return(cache)
  }

  ## get original matrix
  data <- x$get()

  ## calculate inversed matrix
  inversedMatrix <- solve(data, ...)
  
  ## cache the inversed matrix
  x$setCache(inversedMatrix)
  
  inversedMatrix
}
