## These functions are identical in form & function as the demo functions
## for assignment #2, "Caching the Mean of a Vector", except they have been
## modified to compute the inverse of a matrix

## Maintain a precomputed (cached) matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
  )
}


## find the inverse of a matrix if no cached solution exists

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("returning cached matrix...")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}