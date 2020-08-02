## the function "makeCacheMatrix" creates a list containing a function to set and get the values of the matrix and the inverse
## the function "cacheSolve" calculates the Inverse of the list created with the "makeCacheMatrix". 

## set and get the value of the Matrix and set and get the value of the Inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {x}
  setInverse <- function(Inverse) {
    m <<- Inverse
  }
  getInverse <- function() {
    m
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## checks if the Inverse has already been calculated If so, it gets the Inverse from the cache, Otherwise it calculates the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
