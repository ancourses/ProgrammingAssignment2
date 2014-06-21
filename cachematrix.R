## PThese functions manage matrix  with cacheable inverse

## Create matrix with cacheable inverse
## Accepts a raw matrix 'mat' as argument, return its cached version
makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL # By default cached inverse is empty

  # We can set new matrix
  set <- function(y) {
    mat <<- y
    inv <<- NULL # Reset cached inverse when changing matrix
  }

  # We can get current matrix value
  get <- function() mat

  # We can get and set cache inverse value
  setinv <- function(i) inv <<- i
  getinv <- function() inv

  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Get inverse of matrix caching its value
## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  # Get current inverse value
  inv <- x$getinv()

  if(!is.null(inv)) { # If inverse value already cached
    message("getting cached data")
    return(inv) # Return it
  }

  data <- x$get() # Get matrix data
  inv <- solve(data, ...) # Compute inverse value
  x$setinv(inv) # Set cached value
  inv
}
