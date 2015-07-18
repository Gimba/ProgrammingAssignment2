## Create a special object that stores a numeric matrix and cache its inverse matrix

## some code for testing purpose: 
## > m <- replicate(4, sample(4))
## > n <- makeCacheMatrix(m)
## > cacheSolve(n)
## > n$get() %*% n$get_inv()
## the result should be a matrix where the main diagonal contains '1' and '0' elsewhere
## !!because of rounding errors the '0' are very often extremly small numbers like "-2.220446e-16"!!


## Initialize list containing setters and getters for a matrix x plus its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inv <- function(inv) m <<- inv
  get_inv <- function() m
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## Compute and cache the inverse matrix of x

cacheSolve <- function(x, ...) {
  
  ## If inverse already computed return it
  m <- x$get_inv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Compute and return the inverse of x
  data <- x$get()
  m <- solve(data, ...)
  x$set_inv(m)
  m
}
